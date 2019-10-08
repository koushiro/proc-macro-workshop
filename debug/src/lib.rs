extern crate proc_macro;
#[macro_use]
extern crate quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // Construct builder pattern for structure.
    let output = impl_debug_for_struct(input);

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(output)
}

fn impl_debug_for_struct(ast: syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = ast.ident;

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("#[derive(CustomDebug)] is only defined for structs"),
    };

    match fields {
        syn::Fields::Named(_) => {}
        _ => panic!("#[derive(CustomDebug)] is not defined for Unnamed structs and Uint structs"),
    }

    let phantom_data_types = fields
        .iter()
        .filter_map(|field| generic_inner_type(field, "PhantomData"))
        .collect::<Vec<_>>();
    let required_types = ast.generics.type_params()
        .filter(|type_param| {
            for phantom_type in &phantom_data_types {
                if let syn::Type::Path(syn::TypePath { path, .. }) = phantom_type {
                    if path.is_ident(type_param.ident.to_string().as_str()) {
                        return false;
                    }
                }
            }
            type_param.bounds.is_empty()
        })
        .collect::<Vec<_>>();
    let generics = add_trait_bounds(ast.generics.clone(),  required_types);

    let impl_debug_trait_for_struct = impl_debug_trait_for_struct(&name, &fields, &generics);

    quote! {
        #impl_debug_trait_for_struct
    }
}

fn impl_debug_trait_for_struct(
    name: &syn::Ident,
    fields: &syn::Fields,
    generics: &syn::Generics,
) -> proc_macro2::TokenStream {
    let name_str = name.to_string();
    let impl_for_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let name_str = name.as_ref().expect("Field has name").to_string();
        match field_attr_debug_fmt(field) {
            Ok(Some(lit)) => quote! { .field(#name_str, &std::format_args!(#lit, self.#name)) },
            Ok(None) => quote! { .field(#name_str, &self.#name) },
            Err(err) => err.to_compile_error(),
        }
    });
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_str)
                    #( #impl_for_fields )*
                    .finish()
            }
        }
    }
}

fn generic_inner_type<'a>(field: &'a syn::Field, outer_ty: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::AngleBracketed(args),
        }) = segments.first()
        {
            if ident == outer_ty {
                if let Some(syn::GenericArgument::Type(ty)) = args.args.first() {
                    return Some(ty);
                }
            }
        }
    }
    None
}

fn find_field_attr_debug(field: &syn::Field) -> Option<&syn::Attribute> {
    for attr in field.attrs.iter() {
        if attr.path.is_ident("debug") {
            return Some(attr);
        }
    }
    None
}

fn field_attr_debug_fmt(field: &syn::Field) -> syn::Result<Option<syn::LitStr>> {
    match find_field_attr_debug(field) {
        Some(attr) => {
            let meta = attr.parse_meta()?;
            let debug_attr_err = debug_attr_error(&meta);
            if let syn::Meta::NameValue(syn::MetaNameValue {
                path,
                lit: syn::Lit::Str(lit),
                ..
            }) = meta
            {
                if path.is_ident("debug") {
                    Ok(Some(lit))
                } else {
                    Err(debug_attr_err)
                }
            } else {
                Err(debug_attr_err)
            }
        }
        None => Ok(None),
    }
}

fn debug_attr_error<M: quote::ToTokens>(meta: M) -> syn::Error {
    syn::Error::new_spanned(meta, "expected `debug = \"...\"`")
}

// Add a bound `T: Debug` to every type parameter of `required_types`.
fn add_trait_bounds(mut generics: syn::Generics, required_types: Vec<&syn::TypeParam>) -> syn::Generics {
    for type_param in generics.type_params_mut() {
        for required_type in &required_types {
            if required_type.ident == type_param.ident {
                type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}

fn add_where_clauses(mut generics: syn::Generics, types: Vec<&syn::Type>) -> syn::Generics {
    let where_clause = generics.make_where_clause();
    for ty in types {
        let predicate = syn::parse_quote!(#ty: std::fmt::Debug);
        where_clause.predicates.push(predicate);
    }
    generics
}
