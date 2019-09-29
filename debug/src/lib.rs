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

    let generics = add_trait_bounds(ast.generics);

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("#[derive(CustomDebug)] is only defined for structs"),
    };

    match fields {
        syn::Fields::Named(_) => {}
        _ => panic!("#[derive(CustomDebug)] is not defined for Unnamed structs and Uint structs"),
    }

    let impl_debug_trait_for_struct = impl_debug_trait_for_struct(&name, &generics, &fields);

    quote! {
        #impl_debug_trait_for_struct
    }
}

fn impl_debug_trait_for_struct(
    name: &syn::Ident,
    generics: &syn::Generics,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let name_str = name.to_string();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_for_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let name_str = name.as_ref().expect("Field has name").to_string();
        match field_attr_debug_fmt(field) {
            Ok(Some(lit)) => quote! { .field(#name_str, &std::format_args!(#lit, self.#name)) },
            Ok(None) => quote! { .field(#name_str, &self.#name) },
            Err(err) => err.to_compile_error(),
        }
    });

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
        Some(attr) => match attr.parse_meta()? {
            syn::Meta::NameValue(syn::MetaNameValue {
                path,
                lit: syn::Lit::Str(lit),
                ..
            }) => {
                if path.is_ident("debug") {
                    Ok(Some(lit))
                } else {
                    Err(unrecognized_attr_error(attr.parse_meta()?))
                }
            }
            other => Err(unrecognized_attr_error(other)),
        },
        None => Ok(None),
    }
}

fn unrecognized_attr_error<M: quote::ToTokens>(meta: M) -> syn::Error {
    syn::Error::new_spanned(meta, "expected `debug = \"...\"`")
}

// Add a bound `T: Debug` to every type parameter T.
fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }
    generics
}
