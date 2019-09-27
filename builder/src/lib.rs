extern crate proc_macro;
#[macro_use]
extern crate quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // Construct builder pattern for structure.
    let output = builder_for_struct(input);

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(output)
}

fn builder_for_struct(ast: syn::DeriveInput) -> proc_macro2::TokenStream {
    let ident = ast.ident;
    let builder = format_ident!("{}Builder", ident);

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("#[derive(Builder)] is only defined for structs"),
    };

    match fields {
        syn::Fields::Named(_) => {}
        _ => panic!("#[derive(Builder)] is not defined for Unnamed structs and Uint structs"),
    }

    let builder_default_fields = builder_default_fields(&fields);
    let builder_fields = builder_fields(&fields);
    let builder_setters = builder_setters(&fields);
    let build_set_fields = build_set_fields(&fields);

    quote! {
        impl #ident {
            pub fn builder() -> #builder {
                #builder {
                    #( #builder_default_fields )*
                }
            }
        }

        pub struct #builder {
            #( #builder_fields )*
        }

        impl #builder {
            #( #builder_setters )*

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #( #build_set_fields )*
                })
            }
        }
    }
}

fn builder_default_fields(
    fields: &syn::Fields,
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().map(|field| {
        let ident = &field.ident;
        if field_is_vec(field) {
            quote! { #ident: vec![], }
        } else {
            quote! { #ident: None, }
        }
    })
}

fn builder_fields(fields: &syn::Fields) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if field_is_option(field) || field_is_vec(field) {
            quote! { #ident: #ty, }
        } else {
            quote! { #ident: Option<#ty>, }
        }
    })
}

fn builder_setters(fields: &syn::Fields) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if let Some(vec_inner_ty) = generic_inner_type(field, "Vec") {
            if let Some(fn_name) = field_builder_attr(field) {
                let fn_name = format_ident!("{}", fn_name);
                quote! {
                    pub fn #fn_name(&mut self, #ident: #vec_inner_ty) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                }
            } else {
                quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }
                }
            }
        } else if let Some(option_inner_ty) = generic_inner_type(field, "Option") {
            quote! {
                pub fn #ident(&mut self, #ident: #option_inner_ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    })
}

fn build_set_fields(fields: &syn::Fields) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            if field_is_option(field) {
                quote! { #ident: self.#ident.take(), }
            } else if field_is_vec(field) {
                quote! { #ident: std::mem::replace(&mut self.#ident, vec![]), }
            } else {
                quote! { #ident: self.#ident.take().ok_or(concat!("Field [", stringify!(#ident), "] is missing"))?, }
            }
        })
}

fn field_is_option(field: &syn::Field) -> bool {
    field_is(field, "Option")
}

fn field_is_vec(field: &syn::Field) -> bool {
    field_is(field, "Vec")
}

fn field_is(field: &syn::Field, ty: &str) -> bool {
    generic_inner_type(field, ty).is_some()
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

fn field_builder_attr(field: &syn::Field) -> Option<String> {
    for attr in field.attrs.iter() {
        if attr.path.is_ident("builder") {
            if let Ok(syn::Meta::List(meta_list)) = attr.parse_meta() {
                for meta in meta_list.nested {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = meta {
                        if nv.path.is_ident("each") {
                            if let syn::Lit::Str(fn_name) = nv.lit {
                                let fn_name = fn_name.value();
                                if !fn_name.is_empty() {
                                    return Some(fn_name);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}
