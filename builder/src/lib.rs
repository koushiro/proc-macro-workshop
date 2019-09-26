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

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("#[derive(Builder)] is only defined for structs"),
    };

    match fields {
        syn::Fields::Named(_) => {},
        _ => panic!("#[derive(Builder)] is not defined for Unnamed structs and Uint structs"),
    }

    let builder_default_fields = builder_default_fields(&fields);
    let builder_fields = builder_fields(&fields);
    let builder_setters = builder_setters(&fields);
    let build_set_fields = build_set_fields(&fields);

    quote! {
        impl #impl_generics #ident #ty_generics #where_clause {
            pub fn builder() -> #builder {
                #builder {
                    #( #builder_default_fields )*
                }
            }
        }

        pub struct #builder #ty_generics {
            #( #builder_fields )*
        }

        impl #impl_generics #builder #ty_generics #where_clause {
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
            quote! { #ident: None, }
    })
}

fn builder_fields(fields: &syn::Fields) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if field_is(field, "Option") {
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
        if field_is(field, "Option") {
            let inner_ty = generic_inner_type(field);
            quote! {
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
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
            if field_is(field, "Option") {
                quote! { #ident: self.#ident.take(), }
            } else {
                quote! { #ident: self.#ident.take().ok_or(concat!("Field [", stringify!(#ident), "] is missing"))?, }
            }
        })
}

fn field_is(field: &syn::Field, ty: &str) -> bool {
    if let syn::Type::Path(ref path) = field.ty {
        path.path
            .segments
            .first()
            .map(|seg| seg.ident == ty)
            .unwrap_or(false)
    } else {
        unimplemented!()
    }
}

fn generic_inner_type(field: &syn::Field) -> &syn::Type {
    match field.ty {
        syn::Type::Path(ref path) => path
            .path
            .segments
            .first()
            .map(|seg| match seg.arguments {
                syn::PathArguments::AngleBracketed(ref args) => match args.args.first().unwrap() {
                    syn::GenericArgument::Type(ty) => ty,
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            })
            .unwrap(),
        _ => unimplemented!(),
    }
}
