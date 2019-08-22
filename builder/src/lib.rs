extern crate proc_macro;
#[macro_use]
extern crate quote;

use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as syn::DeriveInput);

    // Construct builder pattern for structure.
    let output = builder_for_struct(input);

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(output)
}

fn builder_for_struct(ast: syn::DeriveInput) -> proc_macro2::TokenStream {
    let vis = ast.vis;
    let name = ast.ident;
    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => unreachable!(),
    };

    let builder_name = format_ident!("{}Builder", name);

    let empty_fields = fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote!{ #ident: None }
        });

    let construct_fields = fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            if is_optional_type(ty) {
                quote!{ #ident: #ty }
            } else {
                quote!{ #ident: Option<#ty> }
            }
        });

    let set_fields = fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            if is_optional_type(ty) {
                quote! { #ident: self.#ident.clone() }
            } else {
                quote! { #ident: self.#ident.clone().ok_or(concat!("Field [", stringify!(#ident), "] is missing"))? }
            }
        });

    let setters = fields.iter().map(|field| {
        let vis = &field.vis;
        let ident = &field.ident;
        let ty = &field.ty;
        if let Some(inner_ty) = inner_type("Option", ty) {
            quote! {
                #vis fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                #vis fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    });

    quote! {
        impl #name {
            #vis fn builder() -> #builder_name {
                #builder_name {
                    #(#empty_fields,)*
                }
            }
        }

        #vis struct #builder_name {
            #(#construct_fields,)*
        }

        impl #builder_name {
            #(#setters)*

            #vis fn build(&mut self) -> Result<#name, Box<dyn ::std::error::Error>> {
                Ok(#name {
                    #(#set_fields,)*
                })
            }
        }
    }
}

fn inner_type<'a>(string: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() != 1 || path.segments[0].ident != string {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner) = path.segments[0].arguments {
            if inner.args.len() != 1 {
                return None;
            }
            if let syn::GenericArgument::Type(ref ty) = inner.args.first().unwrap() {
                return Some(ty);
            }
        }
    }
    None
}

fn is_optional_type(ty: &syn::Type) -> bool {
    inner_type("Option", ty).is_some()
}
