extern crate proc_macro;
#[macro_use]
extern crate quote;

use syn::parse::{Error, Parse, ParseStream, Result};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // Construct builder pattern for structure.
    let output = impl_builder_for_struct(input);

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(output)
}

fn impl_builder_for_struct(ast: syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = ast.ident;
    let builder_name = format_ident!("{}Builder", name);

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => panic!("#[derive(Builder)] is only defined for structs"),
    };

    match fields {
        syn::Fields::Named(_) => {}
        _ => panic!("#[derive(Builder)] is not defined for Unnamed structs and Uint structs"),
    }

    let impl_builder_fn_for_struct = impl_builder_fn_for_struct(&name, &builder_name, &fields);
    let struct_builder = struct_builder(&builder_name, &fields);
    let impl_build_fn_for_struct_builder =
        impl_build_fn_for_struct_builder(&name, &builder_name, &fields);
    let impl_setters_for_struct_builder = impl_setters_for_struct_builder(&builder_name, &fields)
        .unwrap_or_else(|err| err.to_compile_error());

    quote! {
        #impl_builder_fn_for_struct

        #struct_builder

        #impl_build_fn_for_struct_builder

        #impl_setters_for_struct_builder
    }
}

fn impl_builder_fn_for_struct(
    name: &syn::Ident,
    builder_name: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let builder_default_fields = fields.iter().map(|field| {
        let ident = &field.ident;
        if field_is_vec(field) {
            quote! { #ident: std::vec![], }
        } else {
            quote! { #ident: std::option::Option::None, }
        }
    });
    quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #builder_default_fields )*
                }
            }
        }
    }
}

fn struct_builder(builder_name: &syn::Ident, fields: &syn::Fields) -> proc_macro2::TokenStream {
    let builder_fields = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if field_is_option(field) || field_is_vec(field) {
            quote! { #ident: #ty, }
        } else {
            quote! { #ident: std::option::Option<#ty>, }
        }
    });
    quote! {
        pub struct #builder_name {
            #( #builder_fields )*
        }
    }
}

fn impl_setters_for_struct_builder(
    builder_name: &syn::Ident,
    fields: &syn::Fields,
) -> Result<proc_macro2::TokenStream> {
    let builder_setters = fields.iter().map(|field| {
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
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }
    });
    Ok(quote! {
        impl #builder_name {
            #( #builder_setters )*
        }
    })
}

fn impl_build_fn_for_struct_builder(
    name: &syn::Ident,
    builder_name: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let build_set_fields = fields
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
        });
    quote! {
        impl #builder_name {
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #( #build_set_fields )*
                })
            }
        }
    }
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

struct BuilderEachAttr {
    fn_name: syn::LitStr,
}

impl Parse for BuilderEachAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let meta = input.parse::<syn::Meta>()?;
        let err = Error::new_spanned(&meta, "expected `builder(each = \"...\")");
        if let syn::Meta::NameValue(nv) = meta {
            if nv.path.is_ident("each") {
                if let syn::Lit::Str(fn_name) = nv.lit {
                    return Ok(BuilderEachAttr { fn_name });
                }
            }
            Err(err)
        } else {
            Err(err)
        }
    }
}
