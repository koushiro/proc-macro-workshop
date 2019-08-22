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
    let name = ast.ident;
    let builder_name = format_ident!("{}Builder", name);

    let fields = match ast.data {
        syn::Data::Struct(data) => data.fields,
        _ => unreachable!(),
    };

    let field_idents = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        quote!(#field_ident)
    }).collect::<Vec<_>>();

    let field_types = fields.iter().map(|field| {
        let field_type = &field.ty;
        quote!(#field_type)
    }).collect::<Vec<_>>();

    let option: syn::Path = syn::parse_str("::std::option::Option").unwrap();

    quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_idents: #option::None,)*
                }
            }
        }

        pub struct #builder_name {
            #(#field_idents: #option<#field_types>,)*
        }

        impl #builder_name {
            #(
                fn #field_idents (&mut self, #field_idents: #field_types) -> &mut Self {
                    self.#field_idents = #option::Some(#field_idents);
                    self
                }
            )*
        }
    }
}
