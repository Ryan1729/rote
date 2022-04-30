use proc_macro::TokenStream;
use syn::{parse_quote, FnArg, ItemMod, ImplItemMethod, Visibility, visit_mut::{visit_item_mod_mut, visit_impl_item_method_mut}, parse_macro_input, visit_mut::VisitMut};
use quote::{ToTokens};

#[derive(Default)]
struct MutMethods {
    method_names: Vec<String>,
}

impl VisitMut for MutMethods {
    fn visit_item_mod_mut(&mut self, item_mod: &mut ItemMod) {
        visit_item_mod_mut(self, item_mod);

        match item_mod.content {
            Some((_, ref mut items)) => {
                let method_names = &self.method_names;
                let length = method_names.len();

                items.push(parse_quote!{
                    pub const MUT_METHODS: [&'static str; #length] = [
                        #(#method_names),*
                    ];
                });
            }
            None => {
                // TODO good error message with `compile_error` or similar
                panic!("Need mod with content")
            }
        }
    }

    fn visit_impl_item_method_mut(&mut self, method: &mut ImplItemMethod) {
        visit_impl_item_method_mut(self, method);

        if let Visibility::Public(_) = method.vis {
            let signature = &method.sig;
            if let Some(FnArg::Receiver(r)) = signature.receiver() {
                if r.mutability.is_some() {
                    self.method_names.push(signature.ident.to_string());
                }
            }
        }
    }
}

/// This proc-macro collects the names of all the methods that use "&mut self" receivers and creates a 
/// const array of &'static str containing them inside the mod this attribute was placed on.
#[proc_macro_attribute]
pub fn mut_methods(_attr: TokenStream, mod_tokens: TokenStream) -> TokenStream {
    let mut item_mod = parse_macro_input!(mod_tokens as ItemMod);

    MutMethods::default().visit_item_mod_mut(&mut item_mod);

    item_mod
        .into_token_stream()
        .into()
}
