extern crate proc_macro;
use proc_macro::TokenStream;

#[cfg(feature = "invariant-checking")]
#[proc_macro_attribute]
#[cfg(feature = "invariant-checking")]
pub fn check_or_no_panic(attrs: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[cfg(not(feature = "invariant-checking"))]
#[proc_macro_attribute]
#[cfg(not(feature = "invariant-checking"))]
pub fn check_or_no_panic(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut output: TokenStream = quote::quote! {
        #[check_or_no_panic::no_panic]
    }
    .into();

    output.extend(item);

    output
}
