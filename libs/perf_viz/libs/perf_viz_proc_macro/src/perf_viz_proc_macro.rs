extern crate proc_macro;
use proc_macro::TokenStream;

#[cfg(feature = "flame-chart")]
#[proc_macro_attribute]
#[cfg(feature = "flame-chart")]
pub fn record(attrs: TokenStream, item: TokenStream) -> TokenStream {
    print!("\"flame-chart\" ");
    flamer::flame(attrs, item)
}

//#[cfg(feature = "flame-chart")]
//pub use flamer::flame as record;

#[cfg(not(feature = "flame-chart"))]
#[proc_macro_attribute]
#[cfg(not(feature = "flame-chart"))]
pub fn record(_attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("#[cfg(not(feature = \"flame-chart\"))]");
    item
}
