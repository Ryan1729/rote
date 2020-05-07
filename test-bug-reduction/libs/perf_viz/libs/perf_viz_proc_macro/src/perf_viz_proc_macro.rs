extern crate proc_macro;
use proc_macro::TokenStream;

#[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
#[proc_macro_attribute]
#[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
pub fn record(attrs: TokenStream, item: TokenStream) -> TokenStream {
    flamer::flame(attrs, item)
}

#[cfg(not(any(feature = "flame-chart", feature = "flame-graph")))]
#[proc_macro_attribute]
#[cfg(not(any(feature = "flame-chart", feature = "flame-graph")))]
pub fn record(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
