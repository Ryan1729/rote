extern crate perf_viz_proc_macro;

pub use perf_viz_proc_macro::record;

/// The code in this crate and some of the libraries it uses assume that this crate is called
/// `perf_viz`, and that when it is imported `perf_viz` refers to this crate. So if you were to
/// import this crate like this: `use perf_viz as pv;` and used the `perf_viz` name for another
/// crate, like so:`use some_crate as perf_viz;` things would break. That situation and other
/// situations seemed unlikely enough to move forward with that assumption, since it simplifies
/// dependency management when using this crate. Without that assumption, as far as I can tell
/// adding multipe dependencies and/or features to each crate that used this one would be
/// necessary. Or the root crate would need a dependency on at least one crate. But this way
/// everything related to performance visualization is in this crate besides the annotations.

#[cfg(feature = "flame-chart")]
pub fn flame_start_guard<S: Into<std::borrow::Cow<'static, str>>>(s: S) -> flame::SpanGuard {
    flame::start_guard(s)
}

#[cfg(feature = "flame-chart")]
pub fn flame_output() {
    println!("Writing out flame chart");

    let path: &std::path::Path = "flame-chart.html".as_ref();
    let mut file = std::fs::File::create(path).unwrap();
    flame::dump_html(&mut file).unwrap();

    println!(
        "Wrote to \"{}\". ({}kb)",
        path.to_string_lossy(),
        std::fs::metadata(path).map(|m| m.len() >> 10).unwrap()
    );
}

#[cfg(feature = "flame-chart")]
#[macro_export]
macro_rules! output {
    () => {
        perf_viz::flame_output();
    };
}

#[cfg(not(feature = "flame-chart"))]
#[macro_export]
macro_rules! output {
    () => {
        println!("#[cfg(not(feature = \"flame-chart\"))]");
    };
}
