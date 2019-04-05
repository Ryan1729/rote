#[cfg(feature = "flame-chart")]
#[macro_export]
pub fn flame_start_guard<S: Into<std::borrow::Cow<'static, str>>>(s: S) -> flame::SpanGuard {
    flame::start_guard(s)
}

#[cfg(feature = "flame-chart")]
#[macro_export]
macro_rules! record_fn {
    () => {
        let _guard = perf_viz::flame_start_guard(format!("{}:{}", file!(), line!()));
    };
}

#[cfg(not(feature = "flame-chart"))]
#[macro_export]
macro_rules! record_fn {
    () => {};
}

#[cfg(feature = "flame-chart")]
#[macro_export]
pub fn flame_output() {
    let path: &std::path::Path = "flame-chart.html".as_ref();
    let mut file = std::fs::File::create(path).unwrap();
    flame::dump_html(&mut file).unwrap();

    println!("wrote \"{}\"", path.to_string_lossy())
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
