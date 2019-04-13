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

pub fn start_record<S: Into<std::borrow::Cow<'static, str>>>(_s: S) {
    #[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
    flame::start(_s);
}

pub fn end_record<S: Into<std::borrow::Cow<'static, str>>>(_s: S) {
    #[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
    flame::end(_s);
}

#[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
pub fn flame_start_guard<S: Into<std::borrow::Cow<'static, str>>>(s: S) -> flame::SpanGuard {
    flame::start_guard(s)
}

#[cfg(feature = "flame-chart")]
pub fn flame_output() {
    println!("Writing out flame chart");

    let path: &std::path::Path = "flame-chart.html".as_ref();
    let mut file = std::fs::File::create(path).unwrap();
    flame::dump_html(&mut file).unwrap();

    describe_output(path)
}

// from https://github.com/TyOverby/flame/issues/33#issuecomment-352312506
#[cfg(feature = "flame-graph")]
fn merge_spans(spans: &mut Vec<flame::Span>) {
    if spans.is_empty() {
        return;
    }

    // Sort so spans to be merged are adjacent and spans with the most children are
    // merged into to minimise allocations.
    spans.sort_unstable_by(|s1, s2| {
        let a = (&s1.name, s1.depth, usize::max_value() - s1.children.len());
        let b = (&s2.name, s2.depth, usize::max_value() - s2.children.len());
        a.cmp(&b)
    });

    // Copy children and sum delta from spans to be merged
    let mut merge_targets = vec![0];
    {
        let mut spans_iter = spans.iter_mut().enumerate();
        let (_, mut current) = spans_iter.next().unwrap();
        for (i, span) in spans_iter {
            if current.name == span.name && current.depth == span.depth {
                current.delta += span.delta;
                let children = std::mem::replace(&mut span.children, Vec::new());
                current.children.extend(children.into_iter());
            } else {
                current = span;
                merge_targets.push(i);
            }
        }
    }

    // Move merged spans to the front of the spans vector
    for (target_i, &current_i) in merge_targets.iter().enumerate() {
        spans.swap(target_i, current_i);
    }

    // Remove duplicate spans
    spans.truncate(merge_targets.len());

    // Merge children of the newly collapsed spans
    for span in spans {
        merge_spans(&mut span.children);
    }
}

#[cfg(feature = "flame-graph")]
pub fn flame_output() {
    for (thread_name, mut spans) in flame::threads()
        .into_iter()
        .enumerate()
        .map(|(i, t)| (t.name.unwrap_or_else(|| format!("thread {}", i)), t.spans))
    {
        println!("Writing out flame graph for {} thread", thread_name);
        merge_spans(&mut spans);

        let path_name = format!("flame-graph-{}.html", thread_name);
        let path: &std::path::Path = path_name.as_ref();
        let mut file = std::fs::File::create(path).unwrap();
        flame::dump_html_custom(&mut file, &spans).unwrap();
        describe_output(path)
    }

    let mut spans = flame::threads().into_iter().fold(Vec::new(), |mut acc, t| {
        acc.extend(t.spans);
        acc
    });

    println!("Writing out flame graph for all threads");
    merge_spans(&mut spans);

    let path_name = format!("flame-graph-all.html");
    let path: &std::path::Path = path_name.as_ref();
    let mut file = std::fs::File::create(path).unwrap();
    flame::dump_html_custom(&mut file, &spans).unwrap();
    describe_output(path)
}

#[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
fn describe_output(path: &std::path::Path) {
    println!(
        "Wrote to \"{}\". ({}kb)",
        path.to_string_lossy(),
        std::fs::metadata(path).map(|m| m.len() >> 10).unwrap()
    );
}

#[cfg(any(feature = "flame-chart", feature = "flame-graph"))]
#[macro_export]
macro_rules! output {
    () => {
        perf_viz::flame_output();
    };
}

#[cfg(not(any(feature = "flame-chart", feature = "flame-graph")))]
#[macro_export]
macro_rules! output {
    () => {
        println!("No perf_viz features enabled.");
    };
}
