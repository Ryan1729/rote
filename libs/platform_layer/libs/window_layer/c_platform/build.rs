fn main() {
    // Tell cargo to invalidate the built crate whenever local .c or .h files change
    for entry in std::fs::read_dir(".").expect("Build dir not readable?!") {
        let Ok(entry) = entry else { continue };
        let path = entry.path();
        match (
            path.extension().and_then(|os_s| os_s.to_str()),
            path.file_name().and_then(|os_s| os_s.to_str())
        ) {
            (Some("c") | Some("h"), Some(name)) => {
                println!("cargo:rerun-if-changed={name}");
            }
            _ => {}
        }        
    }

    cc::Build::new()
        .file("wrapper.c")
        .flag("--std=c23")
        .compile("wrapper");
}