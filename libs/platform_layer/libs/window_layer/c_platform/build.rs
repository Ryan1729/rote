// https://internals.rust-lang.org/t/cfg-keyword-for-if-multiple-more-than-one/15751/4
const _: () = {
    let enabled_features = {
        cfg!(feature = "c-platform-dummy") as u32
        + cfg!(feature = "c-platform-linux-posix") as u32
    };

    match enabled_features {
        0 => panic!("\nNone of the c-platform features were enabled. You must enable exactly one."),
        1 => {}
        2.. => panic!("\nMultiple of the c-platform features were enabled at once. You must enable exactly one."),
    }
};

#[cfg(all(feature = "c-platform-linux-posix", not(target_os = "linux")))]
compile_error!{ "The c-platform-linux-posix feature is only known to work on Linux. The c code likely needs to be adjusted for other platforms." }

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

    let mut build = cc::Build::new();
    
    build.flag("--std=c23");
        

    #[cfg(feature = "c-platform-dummy")]
    {
        build.file("dummy.c");
    }

    #[cfg(feature = "c-platform-linux-posix")]
    {
        println!("cargo:rustc-link-lib=GL");
        println!("cargo:rustc-link-lib=Xrandr");
        println!("cargo:rustc-link-lib=X11");
        println!("cargo:rustc-link-lib=dl");
        println!("cargo:rustc-link-lib=pthread");
        println!("cargo:rustc-link-lib=m");
        
        println!("cargo:rustc-link-arg=-fopenmp");

        build
            .include("RGFW/")
            .file("linux_posix.c");
    }

    build.compile("c_platform");
}