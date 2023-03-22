extern crate cc;

use std::path::PathBuf;

fn main() {
    {
        const GRAMMAR_NAME: &str = "tree-sitter-rust";
        let dir: PathBuf = [GRAMMAR_NAME, "src"].iter().collect();
    
        cc::Build::new()
            .include(&dir)
            .file(dir.join("parser.c"))
            .file(dir.join("scanner.c"))
            .warnings(false)
            .compile(GRAMMAR_NAME);
    }

    {
        const GRAMMAR_NAME: &str = "tree-sitter-c";
        let dir: PathBuf = [GRAMMAR_NAME, "src"].iter().collect();
    
        cc::Build::new()
            .include(&dir)
            .file(dir.join("parser.c"))
            //.file(dir.join("scanner.c"))
            .warnings(false)
            .compile(GRAMMAR_NAME);
    }
}