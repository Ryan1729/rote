// We might have different platform layer options later, so let's keep this separate.
use platform_types::EditorAPI;
pub fn run(editor_api: EditorAPI) {
    let result = wimp::run(editor_api);

    if let Err(e) = result {
        println!("wimp::run(editor_api) error:\n{}", e);
    }
}
