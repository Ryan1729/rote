use super::*;
const AVERAGE_SELECTION_LINES_ESTIMATE: usize = 4;

use macros::{dbg, format_if, SaturatingAdd};
use search::SearchResults;

#[perf_viz::record]
fn text_buffer_to_buffer_view_data(
    buffer: &TextBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let buffer_cursors = buffer.borrow_cursors();
    let cursors_len = buffer_cursors.len();
    let mut cursors = Vec::with_capacity(cursors_len);
    let mut highlights = Vec::with_capacity(cursors.len() * selection_lines_estimate);

    for c in buffer_cursors.iter() {
        let position = c.get_position();

        cursors.push(CursorView {
            position,
            state: c.state,
        });

        push_highlights(&mut highlights, position, c.get_highlight_position(), d!());
    }

    BufferViewData {
        scroll: buffer.scroll,
        chars: buffer.clone_rope(),
        cursors,
        highlights,
        ..d!()
    }
}

#[perf_viz::record]
fn editor_to_buffer_view_data(
    parsers: &mut Parsers,
    buffer_name: BufferName,
    editor_buffer: &EditorBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let mut buffer_view_data =
        text_buffer_to_buffer_view_data(&editor_buffer.text_buffer, selection_lines_estimate);

    perf_viz::start_record!("parsers.get_spans");
    buffer_view_data.spans = parsers.get_spans(
        buffer_view_data.chars.clone().into(),
        buffer_name,
        editor_buffer.get_parser_kind()
    );
    perf_viz::end_record!("parsers.get_spans");

    perf_viz::start_record!("push all highlights");
    let highlights = &mut buffer_view_data.highlights;
    let SearchResults {
        ref ranges,
        current_range,
        ..
    } = editor_buffer.search_results;
    for (i, &(p1, p2)) in ranges.iter().enumerate() {
        let kind = if i == current_range {
            HighlightKind::CurrentResult
        } else {
            HighlightKind::Result
        };
        push_highlights(highlights, p1, p2, kind);
    }
    perf_viz::end_record!("push all highlights");

    buffer_view_data
}

// Mainly mutates `state.view`
#[perf_viz::record]
pub fn render(
    state: &mut State,
) {
    let &mut State {
        ref mut buffers,
        font_info: FontInfo { text_char_dim, .. },
        menu_mode,
        ref file_switcher_results,
        ref file_switcher,
        ref find,
        ref replace,
        ref go_to_position,
        buffer_xywh: TextBoxXYWH {
            xy: text_box_pos, ..
        },
        ref mut parsers,
        ref mut view,
        ..
    } = state;
    
    if buffers.should_render_buffer_views()
    {
        let bufs = buffers.buffers();
        dbg!(&bufs);

        view.buffers.replace_with_mapped(
            bufs,
            |editor_buffer| {
                perf_viz::record_guard!("render BufferView");
                let name = &editor_buffer.name;
                BufferView {
                    name: name.clone(),
                    name_string: name.to_string(),
                    data: editor_to_buffer_view_data(parsers, name.clone(), &editor_buffer, AVERAGE_SELECTION_LINES_ESTIMATE),
                }
            }
        );

        dbg!(&view.buffers);
    }
    
    let editor_buffer = buffers.get_current_buffer();
    let search_results = &editor_buffer.search_results;

    perf_viz::start_record!("write view.status_line");

    view.status_line.chars.clear();

    let buffer = &editor_buffer.text_buffer;
    let scroll = buffer.scroll;
    
    fn display_option_compactly<A: ToString>(op: Option<A>) -> String {
        match op {
            None => "N".to_string(),
            Some(a) => a.to_string(),
        }
    }

    use std::fmt::Write;
    let chars = &mut view.status_line.chars;

    let _cannot_actually_fail = write!(
        chars,
        "{}/{}",
        buffers.current_index_part().saturating_add(1).to_string(),
        usize::from(buffers.len())
    );

    let _cannot_actually_fail = write!(chars, " {}{}", 
        display_option_compactly(editor_buffer.parser_kind),
        format_if!(
            editor_buffer.parser_kind.is_none(),
            "({})",
            editor_buffer.get_parser_kind()
        )
    );

    // debugging
    let _cannot_actually_fail = write!(chars, "  ? t{} s{}", text_char_dim, scroll);

    let cursors = buffer.borrow_cursors();
    let cursors_len = cursors.len();
    let _cannot_actually_fail = write!(
        chars,
        " {}",
        if cursors.len() == 1 {
            "c".to_string()
        } else {
            format!("cs({})", cursors_len)
        }
    );
    for c in cursors.iter() {
        let _cannot_actually_fail = write!(
            chars,
            " {} {} ({}|{}),",
            c,
            position_to_screen_space(c.get_position(), text_char_dim, scroll, text_box_pos),
            display_option_compactly(buffer.find_index(c).and_then(|o| if o == 0 {
                None
            } else {
                Some(o - 1)
            })),
            display_option_compactly(buffer.find_index(c)),
        );
    }

    perf_viz::end_record!("write view.status_line");
    perf_viz::start_record!("set view.menu");

    const FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE: usize = 1;
    
    view.menu = match menu_mode {
        MenuMode::Hidden => MenuView::None,
        MenuMode::FindReplace(mode) => MenuView::FindReplace(FindReplaceView {
            mode,
            find: text_buffer_to_buffer_view_data(
                &find,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
            replace: text_buffer_to_buffer_view_data(
                &replace,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
            result_count: search_results.ranges.len(),
        }),
        MenuMode::FileSwitcher => {
            const FILE_SEARCH_SELECTION_LINES_ESTIMATE: usize = 1;
            MenuView::FileSwitcher(FileSwitcherView {
                search: text_buffer_to_buffer_view_data(
                    &file_switcher,
                    FILE_SEARCH_SELECTION_LINES_ESTIMATE,
                ),
                results: file_switcher_results.clone(),
            })
        },
        MenuMode::GoToPosition => {
            const GO_TO_POSITION_SELECTION_LINES_ESTIMATE: usize = 1;
            MenuView::GoToPosition(GoToPositionView {
                go_to_position: text_buffer_to_buffer_view_data(
                    &go_to_position,
                    GO_TO_POSITION_SELECTION_LINES_ESTIMATE,
                ),
            })
        },
    };
    perf_viz::end_record!("set view.menu");

    view.current_buffer_kind = state.current_buffer_kind;
}