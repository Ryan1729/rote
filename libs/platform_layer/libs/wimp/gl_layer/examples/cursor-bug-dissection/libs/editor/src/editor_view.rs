use super::*;
const AVERAGE_SELECTION_LINES_ESTIMATE: usize = 4;

fn scrollable_to_buffer_view_data(
    scrollable: &ScrollableBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let buffer = &scrollable.text_buffer;
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

    let chars = buffer.chars().collect::<String>();

    BufferViewData {
        scroll: scrollable.scroll,
        chars,
        cursors,
        highlights,
        ..d!()
    }
}

fn editor_to_buffer_view_data(
    parsers: &mut Parsers,
    editor_buffer: &EditorBuffer,
    selection_lines_estimate: usize,
) -> BufferViewData {
    let mut buffer_view_data =
        scrollable_to_buffer_view_data(&editor_buffer.scrollable, selection_lines_estimate);

    buffer_view_data.spans = parsers.get_spans(
        &buffer_view_data.chars,
        editor_buffer.get_parser_kind()
    );

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

    buffer_view_data
}

#[perf_viz::record]
pub fn render(
    &mut State {
        ref buffers,
        font_info: FontInfo { text_char_dim, .. },
        current_buffer_id,
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
        ..
    }: &mut State,
    view: &mut View,
) {
    view.index_state = buffers.index_state;

    view.buffers.clear();

    for editor_buffer in buffers.iter() {
        let name = &editor_buffer.name;
        view.buffers.push(BufferView {
            name: name.clone(),
            name_string: name.to_string(),
            data: editor_to_buffer_view_data(parsers, &editor_buffer, AVERAGE_SELECTION_LINES_ESTIMATE),
        });
    }

    view.status_line.chars.clear();
    view.visible_buffer = d!();

    let current_buffer_index = current_buffer_id.index;
    match buffers.get(current_buffer_index) {
        Some(EditorBuffer {
            scrollable:
                ScrollableBuffer {
                    text_buffer: buffer,
                    scroll,
                    ..
                },
            ..
        }) => {
            let scroll = *scroll;
            view.visible_buffer = Some(current_buffer_index);
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
                display_option_compactly(
                    current_buffer_index
                        .get(view.index_state)
                        .map(|i| i.saturating_add(1))
                ),
                usize::from(buffers.len())
            );

            // debugging
            let _cannot_actually_fail = write!(chars, "  ? t{} s{}", text_char_dim, scroll);

            for c in buffer.borrow_cursors().iter() {
                let _cannot_actually_fail = write!(
                    chars,
                    "{} {} ({}|{}), ",
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
        }
        None => {
            view.status_line.chars.push_str(DEFAULT_STATUS_LINE_CHARS);
        }
    };

    const FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE: usize = 1;

    view.menu = match menu_mode {
        MenuMode::Hidden => MenuView::None,
        MenuMode::FindReplace(mode) => MenuView::FindReplace(FindReplaceView {
            mode,
            find: scrollable_to_buffer_view_data(
                &find,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
            replace: scrollable_to_buffer_view_data(
                &replace,
                FIND_REPLACE_AVERAGE_SELECTION_LINES_ESTIMATE,
            ),
        }),
        MenuMode::FileSwitcher => {
            const FILE_SEARCH_SELECTION_LINES_ESTIMATE: usize = 1;
            MenuView::FileSwitcher(FileSwitcherView {
                search: scrollable_to_buffer_view_data(
                    &file_switcher,
                    FILE_SEARCH_SELECTION_LINES_ESTIMATE,
                ),
                results: dbg!(file_switcher_results.clone()),
            })
        },
        MenuMode::GoToPosition => {
            const GO_TO_POSITION_SELECTION_LINES_ESTIMATE: usize = 1;
            MenuView::GoToPosition(GoToPositionView {
                go_to_position: scrollable_to_buffer_view_data(
                    &go_to_position,
                    GO_TO_POSITION_SELECTION_LINES_ESTIMATE,
                ),
            })
        },
    };

    view.current_buffer_id = current_buffer_id;
}