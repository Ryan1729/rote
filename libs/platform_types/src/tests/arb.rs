use super::*;
pub use pub_arb_g_i::selectable_vec1;
use arb_macros::{arb_enum};
use proptest::collection::vec;
use proptest::num::f32;
use proptest::prelude::{prop_compose, any, Strategy};
use std::path::PathBuf;

pub fn usual() -> f32::Any {
    f32::POSITIVE | f32::NEGATIVE | f32::NORMAL | f32::ZERO
}

pub fn char_dim(spec: f32::Any) -> impl Strategy<Value = CharDim> {
    (spec, spec).prop_map(|(w, h)| CharDim { w, h })
}

pub fn scroll_xy(spec: f32::Any) -> impl Strategy<Value = ScrollXY> {
    (spec, spec).prop_map(|(x, y)| ScrollXY { x, y })
}

pub fn text_xy(spec: f32::Any) -> impl Strategy<Value = TextSpaceXY> {
    (spec, spec).prop_map(|(x, y)| TextSpaceXY { x, y })
}

pub fn text_box_xy(spec: f32::Any) -> impl Strategy<Value = TextBoxXY> {
    (spec, spec).prop_map(|(x, y)| TextBoxXY { x, y })
}

pub fn rounded_non_negative_text_xy() -> impl Strategy<Value = TextSpaceXY> {
    let spec = 0..(1 << 24);
    (spec.clone(), spec).prop_map(|(x, y)| TextSpaceXY {
        x: x as f32,
        y: y as f32,
    })
}

#[allow(dead_code)]
pub fn screen_xy(spec: f32::Any) -> impl Strategy<Value = ScreenSpaceXY> {
    (spec, spec).prop_map(|(x, y)| ScreenSpaceXY { x, y })
}

pub fn rounded_non_negative_screen_xy() -> impl Strategy<Value = ScreenSpaceXY> {
    let spec = 0..(1 << 24);
    (spec.clone(), spec).prop_map(|(x, y)| ScreenSpaceXY {
        x: x as f32,
        y: y as f32,
    })
}

prop_compose!{
    pub fn position()(
        l in any::<usize>(),
        o in any::<usize>(),
    ) -> Position {
        pos!(l l, o o)
    }
}

arb_enum!{
    pub fn highlight_kind() -> HighlightKind {
        User => Just(User),
        Result => Just(Result),
        CurrentResult => Just(CurrentResult),
    }
}

prop_compose!{
    pub fn highlight()(
        min in position(),
        max in position(),
        kind in highlight_kind(),
    ) -> Highlight {
        Highlight {
            min,
            max,
            kind,
        }
    }
}

#[allow(dead_code)]
pub fn wh(spec: f32::Any) -> impl Strategy<Value = ScreenSpaceWH> {
    (spec, spec).prop_map(|(w, h)| ScreenSpaceWH { w, h })
}

arb_enum!{
    pub fn selection_move() -> SelectionMove {
        Left => Just(Left),
        Right => Just(Right),
        ToStart => Just(ToStart),
        ToEnd => Just(ToEnd),
    }
}

arb_enum!{
    pub fn buffer_id_kind() -> BufferIdKind {
        None => Just(None),
        Text => Just(Text),
        Find => Just(Find),
        Replace => Just(Replace),
        FileSwitcher => Just(FileSwitcher),
        GoToPosition => Just(GoToPosition),
    }
}

arb_enum!{
    pub fn menu_mode() -> MenuMode {
        Hidden => Just(Hidden),
        FileSwitcher => Just(FileSwitcher),
        FindReplace(_) => find_replace_mode().prop_map(FindReplace),
        GoToPosition => Just(GoToPosition),
    }
}

arb_enum!{
    pub fn menu_view() -> MenuView {
        None => Just(None),
        FileSwitcher(_) => file_switcher_view(16).prop_map(FileSwitcher),
        FindReplace(_) => find_replace_view().prop_map(FindReplace),
        GoToPosition(_) => go_to_position_view().prop_map(GoToPosition),
    }
}

prop_compose!{
    pub fn file_switcher_results(max_len: usize)(
        paths in vec(".*".prop_map(PathBuf::from), 0..=max_len),
    ) -> FileSwitcherResults {
        paths
    }
}

prop_compose!{
    pub fn file_switcher_view(max_len: usize)(
        search in buffer_view_data(),
        results in file_switcher_results(max_len),
    ) -> FileSwitcherView {
        FileSwitcherView {
            search,
            results,
        }
    }
}

prop_compose!{
    pub fn find_replace_view()(
        mode in find_replace_mode(),
        find in buffer_view_data(),
        replace in buffer_view_data(),
    ) -> FindReplaceView {
        FindReplaceView {
            mode,
            find,
            replace,
        }
    }
}

arb_enum!{
    pub fn find_replace_mode() -> FindReplaceMode {
        CurrentFile => Just(CurrentFile),
    }
}

prop_compose!{
    pub fn go_to_position_view()(
        go_to_position in buffer_view_data(),
    ) -> GoToPositionView {
        GoToPositionView {
            go_to_position,
        }
    }
}

arb_enum!{
    pub fn buffer_name() -> BufferName {
        Path(_) => ".*\\.fakefile".prop_map(|s| { let p: PathBuf = s.into(); p }).prop_map(Path),
        Scratch(_) => any::<u32>().prop_map(Scratch),
    }
}

prop_compose!{
    pub fn buffer_view_data()(
        chars in ".*",
        scroll in scroll_xy(usual()),
        cursors in vec(cursor_view(), 0..=16),
        highlights in vec(highlight(), 0..=16),
        spans in vec(span_view(), 0..=16),
    ) -> BufferViewData {
        BufferViewData {
            chars,
            scroll,
            cursors,
            highlights,
            spans,
        }
    }
}

prop_compose!{
    pub fn buffer_view()(
        name in buffer_name(),
        data in buffer_view_data(),
    ) -> BufferView {
        let name_string = name.to_string();
        BufferView {
            name,            name_string,
            data,
        }
    }
}

prop_compose!{
    pub fn span_kind()(
        kind in any::<SpanKindRaw>().prop_map(|raw| SpanKind::new(raw))
    ) -> SpanKind {
        kind
    }
}

prop_compose!{
    pub fn span_view()(
        end_byte_index in any::<usize>(),
        kind in span_kind(),
    ) -> SpanView {
        SpanView {
            end_byte_index,
            kind,
        }
    }
}

prop_compose!{
    pub fn status_line_view()(
        chars in ".*",
    ) -> StatusLineView {
        StatusLineView {
            chars,
        }
    }
}

arb_enum!{
    pub fn cursor_state() -> CursorState {
        None => Just(None),
        PressedAgainstWall(_) => r#move().prop_map(PressedAgainstWall),
    }
}

arb_enum!{
    pub fn r#move() -> Move {
        Up => Just(Up),
        Down => Just(Down),
        Left => Just(Left),
        Right => Just(Right),
        ToLineStart => Just(ToLineStart),
        ToLineEnd => Just(ToLineEnd),
        ToBufferStart => Just(ToBufferStart),
        ToBufferEnd => Just(ToBufferEnd),
        ToPreviousLikelyEditLocation => Just(ToPreviousLikelyEditLocation),
        ToNextLikelyEditLocation => Just(ToNextLikelyEditLocation),
    }
}


prop_compose!{
    pub fn cursor_view()(
        p in position(),
        state in cursor_state(),
    ) -> CursorView {
        CursorView {
            position: p,
            state,
        }
    }
}

prop_compose!{
    pub fn view()(
        current_buffer_kind in buffer_id_kind(),
        buffers in selectable_vec1(buffer_view(), 16),
        menu in menu_view(),
        status_line in status_line_view(),
    ) -> View {
        View {
            current_buffer_kind,
            buffers,
            menu,
            status_line,
        }
    }
}

pub fn scrollable_screen(spec: f32::Any) -> impl Strategy<Value = ScrollableScreen> {
    (scroll_xy(spec), spec, spec).prop_map(|(scroll, w, h)| ScrollableScreen {
        scroll,
        wh: ScreenSpaceWH { w, h },
    })
}

pub fn plausible_scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    let u = usual();
    let len = f32::POSITIVE | f32::NORMAL | f32::ZERO;
    (u, u, len, len).prop_map(|(x, y, w, h)| ScrollableScreen {
        scroll: ScrollXY { x, y },
        wh: ScreenSpaceWH { w, h },
    })
}