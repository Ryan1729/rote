use super::*;
use screen_positioning::{FontInfo, TextBoxXY, TextBoxXYWH, TextSpaceXY};
pub use pub_arb_g_i::{selection_adjustment, selectable_vec1};
use arb_macros::{arb_enum};
use proptest::collection::vec;
use proptest::num::f32;
use proptest::prelude::{prop_compose, any, Strategy};
use pub_arb_std::{path_buf, f32::usual};

pub fn apron(spec: f32::Any) -> impl Strategy<Value = Apron> {
    (spec, spec, spec, spec).prop_map(|(left_w, right_w, top_h, bottom_h)| Apron {
        left_w,
        right_w,
        top_h,
        bottom_h,
    })
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

pub fn text_box_space_xy(spec: f32::Any) -> impl Strategy<Value = TextBoxSpaceXY> {
    (spec, spec).prop_map(|(x, y)| TextBoxSpaceXY { x, y })
}

pub fn text_box_xywh(spec: f32::Any) -> impl Strategy<Value = TextBoxXYWH> {
    (text_box_xy(spec), wh(spec)).prop_map(|(xy, wh)| TextBoxXYWH { xy, wh })
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
        paths in vec(path_buf(), 0..=max_len),
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
        Path(_) => path_buf().prop_map(Path),
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
            chars: Rope::from(chars),
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
    pub fn font_info()(
        text_char_dim in char_dim(usual()),
        status_char_dim in char_dim(usual()),
        tab_char_dim in char_dim(usual()),
        find_replace_char_dim in char_dim(usual()),
    ) -> FontInfo {
        FontInfo {
            text_char_dim,
            status_char_dim,
            tab_char_dim,
            find_replace_char_dim,
        }
    }
}

prop_compose!{
    pub fn size_dependents()(
        f_i in proptest::option::of(font_info()),
        buffer_xywh in proptest::option::of(text_box_xywh(usual())),
        find_xywh in proptest::option::of(text_box_xywh(usual())),
        replace_xywh in proptest::option::of(text_box_xywh(usual())),
        go_to_position_xywh in proptest::option::of(text_box_xywh(usual())),
    ) -> SizeDependents {
        SizeDependents {
            font_info: f_i,
            buffer_xywh,
            find_xywh,
            replace_xywh,
            go_to_position_xywh,
        }
    }
}

prop_compose!{
    pub fn buffer_id()(
        kind in buffer_id_kind(),
        index in pub_arb_g_i::index(16),
    ) -> BufferId {
        BufferId {
            kind,
            index,
        }
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

arb_enum!{
    pub fn edited_transition() -> EditedTransition {
        ToEdited => Just(ToEdited),
        ToUnedited => Just(ToUnedited),
    }
}

prop_compose!{
    pub fn indexed_edited_transition(max_len: g_i::LengthSize)(
        i in pub_arb_g_i::index(max_len),
        e_t in edited_transition(),
    ) -> IndexedEditedTransition {
        (i, e_t)
    }
}

prop_compose!{
    pub fn edited_transitions()(
        inner in proptest::collection::vec(indexed_edited_transition(16), 0..=16),
    ) -> EditedTransitions {
        EditedTransitions(inner)
    }
}

prop_compose!{
    pub fn view()(
        current_buffer_kind in buffer_id_kind(),
        buffers in selectable_vec1(buffer_view(), 16),
        menu in menu_view(),
        status_line in status_line_view(),
        e_t in edited_transitions(),
    ) -> View {
        View {
            current_buffer_kind,
            buffers,
            menu,
            status_line,
            edited_transitions: e_t,
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

arb_enum!{
    pub fn replace_or_add() -> ReplaceOrAdd {
        Replace => Just(Replace),
        Add => Just(Add),
    }
}

arb_enum!{
    pub fn input() -> Input {
        None => Just(None),
        Quit => Just(Quit),
        CloseMenuIfAny => Just(CloseMenuIfAny),
        Insert(_) => any::<char>().prop_map(Insert),
        Delete => Just(Delete),
        DeleteLines => Just(DeleteLines),
        ResetScroll => Just(ResetScroll),
        ScrollVertically(_) => usual().prop_map(ScrollVertically),
        ScrollHorizontally(_) => usual().prop_map(ScrollHorizontally),
        SetSizeDependents(_) => size_dependents().prop_map(SetSizeDependents),
        MoveAllCursors(_) => r#move().prop_map(MoveAllCursors),
        ExtendSelectionForAllCursors(_) => r#move().prop_map(ExtendSelectionForAllCursors),
        SelectAll => Just(SelectAll),
        SetCursor(_, _) => (text_box_space_xy(usual()), replace_or_add())
            .prop_map(|(xy, r_or_add)| SetCursor(xy, r_or_add)),
        DragCursors(_) => text_box_space_xy(usual()).prop_map(DragCursors),
        SelectCharTypeGrouping(_, _) => (text_box_space_xy(usual()), replace_or_add())
            .prop_map(|(xy, r_or_add)| SelectCharTypeGrouping(xy, r_or_add)),
        ExtendSelectionWithSearch => Just(ExtendSelectionWithSearch),
        SavedAs(_, _) => (pub_arb_g_i::index(16), path_buf())
            .prop_map(|(i, p)| SavedAs(i, p)),
        Undo => Just(Undo),
        Redo => Just(Redo),
        Cut => Just(Cut),
        Copy => Just(Copy),
        Paste(_) => proptest::option::of(".*").prop_map(Paste),
        InsertNumbersAtCursors => Just(InsertNumbersAtCursors),
        AddOrSelectBuffer(_, _) => (buffer_name(), ".*").prop_map(|(bn, s)| AddOrSelectBuffer(bn, s)),
        NewScratchBuffer(_) => proptest::option::of(".*").prop_map(NewScratchBuffer),
        TabIn => Just(TabIn),
        TabOut => Just(TabOut),
        AdjustBufferSelection(_) => selection_adjustment()
            .prop_map(AdjustBufferSelection),
        NextLanguage => Just(NextLanguage),
        SelectBuffer(_) => buffer_id().prop_map(SelectBuffer),
        OpenOrSelectBuffer(_) => path_buf().prop_map(OpenOrSelectBuffer),
        CloseBuffer(_) => pub_arb_g_i::index(16).prop_map(CloseBuffer),
        SetMenuMode(_) => menu_mode().prop_map(SetMenuMode),
        SubmitForm => Just(SubmitForm),
    }
}