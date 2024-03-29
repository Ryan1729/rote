use crate::{*, screen_positioning::{*, FontInfo, TextBoxXY, TextBoxXYWH, TextSpaceXY}};
use crate::spans::SpanKindRaw;
use crate::screen_positioning::{
    Apron,
};
use super::ScrollableScreen;
pub use pub_arb_g_i::{selection_adjustment, selectable_vec1};
use arb_macros::{arb_enum};
use proptest::collection::vec;
use proptest::f32;
use proptest::{prop_compose, Strategy, any_u8 as any_span_kind_raw, extra::*};
use pub_arb_std::{path_buf, f32::usual};
use pub_arb_abs::{abs_pos, abs_pos_quarter, abs_length, abs_vector, abs_vector_quarter};
use pub_arb_f32_0_1::{f32_0_1};
use pub_arb_pos_f32::{pos_f32};

pub fn apron() -> impl Strategy<Value = Apron> {
    let strat = f32_0_1();
    (strat, strat, strat, strat).prop_map(|(left_w_ratio, right_w_ratio, top_h_ratio, bottom_h_ratio)| Apron {
        left_w_ratio,
        right_w_ratio,
        top_h_ratio,
        bottom_h_ratio,
    })
}

pub fn char_dim() -> impl Strategy<Value = CharDim> {
    let strat = abs_length();
    (strat, strat).prop_map(|(w, h)| CharDim { w, h })
}

pub fn scroll_xy() -> impl Strategy<Value = ScrollXY> {
    let spec = abs_vector();
    (spec, spec).prop_map(|(x, y)| slxy!{ x, y })
}

pub fn scroll_xy_quarter() -> impl Strategy<Value = ScrollXY> {
    let spec = abs_vector_quarter();
    (spec, spec).prop_map(|(x, y)| ScrollXY { x, y })
}

pub fn text_xy() -> impl Strategy<Value = TextSpaceXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| tsxy!{ x, y })
}

pub fn text_xy_quarter() -> impl Strategy<Value = TextSpaceXY> {
    let spec = abs_pos_quarter();
    (spec, spec).prop_map(|(x, y)| TextSpaceXY { x, y })
}

pub fn text_box_xy() -> impl Strategy<Value = TextBoxXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| tbxy!{ x, y })
}

pub fn text_box_xy_quarter() -> impl Strategy<Value = TextBoxXY> {
    let spec = abs_pos_quarter();
    (spec, spec).prop_map(|(x, y)| tbxy!{ x, y })
}

pub fn text_box_space_xy() -> impl Strategy<Value = TextBoxSpaceXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| tbsxy!{ x, y })
}

pub fn text_box_xywh() -> impl Strategy<Value = TextBoxXYWH> {
    (text_box_xy(), wh()).prop_map(|(xy, wh)| TextBoxXYWH { xy, wh })
}

pub fn rounded_non_negative_text_xy() -> impl Strategy<Value = TextSpaceXY> {
    let spec = 0..(1 << 24);
    (spec.clone(), spec).prop_map(|(x, y)| tsxy! {
        x as f32,
        y as f32,
    })
}

pub fn screen_xy() -> impl Strategy<Value = ScreenSpaceXY> {
    let spec = abs_pos();
    (spec, spec).prop_map(|(x, y)| ssxy!{ x, y })
}

pub fn screen_xy_quarter() -> impl Strategy<Value = ScreenSpaceXY> {
    let spec = abs_pos_quarter();
    (spec, spec).prop_map(|(x, y)| ssxy!{ x, y })
}

pub fn rounded_non_negative_screen_xy() -> impl Strategy<Value = ScreenSpaceXY> {
    let spec = 0..(1 << 24);
    (spec.clone(), spec).prop_map(|(x, y)| ssxy!{
        x as f32,
        y as f32,
    })
}

prop_compose!{
    pub fn position()(
        l in any_usize(),
        o in any_usize(),
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

pub fn wh() -> impl Strategy<Value = ScreenSpaceWH> {
    let strat = abs_length();
    (strat, strat).prop_map(|(w, h)| ScreenSpaceWH { w, h })
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
        result_count in any_usize(),
    ) -> FindReplaceView {
        FindReplaceView {
            mode,
            find,
            replace,
            result_count,
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
        Scratch(_) => scratch_buffer_name(),
        Path(_) => path_buffer_name(),
    }
}

prop_compose!{
    pub fn scratch_buffer_name()
    (n in any_u32().prop_map(BufferName::Scratch)) -> BufferName {
        n
    }
}

prop_compose!{
    pub fn path_buffer_name()
    (n in path_buf().prop_map(BufferName::Path)) -> BufferName {
        n
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BufferNameSpec {
    Any,
    Path,
    Scratch,
}
d!(for BufferNameSpec: BufferNameSpec::Any);

pub fn buffer_name_with_spec(spec: BufferNameSpec) -> impl Strategy<Value = BufferName> {
    u!{BufferNameSpec}
    match spec {
        Any => buffer_name().boxed(),
        Path => path_buffer_name().boxed(),
        Scratch => scratch_buffer_name().boxed(),
    }
}

prop_compose!{
    pub fn buffer_view_data()(
        chars in ".*",
        scroll in scroll_xy(),
        cursors in vec(cursor_view(), 0..=16),
        highlights in vec(highlight(), 0..=16),
        spans in spans(16),
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
    pub fn buffer_label()(
        name in buffer_name(),
    ) -> BufferLabel {
        let name_string = name.to_string();
        BufferLabel {
            name,
            name_string
        }
    }
}

prop_compose!{
    pub fn buffer_view()(
        label in buffer_label(),
        data in buffer_view_data(),
    ) -> BufferView {
        BufferView {
            label,
            data,
        }
    }
}

prop_compose!{
    pub fn spans(max_len: usize)(
        spans in vec(span_view(), 0..=max_len)
    ) -> Spans {
        Spans::from(spans)
    }
}

prop_compose!{
    pub fn span_kind()(
        kind in any_span_kind_raw().prop_map(SpanKind::new)
    ) -> SpanKind {
        kind
    }
}

prop_compose!{
    pub fn span_view()(
        one_past_end in any_usize(),
        kind in span_kind(),
    ) -> SpanView {
        SpanView {
            one_past_end: ByteIndex(one_past_end),
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
        text_char_dim in char_dim(),
        status_char_dim in char_dim(),
        tab_char_dim in char_dim(),
        find_replace_char_dim in char_dim(),
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
        buffer_xywh in proptest::option::of(text_box_xywh()),
        find_xywh in proptest::option::of(text_box_xywh()),
        replace_xywh in proptest::option::of(text_box_xywh()),
        go_to_position_xywh in proptest::option::of(text_box_xywh()),
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
    pub fn stats()(
        _: ()
    ) -> ViewStats {
        d!() // Do we care?
        /*ViewStats {
            latest_render_duration: Duration::new(secs, nanos)
        }*/
    }
}

/*prop_compose!{
    pub fn stats()(
        (secs, nanos) in (any_u64(), any_u32()),
    ) -> ViewStats {
        ViewStats {
            latest_render_duration: Duration::new(secs, nanos)
        }
    }
}*/

prop_compose!{
    pub fn view()(
        current_buffer_kind in buffer_id_kind(),
        buffers in selectable_vec1(buffer_label(), 16),
        menu in menu_view(),
        status_line in status_line_view(),
        e_t in edited_transitions(),
        stats in stats(),
    ) -> View {
        View {
            current_buffer_kind,
            buffers,
            menu,
            status_line,
            edited_transitions: e_t,
            stats,
        }
    }
}

pub fn scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    (scroll_xy(), wh()).prop_map(|(scroll, wh)| ScrollableScreen {
        scroll,
        wh,
    })
}

pub fn plausible_scrollable_screen() -> impl Strategy<Value = ScrollableScreen> {
    let a_p = abs_vector();
    let p_a_p = abs_length();
    (a_p, a_p, p_a_p, p_a_p).prop_map(|(x, y, w, h)| ScrollableScreen {
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
        Escape => Just(Escape),
        Insert(_) => insert(),
        Delete => Just(Delete),
        DeleteLines => Just(DeleteLines),
        ResetScroll => Just(ResetScroll),
        ScrollVertically(_) => usual().prop_map(|f| ScrollVertically(abs::Vector::from(f))),
        ScrollHorizontally(_) => usual().prop_map(|f| ScrollHorizontally(abs::Vector::from(f))),
        SetSizeDependents(_) => size_dependents()
            .prop_map(Box::new)
            .prop_map(SetSizeDependents),
        MoveAllCursors(_) => r#move().prop_map(MoveAllCursors),
        ExtendSelectionForAllCursors(_) => r#move().prop_map(ExtendSelectionForAllCursors),
        SelectAll => Just(SelectAll),
        SetCursor(_, _) => (text_box_space_xy(), replace_or_add())
            .prop_map(|(xy, r_or_add)| SetCursor(xy, r_or_add)),
        DragCursors(_) => text_box_space_xy().prop_map(DragCursors),
        SelectCharTypeGrouping(_, _) => (text_box_space_xy(), replace_or_add())
            .prop_map(|(xy, r_or_add)| SelectCharTypeGrouping(xy, r_or_add)),
        ExtendSelectionWithSearch => Just(ExtendSelectionWithSearch),
        ExtendSelectionMaximallyWithSearch => Just(ExtendSelectionMaximallyWithSearch),
        SavedAs(_, _) => saved_as(),
        Undo => Just(Undo),
        Redo => Just(Redo),
        Cut => Just(Cut),
        Copy => Just(Copy),
        Paste(_) => proptest::option::of(".*").prop_map(Paste),
        InsertNumbersAtCursors => Just(InsertNumbersAtCursors),
        AddOrSelectBuffer(_, _) => (buffer_name(), ".*").prop_map(|(bn, s)| AddOrSelectBuffer(bn, s)),
        AddOrSelectBufferThenGoTo(_, _, _) => (buffer_name(), ".*", text_pos::tests::arb::pos(16, 16)).prop_map(|(bn, s, p)| AddOrSelectBufferThenGoTo(bn, s, p)),
        NewScratchBuffer(_) => proptest::option::of(".*").prop_map(NewScratchBuffer),
        TabIn => Just(TabIn),
        TabOut => Just(TabOut),
        AdjustBufferSelection(_) => selection_adjustment()
            .prop_map(AdjustBufferSelection),
        NextLanguage => Just(NextLanguage),
        PreviousLanguage => Just(PreviousLanguage),
        SelectBuffer(_) => buffer_id().prop_map(SelectBuffer),
        ToggleSingleLineComments => Just(ToggleSingleLineComments),
        ToggleCase => Just(ToggleCase),
        AutoIndentSelection => Just(AutoIndentSelection),
        DuplicateLines => Just(DuplicateLines),
        OpenOrSelectBuffer(_) => path_buf().prop_map(OpenOrSelectBuffer),
        CloseBuffer(_) => close_buffer(),
        SetMenuMode(_) => menu_mode().prop_map(SetMenuMode),
        SubmitForm => Just(SubmitForm),
        StripTrailingWhitespace => Just(StripTrailingWhitespace),
        ShowError(_) => Just(ShowError("test ShowError".to_string())),
    }
}

prop_compose!{
    pub fn close_buffer()
    (input in pub_arb_g_i::index(16).prop_map(Input::CloseBuffer)) -> Input {
        input
    }
}

prop_compose!{
    pub fn insert()
    (input in any_char().prop_map(Input::Insert)) -> Input {
        input
    }
}

prop_compose!{
    pub fn saved_as()
    (input in (pub_arb_g_i::index(16), path_buf())
            .prop_map(|(i, p)| Input::SavedAs(i, p))) -> Input {
        input
    }
}