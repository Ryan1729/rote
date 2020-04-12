use glutin::event::{ModifiersState, VirtualKeyCode};
use macros::{d, ord, u};
use platform_types::{screen_positioning::*, g_i, Input, Cmd, EditedTransition};

use std::collections::{VecDeque, HashMap, BTreeMap};
use std::path::PathBuf;

pub use glutin::event_loop::EventLoopProxy;

mod clipboard_layer {
    pub use clipboard::ClipboardProvider;
    use shared::Res;
    use macros::fmt_debug;

    
    /// This enum exists so we can do dynamic dispatch on `ClipboardProvider` instances even though
    /// the trait requires `Sized`. The reason  we want to do that, is so that if we try to run this
    /// on a platform where `clipboard::ClipboardContext::new` retirns an `Err` we can continue
    /// operation, just without system clipboard support.
    pub enum Clipboard {
        System(clipboard::ClipboardContext),
        Fallback(clipboard::nop_clipboard::NopClipboardContext),
    }

    fmt_debug!(for Clipboard: c in "{}", {
        use Clipboard::*;
        match c {
            System(_) => {
                "System(_)"
            },
            Fallback(_) => {
                "Fallback(_)"
            },
        }
    });

    impl clipboard::ClipboardProvider for Clipboard {
        fn new() -> Res<Self> {
            let result: Result<
                clipboard::ClipboardContext,
                clipboard::nop_clipboard::NopClipboardContext,
            > = clipboard::ClipboardContext::new().map_err(|err| {
                eprintln!("System clipboard not supported. {}", err);
                // `NopClipboardContext::new` always returns an `Ok`
                clipboard::nop_clipboard::NopClipboardContext::new().unwrap()
            });

            let output = match result {
                Ok(ctx) => Clipboard::System(ctx),
                Err(ctx) => Clipboard::Fallback(ctx),
            };

            // `get_clipboard` currently relies on this neer returning `Err`.
            Ok(output)
        }
        fn get_contents(&mut self) -> Res<String> {
            match self {
                Clipboard::System(ctx) => ctx.get_contents(),
                Clipboard::Fallback(ctx) => ctx.get_contents(),
            }
        }
        fn set_contents(&mut self, s: String) -> Res<()> {
            match self {
                Clipboard::System(ctx) => ctx.set_contents(s),
                Clipboard::Fallback(ctx) => ctx.set_contents(s),
            }
        }
    }

    pub fn get_clipboard() -> Clipboard {
        // As you can see in the implementation of the `new` method, it always returns `Ok`
        Clipboard::new().unwrap()
    }
}
pub use clipboard_layer::{get_clipboard, Clipboard, ClipboardProvider};

// Parts of RunState that represent externally chosen, absolute dimensions of things, 
// including the window, from which the sizes of several UI elements are derived.
#[derive(Clone, Copy, Debug)]
pub struct Dimensions {
    pub window: ScreenSpaceWH,
    pub font: FontInfo,
}

#[derive(Clone, Debug)]
pub enum CustomEvent {
    OpenFile(PathBuf),
    SaveNewFile(PathBuf, g_i::Index),
    SendBuffersToBeSaved,
    EditedBufferError(String),
}

/// This module exists because when adding WIMP only UI elements we found that 
/// we wanted to have several parts of the code start checking the whether a 
/// WIMP only menu was up. Because I was not sure where all the places that 
/// parts of the code were that were talking directly to the 
/// `platform_types::View` and therefore might need to be changed, I wanted to 
/// lean on the compiler to find all those places for me.
mod view {
    use macros::{d};
    use super::ui; // Your app's written in Electron? Shoulda used Super UI.
    use super::g_i;
    pub use platform_types::{CursorView, BufferName, BufferViewData, FileSwitcherView, FindReplaceView, GoToPositionView, MenuMode, MenuView, IndexedEditedTransition};

    #[derive(Clone, Copy, Debug, PartialEq)]
    enum LocalMenuMode {
        Command
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum FindReplaceMode {
        CurrentFile,
    }
    d!(for FindReplaceMode: FindReplaceMode::CurrentFile);

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum WimpMenuMode {
        Hidden,
        FileSwitcher,
        FindReplace(FindReplaceMode),
        GoToPosition,
        Command,
        Debug,
    }
    d!(for WimpMenuMode: WimpMenuMode::Hidden);

    #[derive(Clone, Debug)]
    pub enum LocalMenuView {
        Command,
        Debug,
    }

    #[derive(Clone, Debug)]
    pub struct WimpMenuView<'view> {
        pub platform_menu: &'view MenuView,
        pub local_menu: &'view Option<LocalMenuView>,
    }    
    impl WimpMenuView<'_> {
        pub fn get_mode(&self) -> WimpMenuMode {
            match self.local_menu {
                Some(LocalMenuView::Command) => WimpMenuMode::Command,
                Some(LocalMenuView::Debug) => WimpMenuMode::Debug,
                None => self.platform_menu.get_mode().clone().into()
            }
        }
    }

    impl From<platform_types::MenuMode> for WimpMenuMode {
        fn from(p_m: platform_types::MenuMode) -> Self {
            match p_m {
                platform_types::MenuMode::Hidden => WimpMenuMode::Hidden,
                platform_types::MenuMode::FileSwitcher => WimpMenuMode::FileSwitcher,
                platform_types::MenuMode::FindReplace(m) => WimpMenuMode::FindReplace(m.into()),
                platform_types::MenuMode::GoToPosition => WimpMenuMode::GoToPosition
            }
        }
    }

    impl From<platform_types::FindReplaceMode> for FindReplaceMode {
        fn from(p_m: platform_types::FindReplaceMode) -> Self {
            use FindReplaceMode::*;
            match p_m {
                platform_types::FindReplaceMode::CurrentFile => CurrentFile,
            }
        }
    }

    impl From<&platform_types::FindReplaceMode> for FindReplaceMode {
        fn from(p_m: &platform_types::FindReplaceMode) -> Self {
            use FindReplaceMode::*;
            match p_m {
                platform_types::FindReplaceMode::CurrentFile => CurrentFile,
            }
        }
    }

    #[derive(Default, Debug)]
    pub struct View {
        // We want to hide the `platform_types::View` from the rest of the code
        // because we had a bug when LocalMenuView was introduced that hiding
        // the `platform_types::View` prevents.
        platform_view: platform_types::View,
        local_menu: Option<LocalMenuView>
    }

    macro_rules! toggle_impl {
        ($view: expr,  $toggled: path $(,)?) => {
            match $view.local_menu {
                None => match $view.platform_view.menu {
                    platform_types::MenuView::None => {
                        $view.local_menu = Some($toggled)
                    },
                    _ => { 
                        // We don't want to be able to layer the local menus on top of the editor menus,
                        // because that makes them feel different/less integrated.
                    }
                },
                Some($toggled) => {
                    $view.local_menu = None;
                },
                Some(_) => {
                    $view.local_menu = Some($toggled);
                }
            }
        }
    }

    impl View {
        pub fn close_menus(&mut self) {
            self.local_menu = None;
            self.platform_view.menu = platform_types::MenuView::None;
        }

        pub fn toggle_command_menu(&mut self) {
            toggle_impl!{
                self,
                LocalMenuView::Command,
            }
        }

        pub fn toggle_debug_menu(&mut self) {
            toggle_impl!{
                self,
                LocalMenuView::Debug,
            }
        }

        pub fn update(&mut self, p_view: platform_types::View) {
            self.platform_view = p_view;
        }
    }

    impl View {
        pub fn edited_transitions(&self) -> impl Iterator<Item = IndexedEditedTransition> {
            self.platform_view.edited_transitions.clone().into_iter()
        }

        pub fn buffers_count(&self) -> g_i::Length {
            self.platform_view.buffers.len()
        }

        pub fn buffer_iter(&self) -> impl Iterator<Item = (g_i::Index, &platform_types::BufferView)> {
            self.platform_view.buffers.iter_with_indexes()
        }

        fn get_selected_buffer_view_data(&self) -> Option<&BufferViewData> {
            if self.local_menu.is_none() {
                return self.platform_view.get_selected_buffer_view_data();
            }
            None
        }

        pub fn get_fresh_navigation(&self) -> Option<ui::Navigation> {
            self.get_selected_buffer_view_data()
                .map(|bvd| {
                    navigation_from_cursors(&bvd.cursors)
                })
        }

        pub fn current_buffer_kind(&self) -> platform_types::BufferIdKind {
            self.platform_view.current_buffer_kind
        }

        pub fn current_buffer_id(&self) -> platform_types::BufferId {
            self.platform_view.current_buffer_id()
        }

        pub fn current_text_index(&self) -> g_i::Index {
            self.platform_view.current_text_index()
        }

        pub fn current_text_index_and_buffer(&self) -> (g_i::Index, &platform_types::BufferView) {
            self.platform_view.current_text_index_and_buffer()
        }

        pub fn get_buffer(&self, index: g_i::Index) -> Option<&platform_types::BufferView> {
            self.platform_view.get_buffer(index)
        }

        pub fn menu(&self) -> WimpMenuView {
            WimpMenuView {
                platform_menu: &self.platform_view.menu,
                local_menu: &self.local_menu,
            }
        }

        pub fn menu_mode(&self) -> WimpMenuMode {
            self.menu().get_mode()
        }

        pub fn status_line(&self) -> &platform_types::StatusLineView {
            &self.platform_view.status_line
        }

        pub fn index_state(&self) -> g_i::State {
            self.platform_view.buffers.index_state()
        }
    }

    fn navigation_from_cursors(cursors: &Vec<CursorView>) -> ui::Navigation {
        let mut output = d!();
    
        for c in cursors.iter() {
            use platform_types::{CursorState, Move};
            match c.state {
                CursorState::None => {}
                CursorState::PressedAgainstWall(dir) => match dir {
                    Move::Up => {
                        output = ui::Navigation::Up;
                        break;
                    }
                    Move::Down => {
                        output = ui::Navigation::Down;
                        break;
                    }
                    _ => {}
                },
            }
        }
    
        output
    }
}
pub use view::{View, LocalMenuView, MenuMode, WimpMenuMode, MenuView, WimpMenuView, FindReplaceMode};

/// State owned by the `run` function, which can be uniquely borrowed by other functions called inside `run`.
#[derive(Debug)]
pub struct RunState {
    pub view: View,
    pub cmds: VecDeque<Cmd>,
    pub ui: ui::State,
    pub buffer_status_map: BufferStatusMap,
    pub editor_in_sink: std::sync::mpsc::Sender<Input>,
    pub dimensions: Dimensions,
    pub event_proxy: EventLoopProxy<CustomEvent>, 
    pub clipboard: Clipboard,
}

pub type CommandKey = (ModifiersState, VirtualKeyCode);

pub mod command_keys {
    use super::{CommandKey, ModifiersState, VirtualKeyCode};
    const CTRL: ModifiersState = ModifiersState::CTRL;
    const SHIFT: ModifiersState = ModifiersState::SHIFT;
    const ALT: ModifiersState = ModifiersState::ALT;
    const LOGO: ModifiersState = ModifiersState::LOGO;

    pub fn command_menu() -> CommandKey {
        (ModifiersState::empty(), VirtualKeyCode::Apps)
    }
    
    pub fn debug_menu() -> CommandKey {
        (CTRL | SHIFT, VirtualKeyCode::Slash)
    }

    pub fn add_run_state_snapshot() -> CommandKey {
        (CTRL | SHIFT, VirtualKeyCode::F1)
    }
}



pub struct LabelledCommand {
    pub label: &'static str, 
    pub command: fn(&mut RunState),
}

impl std::fmt::Debug for LabelledCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("LabelledCommand")
           .field("label", &self.label)
           .field("command", &"fn(&mut RunState)")           .finish()
    }
}

pub type CommandsMap = BTreeMap<CommandKey, LabelledCommand>;

#[derive(Debug)]
/// Values that should not be changed (i.e. should be left constant,) whihc were lazily initialized by the `run`
/// function, which can be shared by other functions called inside `run`. Keeping this separate from `RunState`
/// also simplifies some borrow checking when using the `commands` map.
pub struct RunConsts {
    pub commands: CommandsMap
}

pub mod ui {
    use super::*;
    use macros::{fmt_debug};

    /// The varaints here represent sections of code that want to be able to store information in the
    /// ids. For example, so that the ui state can change differently based on which part of some
    /// dynamically generated UI is selected.
    #[derive(Clone, Copy, Debug)]
    pub enum Tag {
        FileSwitcherResults,
    }

    /// 31 to leave space for the enum variant tag.
    pub const DATA_LEN: usize = 31;

    /// The payload of the `UUId::Data` variant
    type Data = [u64; DATA_LEN];

    // This is probably excessive size-wise. We can make this smaller if there is a measuarable
    // perf impact but given this goes on the stack, that seems unlikely?
    #[derive(Clone, Copy)]
    pub enum Id {
        /// The generic data variant. Used when the data's sizes are not known ahead of time
        Data(Data),
        TaggedUsize(Tag, usize),
    }
    d!(for Id: Id::Data(d!()));

    fmt_debug!(for Id: id in "{}", {
        match id {
            Id::Data(data) => {
                let mut s = String::with_capacity(31 * std::mem::size_of::<u64>());

                'outer: for n in data.iter() {
                    let bytes = n.to_be_bytes();
                    for &byte in bytes.iter() {
                        if byte == 0 {
                            break 'outer;
                        }
                        s.push(byte as char);
                    }
                }

                if s.len() == 0 {
                    "\"\"".to_string()
                } else {
                    s
                }
            },
            Id::TaggedUsize(tag, payload) => {
                format!("TaggedUsize{:?}", (tag, payload))
            }
        }
    });
    ord!(and friends for Id: id, other in {
        use Id::*;
        use std::cmp::Ordering::*;
        match (id, other) {
            (Data(_), TaggedUsize(_, _)) => {
                Less
            },
            (TaggedUsize(_, _), Data(_)) => {
                Greater
            },
            (Data(d1), Data(d2)) => {
                d1.cmp(&d2)
            }
            (TaggedUsize(Tag::FileSwitcherResults, payload1), TaggedUsize(Tag::FileSwitcherResults, payload2)) => {
                payload1.cmp(&payload2)
            }
        }
    });

    impl Id {
        pub const fn new(id: Data) -> Self {
            Id::Data(id)
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum PhysicalButtonState {
        Released,
        Pressed,
        ReleasedThisFrame,
        PressedThisFrame,
    }
    ord!(and friends for PhysicalButtonState: s, other in {
        use PhysicalButtonState::*;
        let s = match s {
            Released => 0,
            Pressed => 1,
            ReleasedThisFrame => 2,
            PressedThisFrame => 3,
        };
    
        let other = match other {
            Released => 0,
            Pressed => 1,
            ReleasedThisFrame => 2,
            PressedThisFrame => 3,
        };
    
        s.cmp(&other)
    });
    
    d!(for PhysicalButtonState: PhysicalButtonState::Released);
    
    impl PhysicalButtonState {
        fn decay(&mut self) {
            *self = match *self {
                Self::ReleasedThisFrame => Self::Released,
                Self::PressedThisFrame => Self::Pressed,
                other => other,
            }
        }
    
        pub fn is_pressed(&self) -> bool {
            match *self {
                Self::ReleasedThisFrame | Self::Released => false,
                Self::PressedThisFrame | Self::Pressed => true,
            }
        }
    }

    #[derive(Debug, Default)]
    pub struct ListPosition {
        pub index: usize,
        pub scroll: f32,
    }

    #[derive(Debug, Default)]
    pub struct State {
        pub mouse_pos: ScreenSpaceXY,
        pub left_mouse_state: PhysicalButtonState,
        pub enter_key_state: PhysicalButtonState,
        pub tab_scroll: f32,
        pub file_switcher_pos: ListPosition,
        pub command_menu_pos: ListPosition,
        /// This is should be in the range [0.0, 2.0]. This needs the extra space to repesent the down
        /// side of the sawtooth pattern.
        pub fade_alpha_accumulator: f32,
        // If the user has recently made or is making an input, we don't want a distracting animation
        // during that time. Afterwards though, we do want the animation to start again.
        pub fade_solid_override_accumulator: f32,
        pub mouse: Focus,
        pub keyboard: Focus,
        pub navigation: Navigation,
        pub window_is_focused: bool,
    }
    
    impl State {
        pub fn note_interaction(&mut self) {
            self.fade_solid_override_accumulator = 1.5;
        }
        pub fn add_dt(&mut self, dt: std::time::Duration) {
            let offset = ((dt.as_millis() as u64 as f32) / 1000.0) * 1.5;
    
            if self.fade_solid_override_accumulator > 0.0 {
                self.fade_solid_override_accumulator -= offset;
                if self.fade_solid_override_accumulator < 0.0 {
                    self.fade_solid_override_accumulator = 0.0;
                    // Make sure when the override ends that we don't jump to a random point in the
                    // blink
                    self.fade_alpha_accumulator = 1.0;
                }
            } else {
                self.fade_alpha_accumulator += offset;
                self.fade_alpha_accumulator = self.fade_alpha_accumulator.rem_euclid(2.0);
            }
        }
        pub fn get_fade_alpha(&self) -> f32 {
            if self.fade_solid_override_accumulator > 0.0 {
                1.0
            } else if self.fade_alpha_accumulator > 1.0 {
                2.0 - self.fade_alpha_accumulator
            } else {
                self.fade_alpha_accumulator
            }
        }
    }
    
    impl State {
        pub fn frame_init(&mut self) {
            self.mouse.frame_init();
            self.keyboard.frame_init();
        }
        pub fn frame_end(&mut self) {
            // This needs to go here instead of in init, so that we actually see the undecayed state
            // for the first frame after the input event.
            self.left_mouse_state.decay();
            self.enter_key_state.decay();
        }
    }
    
    #[derive(Debug, Default)]
    pub struct Focus {
        pub active: Id,
        pub hot: Id,
        pub next_hot: Id,
    }
    
    impl Focus {
        pub fn set_not_active(&mut self) {
            self.active = d!();
        }
        pub fn set_active(&mut self, id: Id) {
            self.active = id;
        }
        pub fn set_next_hot(&mut self, id: Id) {
            self.next_hot = id;
        }
        #[allow(dead_code)]
        pub fn set_not_hot(&mut self) {
            self.hot = d!();
        }
        pub fn frame_init(&mut self) {
            if self.active == d!() {
                self.hot = self.next_hot;
            }
            self.next_hot = d!();
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum InputType {
        Mouse,
        Keyboard,
        Both,
    }
    
    macro_rules! input_type_tag {
        ($input_type: expr) => {{
            use InputType::*;
            match $input_type {
                Mouse => 1,
                Keyboard => 2,
                Both => 3,
            }
        }};
    }
    
    ord!(and friends for InputType: t, other in input_type_tag!(t).cmp(&input_type_tag!(other)));
    
    #[derive(Clone, Copy, Debug)]
    pub enum ButtonState {
        Usual,
        Hover(InputType),
        Pressed(InputType),
    }
    ord!(and friends for ButtonState: s, other in {
        use ButtonState::*;
        macro_rules! button_state_tag {
            ($button_state: expr) => (
                match $button_state {
                    Usual => (0, 0),
                    Hover(input_type) => (1, input_type_tag!(input_type)),
                    Pressed(input_type) => (2, input_type_tag!(input_type)),
                }
            );
        }
    
        button_state_tag!(s).cmp(&button_state_tag!(other))
    });
    
    type DoButtonResult = (bool, ButtonState);
    
    /// calling this once will swallow multiple clicks on the button. We could either
    /// pass in and return the number of clicks to fix that, or this could simply be
    /// called multiple times per frame (once for each click).
    pub fn do_button_logic(ui: &mut ui::State, id: ui::Id, rect: ScreenSpaceRect) -> DoButtonResult {
        use ButtonState::*;
        let mut clicked = false;
    
        let mouse_pos = ui.mouse_pos;
        let mouse_state = ui.left_mouse_state;
        let enter_key_state = ui.enter_key_state;
    
        let inside = inside_rect(mouse_pos, rect);
    
        if ui.mouse.active == id {
            if mouse_state == PhysicalButtonState::ReleasedThisFrame {
                clicked = ui.mouse.hot == id && inside;
                ui.mouse.set_not_active();
            }
        } else if ui.keyboard.active == id {
            if enter_key_state == PhysicalButtonState::PressedThisFrame {
                clicked = ui.keyboard.hot == id;
                ui.keyboard.set_not_active();
            }
        } else {
            if ui.mouse.hot == id {
                if mouse_state == PhysicalButtonState::PressedThisFrame {
                    ui.mouse.set_active(id);
                }
            }
    
            if ui.keyboard.hot == id {
                if enter_key_state == PhysicalButtonState::PressedThisFrame {
                    ui.keyboard.set_active(id);
                }
            }
        }
    
        if inside {
            ui.mouse.set_next_hot(id);
        }
        // keyboard_hot is expected to be set by other ui code, since it depends on what that code
        // wants to allow regarding movement.
    
        let state = match (
            ui.mouse.active == id && mouse_state.is_pressed(),
            ui.keyboard.active == id && enter_key_state.is_pressed(),
        ) {
            (true, true) => Pressed(InputType::Both),
            (true, false) => Pressed(InputType::Mouse),
            (false, true) => Pressed(InputType::Keyboard),
            (false, false) => match (ui.mouse.hot == id, ui.keyboard.hot == id) {
                (true, true) => Hover(InputType::Both),
                (true, false) => Hover(InputType::Mouse),
                (false, true) => Hover(InputType::Keyboard),
                (false, false) => Usual,
            },
        };
        (clicked, state)
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Navigation {
        None,
        Up,
        Down,
        Interact,
    }
    d!(for Navigation: Navigation::None);
    
    pub fn begin_view(ui: &mut State, view: &View) {
        if let Some(navigation) = view.get_fresh_navigation() {
            ui.navigation = navigation;
        } else {
            // use the navigation that was set before `view` was called if there was one.
        }
    }
    
    pub fn end_view(ui: &mut State) {
        ui.navigation = d!();
    }
}

/// This macro creates a ui::Id based on the expression passed in and the location of the invocation
/// in the file. This implies it may assign the same id to multiple `id` invocations inside another
/// macro. A suggested fix for that is to pass down the needed ids from outside that macro.
#[macro_export]
macro_rules! ui_id {
    () => {{
        ui_id!(0xFFFF_FFFF_FFFF_FFFF)
    }};
    ($thing: expr) => {{
        let mut id = [0; ui::DATA_LEN];
        // TODO is the compilier smart enough to avoid the allocation here?
        let s = format!(
            "{},{}",
            $thing,
            concat!(column!(), ",", line!(), ",", file!())
        );
        let slice = s.as_bytes();

        let mut i = 0;
        for chunk in slice.chunks_exact(8) {
            let mut value = 0u64;
            value |= (chunk[0] as u64) << 56;
            value |= (chunk[1] as u64) << 48;
            value |= (chunk[2] as u64) << 40;
            value |= (chunk[3] as u64) << 32;
            value |= (chunk[4] as u64) << 24;
            value |= (chunk[5] as u64) << 16;
            value |= (chunk[6] as u64) << 8;
            value |= (chunk[7] as u64) << 0;
            id[i] = value;
            i += 1;
            if i >= id.len() {
                break;
            }
        }

        // use new so we can use this macro outside this package.
        ui::Id::new(id)
    }};
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferStatus {
    /// The copy on disk at the buffer's path is the same as in memory. Note that we don't
    /// distinguish whether the temp file matches or not. This is becaue the difference would only
    /// let us skip some writes to the temp files, and we currently just write those out every time.
    Unedited,
    /// The copy on disk at the buffer's path is different than in memory but the temp file is the same as in memory
    EditedAndSaved,
    /// The copy on disk at the buffer's path and the temp file are both different than what is in memory
    EditedAndUnSaved,
}
d!(for BufferStatus: BufferStatus::Unedited);

/// A representaion of how a `BufferStatus` can change. Having this be a separate data type allows
/// us to keep all of the reading of a `BufferStatusMap` on a single thread.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferStatusTransition {
    /// The copy in memory has changed, so we know that it may not match the what is on disk any longer.
    Edit,
    /// The temp file on disk has been updated to match what is in memory.
    SaveTemp,
    /// The copy on disk at the buffer's path has been updated to match what is in memory.
    Save,
}

impl From<EditedTransition> for BufferStatusTransition {
    fn from(e_t: EditedTransition) -> Self {
        u!{BufferStatusTransition};
        u!{EditedTransition};
        match e_t {
            ToEdited => Edit,
            ToUnedited => Save,
        }
    }
}

pub fn transform_status(status: BufferStatus, transition: BufferStatusTransition) -> BufferStatus {
    u!{BufferStatus}
    u!{BufferStatusTransition}
    match (status, transition) {
        (_, Edit) => EditedAndUnSaved,
        (_, Save) | (Unedited, SaveTemp) => Unedited,
        (EditedAndUnSaved, SaveTemp) | (EditedAndSaved, SaveTemp) => EditedAndSaved,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BufferStatusMap {
    map: HashMap<usize, BufferStatus>,
    last_state: Option<g_i::State>,
}

impl BufferStatusMap {
    pub fn with_capacity(capacity: usize) -> Self {
        BufferStatusMap {
            map: HashMap::with_capacity(capacity),
            last_state: d!(),
        }
    }

    pub fn get(&self, state: g_i::State, index: g_i::Index) -> Option<BufferStatus> {
        index.get(state).and_then(|i| self.map.get(&i).cloned())
    }

    pub fn insert(&mut self, state: g_i::State, current_index: g_i::Index, status: BufferStatus) {
        if let Some(current_index) = current_index.get(state) {
            let last_state = self.last_state;
            if Some(state) != last_state {
                let mut keys: Vec<_> = self.map.keys().cloned().collect();
                //currently all the state fixups work if we use this reverse order.
                keys.sort();
                keys.reverse();
                for key in keys {
                    if let Some(i) = last_state.and_then(|s| {
                        state
                            .migrate(s.new_index(g_i::IndexPart::or_max(key)))
                            .and_then(|i| i.get(state))
                    }) {
                        let status = self.map.remove(&i).unwrap_or_default();
                        self.map.insert(i, status);
                    } else {
                        self.map.remove(&key);
                    }
                }

                self.last_state = Some(state);
            }

            self.map.insert(current_index, status);
        }
    }

    pub fn transform_at(
        &mut self,
        index_state: g_i::State,
        index: g_i::Index,
        transition: BufferStatusTransition
    ) {
        self.insert(
            index_state,
            index,
            transform_status(
                self
                    .get(index_state, index)
                    .unwrap_or_default(),
                transition
            )
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn state_at_generation(generation: g_i::Generation) -> g_i::State {
        let mut s: g_i::State = d!();
        for _ in 0..generation {
            s.removed_at_index_part(g_i::IndexPart::or_max(usize::max_value()));
        }
        s
    }

    #[test]
    fn buffer_status_map_get_works_on_this_example_with_multiple_generations() {
        let state_0: g_i::State = d!();
        let state_1 = {
            let mut s = state_0.clone();
            s.removed_at_index_part(g_i::IndexPart::or_max(2));
            s
        };
        let state_2 = {
            let mut s = state_1.clone();
            s.removed_at_index_part(g_i::IndexPart::or_max(usize::max_value()));
            s
        };
        let len = 5;
        // In the map below, we simulate a map that was like this at generation 0:
        // [status_pre_2, status_pre_2, status_at_2, status_at_2, status_post_2, status_post_2]
        // and in generation 1 it became this:
        // [status_pre_2, status_pre_2, status_at_2, status_post_2, status_post_2]
        let status_pre_2 = BufferStatus::Unedited;
        let status_at_2 = BufferStatus::EditedAndSaved;
        let status_post_2 = BufferStatus::EditedAndUnSaved;

        let mut map = BufferStatusMap::with_capacity(len);
        // this test assumes that the state passed "matches" the maps state, so in this case,
        // always `state_1`. Note the index state is varied. We use state_1 so we can check what
        // happens above, at, and after the current state. Similarly we care about index 2 so we
        // have examples before and after it.
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(0)),
            status_pre_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(1)),
            status_pre_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(2)),
            status_at_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(3)),
            status_post_2,
        );
        map.insert(
            state_1,
            state_1.new_index(g_i::IndexPart::or_max(4)),
            status_post_2,
        );

        macro_rules! _0_to_5_get_assert {
            ($index_state: expr =>
                $ex0: expr,
                $ex1: expr,
                $ex2: expr,
                $ex3: expr,
                $ex4: expr,
             ) => {
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(0))),
                    $ex0,
                    "$ex0"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(1))),
                    $ex1,
                    "$ex1"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(2))),
                    $ex2,
                    "$ex2"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(3))),
                    $ex3,
                    "$ex3"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(4))),
                    $ex4,
                    "$ex4"
                );
            };
        }

        _0_to_5_get_assert! {
            state_0
            =>
            Some(status_pre_2),
            Some(status_pre_2),
            None,
            Some(status_at_2),
            Some(status_post_2),
        };

        _0_to_5_get_assert! {
            state_1
            =>
            Some(status_pre_2),
            Some(status_pre_2),
            Some(status_at_2),
            Some(status_post_2),
            Some(status_post_2),
        }

        _0_to_5_get_assert! {
            state_2
            =>
            None,
            None,
            None,
            None,
            None,
        }
    }

    #[test]
    fn buffer_status_map_insert_clears_away_old_generations() {
        let old_state = d!();
        let new_state = state_at_generation(3);
        let status = BufferStatus::EditedAndUnSaved;
        let len = 16;

        let mut map = BufferStatusMap::with_capacity(len);

        for i in 0..len {
            map.insert(
                old_state,
                old_state.new_index(g_i::IndexPart::or_max(i)),
                status,
            );
        }

        // precondition
        assert_eq!(map.map.len(), len);

        map.insert(
            new_state,
            new_state.new_index(g_i::IndexPart::or_max(0)),
            status,
        );

        assert_eq!(map.map.len(), 1);
    }
}

