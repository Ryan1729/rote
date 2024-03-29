use window_layer::{ModifiersState, KeyCode};
use macros::{d, ord, u};
use platform_types::{screen_positioning::*, abs, g_i, Input, Cmd, EditedTransition, TimeSpan, BufferLabel, BufferName};

use std::collections::{VecDeque, BTreeMap};
use std::path::PathBuf;
use std::cmp::min;

pub use clipboard::{get_clipboard, Clipboard};

// Parts of RunState that represent externally chosen, absolute dimensions of things,
// including the window, from which the sizes of several UI elements are derived.
#[derive(Clone, Copy, Debug)]
pub struct Dimensions {
    pub window: window_layer::Dimensions,
    pub hidpi_factor_override: Option<DpiFactor>,
    pub current_hidpi_factor: DpiFactor,
    pub font: FontInfo,
}

impl Default for Dimensions {
    fn default() -> Self {
        Self {
            window: window_layer::Dimensions{
                width: abs::Length::from(1024),
                height: abs::Length::from(768),
            },
            hidpi_factor_override: None,
            current_hidpi_factor: 1.,
            font: d!(),
        }
    }
}

#[derive(Debug)]
pub struct BufferInfo {
    pub name: BufferName,
    pub name_string: String,
    pub chars: String,
    pub status: BufferStatus,
}

#[derive(Debug)]
pub enum EditedFilesThread {
    Quit,
    Buffers(g_i::State, Vec<BufferInfo>),
}

pub type PID = u32;

#[derive(Clone, Debug)]
pub enum PidKind {
    PathMailbox,
}

#[derive(Clone, Debug)]
pub enum CustomEvent {
    OpenFile(PathBuf),
    SaveNewFile(PathBuf, g_i::Index),
    SendBuffersToBeSaved,
    EditedBufferError(String),
    Pid(PidKind, PID),
    MarkBufferStatusTransition(PathBuf, g_i::Index, BufferStatusTransition),
}

/// This module exists because when adding WIMP only UI elements we found that
/// we wanted to have several parts of the code start checking the whether a
/// WIMP only menu was up. Because I was not sure where all the parts of the code
/// that were that were talking directly to the `platform_types::View` and therefore
/// might need to be changed, I wanted to lean on the compiler to find all those
/// places for me.
mod view {
    use macros::{d};
    use super::ui; // Your app's written in Electron? Shoulda used Super UI.
    use super::g_i;
    use platform_types::{CursorView, BufferViewData, MenuView, IndexedEditedTransition};

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
    pub enum LocalMenu {
        Command,
        Debug,
    }

    #[derive(Clone, Debug)]
    pub struct WimpMenu<'view> {
        pub platform_menu: &'view MenuView,
        pub local_menu: &'view Option<LocalMenu>,
    }

    impl WimpMenu<'_> {
        #[must_use]
        pub fn get_mode(&self) -> WimpMenuMode {
            match self.local_menu {
                Some(LocalMenu::Command) => WimpMenuMode::Command,
                Some(LocalMenu::Debug) => WimpMenuMode::Debug,
                None => self.platform_menu.get_mode().into()
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
    // This struct hides the platform::View, but allows borrowing the buffers
    // disjointly, which helps rustc figure out some ownership things.
    pub struct Buffers {
        // We want to hide the `platform_types::View` from the rest of the code
        // because we had a bug when LocalMenu was introduced that hiding
        // the `platform_types::View` prevents.
        platform_view: platform_types::View
    }

    impl Buffers {
        pub fn buffer_iter(&self) -> impl Iterator<Item = (g_i::Index, &platform_types::BufferLabel)> {
            self.platform_view.buffers.iter_with_indexes()
        }
    }

    #[derive(Default, Debug)]
    pub struct View {
        pub buffers: Buffers,
        pub local_menu: Option<LocalMenu>,
        pub scratch: Scratch,
    }

    // TODO Maybe replace this with a bump allocated arena that is cleared each
    // render?
    #[derive(Clone, Default, Debug, PartialEq)]
    pub struct Scratch {
        pub buffer_view_data: BufferViewData
    }

    macro_rules! toggle_impl {
        ($view: expr,  $toggled: path $(,)?) => {
            match $view.local_menu {
                None => match $view.buffers.platform_view.menu {
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
            self.buffers.platform_view.menu = platform_types::MenuView::None;
        }

        pub fn toggle_command_menu(&mut self) {
            toggle_impl!{
                self,
                LocalMenu::Command,
            }
        }

        pub fn toggle_debug_menu(&mut self) {
            toggle_impl!{
                self,
                LocalMenu::Debug,
            }
        }

        pub fn update(
            &mut self,
            p_view: platform_types::View,
            buffer_view_data: BufferViewData,
        ) {
            self.buffers.platform_view = p_view;
            self.scratch.buffer_view_data = buffer_view_data;
        }
    }

    impl View {
        pub fn edited_transitions(&self) -> impl Iterator<Item = IndexedEditedTransition> {
            self.buffers.platform_view.edited_transitions.clone().into_iter()
        }

        #[must_use]
        #[perf_viz::record]
        pub fn buffers_count(&self) -> g_i::Length {
            self.buffers.platform_view.buffers.len()
        }

        pub fn buffer_iter(&self) -> impl Iterator<Item = (g_i::Index, &platform_types::BufferLabel)> {
            self.buffers.buffer_iter()
        }

        #[must_use]
        fn get_selected_cursors(&self) -> Option<&[CursorView]> {
            if self.local_menu.is_none() {
                return self.buffers.platform_view.get_selected_cursors();
            }
            None
        }

        #[must_use]
        pub fn get_navigation(&self) -> Option<ui::Navigation> {
            self.get_selected_cursors()
                .map(navigation_from_cursors)
        }

        #[must_use]
        pub fn current_buffer_kind(&self) -> platform_types::BufferIdKind {
            self.buffers.platform_view.current_buffer_kind
        }

        #[must_use]
        pub fn current_buffer_id(&self) -> platform_types::BufferId {
            self.buffers.platform_view.current_buffer_id()
        }

        #[must_use]
        pub fn current_text_index(&self) -> g_i::Index {
            self.buffers.platform_view.current_text_index()
        }

        #[must_use]
        pub fn current_text_index_and_buffer_label(&self) -> (g_i::Index, &platform_types::BufferLabel) {
            self.buffers.platform_view.current_text_index_and_buffer_label()
        }

        #[must_use]
        pub fn get_buffer_label(&self, index: g_i::Index) -> Option<&platform_types::BufferLabel> {
            self.buffers.platform_view.get_buffer_label(index)
        }

        #[must_use]
        pub fn current_path(&self) -> Option<std::path::PathBuf> {
            self.buffers.platform_view.current_path()
        }

        #[must_use]
        pub fn stats(&self) -> &platform_types::ViewStats {
            &self.buffers.platform_view.stats
        }

        #[must_use]
        pub fn menu(&self) -> WimpMenu {
            WimpMenu {
                platform_menu: &self.buffers.platform_view.menu,
                local_menu: &self.local_menu,
            }
        }

        #[must_use]
        pub fn menu_mode(&self) -> WimpMenuMode {
            self.menu().get_mode()
        }

        #[must_use]
        pub fn status_line(&self) -> &platform_types::StatusLineView {
            &self.buffers.platform_view.status_line
        }

        #[must_use]
        pub fn index_state(&self) -> g_i::State {
            self.buffers.platform_view.buffers.index_state()
        }
    }

    fn navigation_from_cursors(cursors: &[CursorView]) -> ui::Navigation {
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
pub use view::{View, LocalMenu, WimpMenuMode, WimpMenu, FindReplaceMode};

#[derive(Copy, Clone, Debug, Default)]
/// Process Ids for the different threads. As of this writing, only used to display
/// as a debugging aid.
pub struct Pids {
    pub window: PID,
    pub editor: PID,
    pub path_mailbox: PID,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Stats {
    pub latest_view_function_time_span: TimeSpan
}

#[derive(Debug)]
pub enum EditorThreadInput {
    Render(Input),
    //LoadBuffers(&[BufferName]),
    SaveBuffers(g_i::State, Vec<BufferName>, Vec<BufferStatus>),
    SaveToDisk(PathBuf, BufferLabel, g_i::Index),
}

/// Info displayed on the debug menu
#[derive(Debug, Default)]
pub struct DebugMenuState {
    // We don't want to be allocating a string every frame. So instead, we clear 
    // this one and write the data into it.
    pub preallocated_scratch: String,
    pub startup_description: String,
    pub pids: Pids,
    pub editor_buffers_size_in_bytes: usize,
    pub last_hidpi_factors: [window_layer::ScaleFactor; 4],
    pub status_line_rect: ScreenSpaceRect,
    pub mouse_pos: ScreenSpaceXY,
    pub window: window_layer::Dimensions,
}

impl DebugMenuState {
    pub fn render_to_scratch(&mut self) {
        use std::fmt::Write;
        let output = &mut self.preallocated_scratch;
        output.clear();

        // TODO human readable size
        let _cannot_actually_fail = writeln!(
            output,
            "buffers bytes: {}",
            self.editor_buffers_size_in_bytes,
        );

        output.push_str(&self.startup_description);
        output.push('\n');

        let _cannot_actually_fail = writeln!(
            output,
            "last {} DPI factors:",
            self.last_hidpi_factors.len(),
        );

        for factor in &self.last_hidpi_factors {
            let _cannot_actually_fail = writeln!(
                output,
                "{}",
                factor,
            );
        }

        let _cannot_actually_fail = writeln!(
            output,
            "window: {}",
            self.window,
        );

        let _cannot_actually_fail = writeln!(
            output,
            "{}\nin {}?: {}",
            self.mouse_pos,
            self.status_line_rect,
            inside_rect(self.mouse_pos, self.status_line_rect)
        );

        #[cfg(all(unix, feature = "libc-getrusage"))]
        {
            perf_viz::record_guard!("getrusage");

            // SAFETY: The type `libc::rusage` contains just Plain Old Data; no 
            // pointers, and certainly no references.
            let mut rusage: libc::rusage = unsafe{ std::mem::zeroed() };
            // SAFETY: We pass a correct set of params:
            // * We pass `libc::RUSAGE_SELF`, a constant that the fn is documented
            //   to expect.
            // * We pass a pointer to a proprly zeroed `libc::rusage` struct
            //   for `libc::getrusage`'s out param.
            if unsafe { libc::getrusage(
                libc::RUSAGE_SELF,
                &mut rusage as _
            ) } == 0 {
                let _cannot_actually_fail = write!(
                    output,
                    "Resource Usage:\n\
                    User CPU time   (s): {}.{:06}\n\
                    System CPU time (s): {}.{:06}\n\
                    Maximum resident set size (B): {}\n\
                    non-I/O page faults: {}\n\
                    I/O page faults    : {}\n\
                    voluntary contex switches  : {}\n\
                    involuntary contex switches: {}\n\
                    ",
                    rusage.ru_utime.tv_sec,
                    rusage.ru_utime.tv_usec,
                    rusage.ru_stime.tv_sec,
                    rusage.ru_stime.tv_usec,
                    // This is in kilobytes by default.
                    rusage.ru_maxrss.saturating_mul(1024),
                    rusage.ru_minflt,
                    rusage.ru_majflt,
                    rusage.ru_nvcsw,
                    rusage.ru_nivcsw,
                );
            } else {
                let error = std::io::Error::last_os_error();

                let _cannot_actually_fail = write!(
                    output,
                    "getrusage error\n{error}",
                );
            }
        }

        macro_rules! push_pid_line {
            ($field_name: ident) => {{
                let field_name = stringify!($field_name);
                for _ in 0..(16usize.saturating_sub(field_name.len())) {
                    output.push(' ');
                }

                output.push_str(field_name);
                let _cannot_actually_fail = writeln!(output, " PID: {}", self.pids.$field_name);
            }}
        }

        push_pid_line!(window);
        push_pid_line!(editor);
        push_pid_line!(path_mailbox);
    }
}

/// The subset of `RunState` that is relevant to rendering the view.
#[derive(Debug, Default)]
pub struct ViewRunState {
    pub view: View,
    pub ui: ui::State,
    pub buffer_status_map: g_i::Map<BufferStatus>,
    pub dimensions: Dimensions,
    pub debug_menu_state: DebugMenuState,
    pub stats: Stats,
}

// TODO use the ScaleFactor name everywhere.
pub type DpiFactor = window_layer::ScaleFactor;

/// State owned by the `run` function, which can be uniquely borrowed by other functions called inside `run`.
#[derive(Debug)]
pub struct RunState {
    pub view_state: ViewRunState,
    pub cmds: VecDeque<Cmd>,
    pub editor_in_sink: std::sync::mpsc::Sender<EditorThreadInput>,
    pub event_proxy: window_layer::EventLoopProxy<CustomEvent>,
    pub clipboard: Clipboard,
}

pub type CommandKey = (ModifiersState, KeyCode);

pub mod command_keys {
    use super::{CommandKey, ModifiersState, KeyCode};
    pub const CTRL: ModifiersState = ModifiersState::CTRL;
    pub const SHIFT: ModifiersState = ModifiersState::SHIFT;
    pub const ALT: ModifiersState = ModifiersState::ALT;
    pub const LOGO: ModifiersState = ModifiersState::LOGO;

    #[must_use]
    pub fn command_menu() -> CommandKey {
        (ModifiersState::empty(), KeyCode::Apps)
    }

    #[must_use]
    pub fn debug_menu() -> CommandKey {
        (CTRL | SHIFT, KeyCode::Slash)
    }

    #[must_use]
    pub fn add_run_state_snapshot() -> CommandKey {
        (CTRL | SHIFT, KeyCode::F1)
    }
}

pub type CommandFn = fn(&mut RunState, &mut window_layer::Fns);

pub struct LabelledCommand {
    pub label: &'static str,
    pub command: CommandFn,
}

impl std::fmt::Debug for LabelledCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("LabelledCommand")
           .field("label", &self.label)
           .field("command", &"CommandFn")
           .finish()
    }
}

pub type CommandsMap = BTreeMap<CommandKey, LabelledCommand>;

#[derive(Debug, Default)]
/// Values that should not be changed (i.e. should be left constant,) which were
/// lazily initialized by the `run` function, which can be shared by other functions
/// called inside `run`. Keeping this separate from `RunState` also simplifies some
/// borrow checking when using the `commands` map.
pub struct RunConsts {
    pub commands: CommandsMap
}

pub mod ui {
    use super::*;
    use macros::{fmt_debug};

    /// The varaints here represent sections of code that want to be able to store information in the
    /// ids. For example, so that the ui state can change differently based on which part of some
    /// dynamically generated UI is selected.
    #[derive(Clone, Copy, Debug, Ord, PartialOrd, PartialEq, Eq)]
    pub enum Tag {
        FileSwitcherResults,
        CommandMenu,
    }

    /// 31 to leave space for the enum variant tag.
    pub const DATA_LEN: usize = 31;

    /// The payload of the `UUId::Data` variant
    type Data = [u64; DATA_LEN];

    // This is probably excessive size-wise. We can make this smaller if there is a
    // noticable perf impact but given this goes on the stack, that seems unlikely?
    // TODO Try `Box`ing `Data`, and measure the difference.
    #[allow(clippy::large_enum_variant)]
    #[derive(Clone, Copy, Ord, PartialOrd, PartialEq, Eq)]
    pub enum Id {
        /// The generic data variant. Used when the data's sizes are not known ahead of time
        Data(Data),
        TaggedListSelection(Tag, ListSelection),
    }
    d!(for Id: Id::Data(d!()));

    fmt_debug!(for Id: id in "{}", {
        match id {
            Id::Data(data) => {
                let mut s = String::with_capacity(31 * std::mem::size_of::<u64>());

                'outer: for n in data.iter() {
                    let bytes = n.to_be_bytes();
                    for &byte in &bytes {
                        if byte == 0 {
                            break 'outer;
                        }
                        s.push(byte as char);
                    }
                }

                if s.is_empty() {
                    "\"\"".to_string()
                } else {
                    s
                }
            },
            Id::TaggedListSelection(tag, payload) => {
                format!("TaggedListSelection{:?}", (tag, payload))
            }
        }
    });

    impl Id {
        #[must_use]
        pub const fn new(id: Data) -> Self {
            Id::Data(id)
        }
    }

    #[derive(Clone, Copy, Debug, Ord, PartialOrd, PartialEq, Eq, Default)]
    pub struct ListSelection {
        pub index: usize,
        pub window_start: usize
        // Since we are unlikely to need a window as large as 256, this could be
        // phrased as the following if the memory used became an issue. That seems
        // unlikely at the moment though.
        // pub window_start: usize
        // pub intra_window_offset: u8
    }

    pub type ListSelectionWindowSize = core::num::NonZeroUsize;

    impl ListSelection {
        #[must_use]
        pub fn move_up(self) -> Self {
            let index = self.index.saturating_sub(1);
            Self {
                index,
                window_start: min(index, self.window_start)
            }
        }

        #[must_use]
        pub fn move_down(self, window_size: ListSelectionWindowSize, length: usize) -> Self {
            let index = self.index.saturating_add(1);

            if index >= length {
                d!()
            } else {
                let pivot = self.window_start + (window_size.get() / 2);
                Self {
                    index,
                    window_start: if index > pivot {
                        self.window_start + 1
                    } else {
                        self.window_start
                    }
                }
            }
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

        #[must_use]
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
        pub tab_scroll: abs::Vector,
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
        pub fresh_navigation: Navigation,
        pub window_is_focused: bool,
    }

    #[cfg(not(feature = "disable-fade-alpha"))]
    impl State {
        pub fn note_interaction(&mut self) {
            self.fade_solid_override_accumulator = 1.5;
        }

        #[perf_viz::record]
        pub fn add_dt(&mut self, dt: std::time::Duration) {
            let offset = dt.as_secs_f32() * 1.5;

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

        #[must_use]
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

    #[cfg(feature = "disable-fade-alpha")]
    impl State {
        pub fn note_interaction(&mut self) {

        }
        #[perf_viz::record]
        pub fn add_dt(&mut self, _: std::time::Duration) {

        }
        pub fn get_fade_alpha(&self) -> f32 {
            1.0
        }
    }

    impl State {
        pub fn frame_init(&mut self, view: &View) {
            match self.keyboard.hot {
                ui::Id::TaggedListSelection(..) => {},
                ui::Id::Data(..) => {
                    match view.menu().get_mode() {
                        WimpMenuMode::FileSwitcher => {
                            if let Some(Navigation::Down) = view.get_navigation() {
                                self.keyboard.set_next_hot(ui::Id::TaggedListSelection(
                                    ui::Tag::FileSwitcherResults,
                                    d!()
                                ));
                            }
                        },
                        WimpMenuMode::Command => {
                            self.keyboard.set_next_hot(ui::Id::TaggedListSelection(
                                ui::Tag::CommandMenu,
                                d!()
                            ));
                        }
                        _ => {}
                    }
                }
            }

            self.mouse.frame_init();
            self.keyboard.frame_init();
            
            self.navigation = self.fresh_navigation;
            self.fresh_navigation = d!();
        }
        pub fn frame_end(&mut self) {
            // This needs to go here instead of in init, so that we actually see the undecayed state
            // for the first frame after the input event. {
            self.left_mouse_state.decay();
            self.enter_key_state.decay();
            // }

            self.fresh_navigation = d!();
            self.navigation = d!();
        }

        pub fn set_fresh_navigation(&mut self, fresh_navigation: Navigation) {
            self.fresh_navigation = fresh_navigation;
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
    #[perf_viz::record]
    pub fn do_button_logic(
        ui: &mut ui::State,
        id: ui::Id,
        rect: ScreenSpaceRect
    ) -> DoButtonResult {
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
            if ui.mouse.hot == id
            && mouse_state == PhysicalButtonState::PressedThisFrame {
                ui.mouse.set_active(id);
            }

            if ui.keyboard.hot == id
            && enter_key_state == PhysicalButtonState::PressedThisFrame {
                ui.keyboard.set_active(id);
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
}

/// This macro creates a `ui::Id` based on the expression passed in and the location
/// of the invocation in the file. This implies it may assign the same id to
/// multiple `id` invocations inside another macro. A suggested fix for that is to
/// pass down the needed ids from outside that macro.
#[macro_export]
macro_rules! ui_id {
    () => {{
        ui_id!(0xFFFF_FFFF_FFFF_FFFF)
    }};
    ($thing: expr) => {{
        let mut id = [0; ui::DATA_LEN];
        // TODO is the compiler smart enough to avoid the allocation here?
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

#[must_use]
pub fn transform_status(
    status: BufferStatus,
    transition: BufferStatusTransition
) -> BufferStatus {
    u!{BufferStatus}
    u!{BufferStatusTransition}
    match (status, transition) {
        (_, Edit) => EditedAndUnSaved,
        (_, Save) | (Unedited, SaveTemp) => Unedited,
        (EditedAndUnSaved | EditedAndSaved, SaveTemp) => EditedAndSaved,
    }
}

pub fn transform_at(
    map: &mut g_i::Map<BufferStatus>,
    index_state: g_i::State,
    index: g_i::Index,
    transition: BufferStatusTransition
) {
    let previous = map
        .get(index_state, index)
        .copied()
        .unwrap_or_default();

    map.insert(
        index_state,
        index,
        transform_status(
            previous,
            transition
        )
    );
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
        let len = g_i::Length::or_max(5);
        // In the map below, we simulate a map that was like this at generation 0:
        // [status_pre_2, status_pre_2, status_at_2, status_at_2, status_post_2, status_post_2]
        // and in generation 1 it became this:
        // [status_pre_2, status_pre_2, status_at_2, status_post_2, status_post_2]
        let status_pre_2 = BufferStatus::Unedited;
        let status_at_2 = BufferStatus::EditedAndSaved;
        let status_post_2 = BufferStatus::EditedAndUnSaved;

        let mut map = g_i::Map::with_capacity(len);
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
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(0))).cloned(),
                    $ex0,
                    "$ex0"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(1))).cloned(),
                    $ex1,
                    "$ex1"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(2))).cloned(),
                    $ex2,
                    "$ex2"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(3))).cloned(),
                    $ex3,
                    "$ex3"
                );
                assert_eq!(
                    map.get(state_1, $index_state.new_index(g_i::IndexPart::or_max(4))).cloned(),
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
        let len = g_i::Length::or_max(16);

        let mut map = g_i::Map::with_capacity(len);

        for i in 0usize..len.into() {
            map.insert(
                old_state,
                old_state.new_index(g_i::IndexPart::or_max(i)),
                status,
            );
        }

        // precondition
        assert_eq!(map.len(), len);

        map.insert(
            new_state,
            new_state.new_index(g_i::IndexPart::or_max(0)),
            status,
        );

        assert_eq!(map.len(), g_i::Length::or_max(1));
    }
}

