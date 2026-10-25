///! This crate is distinct from [platform_types](platform_types) in that this crate is intended
///! to not be used outside the [platform_layer](platform_layer) folder. But, it is meant to 
///! be accessible to every crate inisde the platform layer, and to house things useful across
///! multiple such crates. [platform_types](platform_types) is the external interface, this crate
///! is the internal interface.

use macros::{u};
use platform_types::{g_i, EditedTransition};

use std::path::PathBuf;

pub type PID = u32;

#[derive(Clone, Debug)]
pub enum PidKind {
    PathMailbox,
}

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

#[derive(Clone, Debug)]
pub enum CustomEvent {
    OpenFile(PathBuf),
    SaveNewFile(PathBuf, g_i::Index),
    SendBuffersToBeSaved,
    EditedBufferError(String),
    Pid(PidKind, PID),
    MarkBufferStatusTransition(PathBuf, g_i::Index, BufferStatusTransition),
}

/// Some things we might want to have implement this trait return a [Result](std::result::Result),
/// but in practice, we don't really have anything meaningful to do with the error in the places 
/// we use it. If we wanted to have retry logic or something, that could be implemented in the
/// itrait implementation, as opposed to being dealt with separately for every call to `send`.
pub trait EventSender: Clone + core::fmt::Debug + std::marker::Send + 'static {
    fn send(&self, event: CustomEvent);
}