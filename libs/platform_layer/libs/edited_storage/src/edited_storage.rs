#![deny(clippy::float_arithmetic)]
use platform_types::*;

// TODO eliminate this dependency
use rand::{thread_rng, Rng};

use wimp_types::{BufferInfo, BufferStatus, BufferStatusTransition};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use macros::{d, fmt_display};

fn get_names_to_uuid(edited_files_index_path: &Path) -> HashMap<BufferName, u128> {
    let index_string = std::fs::read_to_string(edited_files_index_path).unwrap_or_default();
    let mut names_to_uuid: HashMap<BufferName, u128> =
        HashMap::with_capacity(index_string.lines().count());

    for line in index_string.lines() {
        if let Some((name, uuid)) = deserialize(line) {
            names_to_uuid.insert(name, uuid);
        }
    }

    names_to_uuid
}

/// # Errors
/// This returns an error if there is an issue writing to the passed in paths.
pub fn store_buffers(
    edited_files_dir: &Path,
    edited_files_index_path: &Path,
    all_buffers: Vec<BufferInfo>,
    index_state: g_i::State,
) -> std::io::Result<Vec<(g_i::Index, BufferStatusTransition)>> {
    use std::fs::{create_dir_all, remove_file, write};

    create_dir_all(edited_files_dir)?;

    let mut rng = thread_rng();

    let mut names_to_uuid: HashMap<BufferName, u128> = get_names_to_uuid(edited_files_index_path);

    let mut result = Vec::with_capacity(all_buffers.len());

    for (i, info) in all_buffers.into_iter().enumerate() {
        let filename = if let Some(uuid) = names_to_uuid.get(&info.name) {
            get_path(&info.name_string, uuid)
        } else {
            let uuid: u128 = rng.gen();

            let path = get_path(&info.name_string, &uuid);

            // we don't expect to read this again in the same
            // loop, but it should be saved back to disk for
            // next time.
            names_to_uuid.insert(info.name, uuid);

            path
        };

        let path = edited_files_dir.join(filename);

        // TODO replace all files in directory with these files atomically if possible
        match info.status {
            BufferStatus::Unedited => {
                match remove_file(path).map_err(|e| e.kind()) {
                    Err(std::io::ErrorKind::NotFound) => {}
                    otherwise => otherwise?,
                };
            }
            _ => {
                write(path, info.chars)?;
            }
        }

        let index = index_state.new_index(g_i::IndexPart::or_max(i));

        result.push((index, BufferStatusTransition::SaveTemp));
    }

    let mut index_string = String::with_capacity(
        names_to_uuid.len() * INDEX_LINE_LENGTH_ESTIMATE
    );

    for (k, v) in names_to_uuid {
        serialize(&k, v, &mut index_string);
        index_string.push('\n');
    }

    //TODO make this atomic with the other writes in this function.
    write(edited_files_index_path, index_string)?;

    Ok(result)
}

#[derive(Debug)]
pub enum CanonicalizeErrorKind {
    IO(std::io::Error),
    Str(&'static str)
}

fmt_display!{
    for CanonicalizeErrorKind: match kind {
        IO(e) => e.to_string(),
        Str(s) => (*s).to_string(),
    }
}

#[derive(Debug)]
pub struct CanonicalizeError {
    kind: CanonicalizeErrorKind,
    last_path_tried: PathBuf,
}

impl CanonicalizeError {
    #[must_use]
    pub fn io(last_path_tried: PathBuf, error: std::io::Error) -> Self {
        Self {
            kind: CanonicalizeErrorKind::IO(error),
            last_path_tried,
        }
    }

    #[must_use]
    pub fn str(last_path_tried: PathBuf, error: &'static str) -> Self {
        Self {
            kind: CanonicalizeErrorKind::Str(error),
            last_path_tried,
        }
    }
}

fmt_display!{
    for CanonicalizeError: pre in "{}\nLast Path tried: {}", pre.kind, pre.last_path_tried.display()
}

impl std::error::Error for CanonicalizeError {}

pub struct CanonicalPath {
    pub path: PathBuf,
    pub position: Option<Position>,
}

impl <'path> CanonicalPath {
    #[must_use]
    pub fn to_string_lossy(&'path self) -> std::borrow::Cow<'path, str> {
        use std::borrow::Cow::{Borrowed, Owned};

        if let Some(position) = self.position {
            if let Some(os_str) = self.path.file_name() {
                let mut file_name = os_str.to_owned();
                file_name.push::<String>(format!(":{}", position.line));

                let adjusted_path = self.path.with_file_name(file_name);
                let cow = adjusted_path.to_string_lossy();

                return match cow {
                    Borrowed(s) => Owned(s.to_owned()),
                    Owned(s) => Owned(s)
                };
            }
        }

        self.path.to_string_lossy()
    }
}

/// # Errors
/// Retruns an error if the path cannot be parsed correctly, or the 
/// `Path::canocicalize` call fails.
pub fn canonicalize<P: AsRef<Path>>(
    p: P
) -> Result<CanonicalPath, CanonicalizeError> {
    let path = p.as_ref();

    path.file_name()
        .and_then(std::ffi::OsStr::to_str)
        .ok_or_else(|| CanonicalizeError::str(path.to_owned(), "Invalid unicode?"))
        .and_then(|s| {
            let mut chunks = s.splitn(2, ':');
            let file_name = chunks.next().ok_or_else(|| CanonicalizeError::str(path.to_owned(), "Bad filename"))?;

            let position = chunks.next()
                .and_then(|rest| {
                    rest.parse::<Position>().ok()
                });

            let adjusted_path_buf = path.with_file_name(file_name);
            let adjusted_path = &adjusted_path_buf;
            adjusted_path.canonicalize()
                .map(|path| {
                    CanonicalPath {
                        path,
                        position,
                    }
                })
                .map_err(|e| CanonicalizeError::io(adjusted_path.clone(), e))
        })
        // If the path is not valid unicode or whatever, then we just try
        // it as is.
        .or_else(|_| {
            path.canonicalize()
                .map(|path| {
                    CanonicalPath {
                        path,
                        position: d!(),
                    }
                })
                .map_err(|e| CanonicalizeError::io(path.to_owned(), e))
        })
}

pub struct LoadedTab {
    pub name: BufferName,
    pub data: String,
    pub position: Option<Position>,
}

#[must_use]
pub fn load_previous_tabs(
    edited_files_dir: &Path,
    edited_files_index_path: &Path,
) -> Vec<LoadedTab> {
    let names_to_uuid: HashMap<BufferName, u128> = get_names_to_uuid(edited_files_index_path);

    let mut result = Vec::with_capacity(names_to_uuid.len());

    let mut pairs: Vec<_> = names_to_uuid.into_iter().collect();
    //TODO store the save order and sort by that?
    pairs.sort();

    for (name, uuid) in pairs {
        let path = edited_files_dir.join(get_path(&name.to_string(), &uuid));

        if let Ok(tab) = load_tab_with_name(path, name) {
            result.push(tab);
        }
    }
    result
}

/// # Errors
/// This returns an error if the path cannot be read.
pub fn load_tab(canonical_path: CanonicalPath) -> Result<LoadedTab, std::io::Error>{
    std::fs::read_to_string(&canonical_path.path)
        .map(|data| {
            LoadedTab {
                name: BufferName::Path(canonical_path.path),
                data,
                position: canonical_path.position,
            }
        })
}

fn load_tab_with_name<P: AsRef<Path>>(path: P, name: BufferName) -> std::io::Result<LoadedTab>{
    let path = path.as_ref();
    std::fs::read_to_string(path).map(|data| LoadedTab {
        name,
        data,
        position: None,
    })
}

fn get_path(buffer_name: &str, uuid: &u128) -> PathBuf {
    let slug = buffer_name.replace(|c: char| !c.is_ascii_alphabetic(), "_");

    PathBuf::from(format!("{}_{:032x}", slug, uuid))
}

const PATH_PREFIX: &str = "Path: ";
const SCRATCH_PREFIX: &str = "Scratch: ";
const SCRATCH_PREFIX_LENGTH: usize = 9;

const UUID_SUFFIX_LENGTH: usize = 32
    + 1 // for comma
;

const PATH_LENGTH_ESTIMATE: usize = 128;

const INDEX_LINE_LENGTH_ESTIMATE: usize =
    SCRATCH_PREFIX_LENGTH // the longest prefix
    + UUID_SUFFIX_LENGTH
    + PATH_LENGTH_ESTIMATE // the longest extra field
    ;

fn serialize(name: &BufferName, uuid: u128, append_target: &mut String) {
    use std::fmt::Write;
    use BufferName::*;
    let _write_for_string_always_works = match name {
        Path(p) => {
            // if a user selects a non-unicode path, then at least this way the data will be
            // preserved, even if it might be directed to a different filename. Excluding the
            // corner case of two distinct paths being lossily converted to the same string,
            // which would result in the most recent one overwriting the other one I guess?
            // Hopefully seeing the first file being rendered with replacement characters will
            // convince you to be careful editing files with those filenames?
            let path_string = p.to_string_lossy();
            write!(
                append_target,
                "{}{:032x},{}",
                PATH_PREFIX, uuid, path_string
            )
        }
        Scratch(n) => write!(append_target, "{}{:032x},{}", SCRATCH_PREFIX, uuid, n),
    };
}

fn deserialize(s: &str) -> Option<(BufferName, u128)> {
    if let Some(s) = s.strip_prefix(PATH_PREFIX) {
        if let Some((uuid, path)) = split_off_uuid_and_comma(s) {
            return Some((BufferName::Path(PathBuf::from(path)), uuid));
        }
    } else if let Some(s) = s.strip_prefix(SCRATCH_PREFIX) {
        if let Some((uuid, rest)) = split_off_uuid_and_comma(s) {
            if let Ok(n) = rest.parse() {
                return Some((BufferName::Scratch(n), uuid));
            }
        }
    }

    None
}

fn split_off_uuid_and_comma(s: &str) -> Option<(u128, &str)> {
    // > not >= to exclude "" as a `rest`!
    if s.chars().count() > UUID_SUFFIX_LENGTH {
        let (uuid_and_comma, rest) = &s.split_at(UUID_SUFFIX_LENGTH);
        if uuid_and_comma.ends_with(',') {
            if let Ok(uuid) = u128::from_str_radix(&uuid_and_comma[..(UUID_SUFFIX_LENGTH - 1)], 16)
            {
                return Some((uuid, rest));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::{arbitrary::any, prop_oneof, proptest, strategy::Strategy};

    pub fn buffer_name() -> impl Strategy<Value = BufferName> {
        prop_oneof![
            any::<u32>().prop_map(BufferName::Scratch),
            any::<std::ffi::OsString>().prop_map(|s| {
                let s = if s.len() == 0 {
                    "empty-paths-are-invalid".into()
                } else {
                    s
                };
                BufferName::Path(PathBuf::from(s))
            })
        ]
    }

    fn serialize_then_deserialize_works_on(name: BufferName, uuid: u128) {
        let mut serialized = String::with_capacity(UUID_SUFFIX_LENGTH);
        serialize(&name, uuid, &mut serialized);
        let deserialized = deserialize(&serialized);
        let expected = Some((name, uuid));
        assert_eq!(
            deserialized, expected,
            "{:?} deserialized into \"{:?}\", not \"{:?}\"",
            serialized, deserialized, expected
        )
    }

    proptest! {
        #[test]
        fn serialize_then_deserialize_works(
            name in buffer_name(),
            uuid in any::<u128>(),
        ) {
            serialize_then_deserialize_works_on(name, uuid)
        }
    }
}
