use platform_types::*;

use rand::{thread_rng, Rng};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn sync(buffers: Vec<BufferView>) -> std::io::Result<()> {
    let mut rng = thread_rng();

    // TODO load from file
    let mut names_to_uuid: HashMap<BufferName, u128> = HashMap::new();

    for buffer in buffers {
        let filename = if let Some(uuid) = names_to_uuid.get(&buffer.name) {
            get_path(buffer.name_string, uuid)
        } else {
            let uuid: u128 = rng.gen();

            let path = get_path(buffer.name_string, &uuid);

            // we don't expect to read this again in the same
            // loop, but it should be saved back to disk for
            // next time.
            names_to_uuid.insert(buffer.name, uuid);

            path
        };

        std::fs::write(filename, buffer.data.chars)?;
    }

    // TODO write out to disk
    //names_to_uuid

    Ok(())
}

fn get_path(buffer_name: String, uuid: &u128) -> PathBuf {
    let slug = buffer_name.replace(|c: char| !c.is_ascii_alphabetic(), "_");

    PathBuf::from(format!("{}_{}", slug, uuid))
}

const PATH_PREFIX: &'static str = "Path: ";
const SCRATCH_PREFIX: &'static str = "Scratch: ";

const UUID_SUFFIX_LENGTH: usize = 32
    + 1 // for comma
;
fn serialize(name: &BufferName, uuid: u128) -> String {
    use BufferName::*;
    match name {
        Path(p) => {
            // if a user selects a non-unicode path, then at least this way the data will be
            // preserved, even if it might be directed to a different filename. Excluding the
            // corner case of two distinct paths being lossily converted to the same string,
            // which would result in the most recent one overwriting the other one I guess?
            // Hopefully seeing the first file being rendered with replacement characters will
            // convince you to be careful editing files with those filenames?
            let path_string = p.to_string_lossy();
            format!("{}{:032x},{}", PATH_PREFIX, uuid, path_string)
        }
        Scratch(n) => format!("{}{:032x},{}", SCRATCH_PREFIX, uuid, n),
    }
}

fn deserialize(s: &str) -> Option<(BufferName, u128)> {
    if s.starts_with(PATH_PREFIX) {
        // works becasue the prefix is ASCII
        let s = &s[PATH_PREFIX.len()..];

        if let Some((uuid, path)) = split_off_uuid_and_comma(s) {
            return Some((BufferName::Path(PathBuf::from(path)), uuid));
        }
    } else if s.starts_with(SCRATCH_PREFIX) {
        // works becasue the prefix is ASCII
        let s = &s[SCRATCH_PREFIX.len()..];
        if let Some((uuid, rest)) = split_off_uuid_and_comma(s) {
            if let Ok(n) = rest.parse() {
                return Some((BufferName::Scratch(n), uuid));
            }
        }
    }

    None
}

fn split_off_uuid_and_comma<'s>(s: &'s str) -> Option<(u128, &'s str)> {
    // > not >= to exclude "" as a `rest`!
    if s.chars().count() > UUID_SUFFIX_LENGTH {
        let (uuid_and_comma, rest) = &s.split_at(UUID_SUFFIX_LENGTH);
        if uuid_and_comma.ends_with(",") {
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
        let serialized = serialize(&name, uuid);
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
