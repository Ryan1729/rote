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

fn serialize(name: &BufferName, uuid: u128) -> String {
    //TODO
    String::new()
}

fn deserialize(s: String) -> Option<(BufferName, u128)> {
    //TODO
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::{arbitrary::any, prop_oneof, proptest, strategy::Strategy};

    pub fn buffer_name() -> impl Strategy<Value = BufferName> {
        prop_oneof![
            any::<u32>().prop_map(BufferName::Scratch),
            any::<std::ffi::OsString>().prop_map(|s| BufferName::Path(PathBuf::from(s)))
        ]
    }

    proptest! {
        #[test]
        fn serialize_then_deserialize_works(
            name in buffer_name(),
            uuid in any::<u128>(),
        ) {
            assert_eq!(deserialize(serialize(&name, uuid)), Some((name, uuid)))
        }
    }
}
