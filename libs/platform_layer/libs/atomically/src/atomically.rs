#[derive(Copy, Clone)]
pub enum Overwrites {
    Disallow,
    Allow,
}

impl From<Overwrites> for atomicwrites::OverwriteBehavior {
    fn from(overwrites: Overwrites) -> Self {
        match overwrites {
            Overwrites::Disallow => Self::DisallowOverwrite,
            Overwrites::Allow => Self::AllowOverwrite,
        }
    }
}

pub fn write(
    path: impl AsRef<std::path::Path>,
    overwrite: Overwrites,
    writer: impl FnOnce(&mut std::fs::File) -> std::io::Result<()>,
) -> std::io::Result<()> {
    let path = path.as_ref();

    let dir = path.parent()
        .ok_or_else(||
            // We doen't expect this to actually come up in practice,
            // in this project.
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Input path must have a parent"
            )
        )?;

    let file = atomicwrites::AtomicFile::new_with_tmpdir(
        path,
        overwrite.into(),
        &dir
    );

    file.write(writer).map_err(|err| match err {
        atomicwrites::Error::Internal(e) => e,
        atomicwrites::Error::User(e) => e,
    })
}
