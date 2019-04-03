use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;
use std::io;
use std::path::Path;

/// A log file stores entries. This type provides handles to log files, with
/// the following guarantees:
///
///  - Once append returns, the changes are written to disk.
///  - Garbage data at the end of the file is ignored, making the
///    implementation robust against crashes.
///
/// The layout of a log file? A packed sequence of entries, each prefixed with
/// their length as a little-endian 32-bit unsigned integer.
pub struct LogFile {
    /// The handle to the file on disk.
    handle: File,

    /// The physical offset to seek to, relative to the start of the file,
    /// before appending an entry. If None, we do not know this yet, and we
    /// will have to traverse the file first.
    logical_end: Option<u64>,

    /// We want an impl for Send but not for Sync.
    // TODO: Replace with negative trait bound !Sync once stable.
    _not_sync: Box<dyn Send>,
}

impl LogFile {
    /// Create a new log file with no entries.
    pub fn create_new<P>(path: P) -> io::Result<Self> where P: AsRef<Path> {
        let handle =
            OpenOptions::new()
                .create_new(true)
                .read(true)
                .write(true)
                .open(path)?;
        Ok(LogFile{handle, logical_end: Some(0), _not_sync: Box::new(())})
    }

    /// Open an existing log file.
    pub fn open<P>(path: P) -> io::Result<Self> where P: AsRef<Path> {
        let handle =
            OpenOptions::new()
                .read(true)
                .write(true)
                .open(path)?;
        Ok(LogFile{handle, logical_end: None, _not_sync: Box::new(())})
    }

    /// Seek to the logical start of the file.
    fn seek_to_logical_start(&mut self) -> io::Result<u64> {
        self.handle.seek(SeekFrom::Start(0))
    }

    /// Seek to the logical end of the file.
    fn seek_to_logical_end(&mut self) -> io::Result<u64> {
        if let Some(offset) = self.logical_end {
            // We already know the logical end, so we can seek to it and return
            // the new current offset.
            self.handle.seek(SeekFrom::Start(offset))
        } else {
            // We do not yet know the logical end of the file, so fold the file
            // to find the logical end.

            let size = self.handle.metadata()?.len();
            let mut offset = self.seek_to_logical_start()?;
            loop {
                // In this loop we take care not to update the offset variable
                // until we have determined that the entry is complete. Imagine
                // there to be an incomplete entry at the physical end of the
                // file. We do not want this entry to be included in the
                // logical end of the file: we want to overwrite it on the next
                // append.

                // Read the length of the entry payload, but not if there are
                // not enough bytes left.
                if offset + 4 > size { break }
                let length = self.handle.read_u32::<LittleEndian>()?;

                // Skip the payload of the entry, but not if there are not
                // enough bytes left.
                if offset + 4 + length as u64 > size { break }
                self.handle.seek(SeekFrom::Current(length as i64))?;

                // Update the accumulator after we have determined that the
                // entry is complete.
                offset += 4 + length as u64;
            }

            // Seek to the logical end we found and update the cached logical
            // end so that the next call to this subroutine is very fast.
            self.handle.seek(SeekFrom::Start(offset))?;
            self.logical_end = Some(offset);

            // Return the new current offset, which is at the logical end of
            // the file.
            Ok(offset)
        }
    }

    /// Append a new entry onto the log file.
    pub fn append(&mut self, entry: &[u8]) -> io::Result<()> {
        let offset = self.seek_to_logical_end()?;
        let written = self.write_entry(entry)?;
        self.logical_end = Some(offset + written);
        self.handle.sync_all()?;
        Ok(())
    }

    /// All entries in the log file, in order.
    pub fn entries(&mut self) -> Entries {
        Entries{inner: self, started: false}
    }

    /// Write an entry at the current offset in the file and return how many
    /// bytes were written.
    fn write_entry(&mut self, entry: &[u8]) -> io::Result<u64> {
        // TODO: What to do if the length is larger than std::u32::MAX?
        self.handle.write_u32::<LittleEndian>(entry.len() as u32)?;
        self.handle.write_all(entry)?;
        Ok(4 + entry.len() as u64)
    }

    /// Read an entry at the current offset in the file.
    fn read_entry(&mut self) -> io::Result<Vec<u8>> {
        let length = self.handle.read_u32::<LittleEndian>()? as usize;
        let mut entry = vec![0; length];
        self.handle.read_exact(&mut entry)?;
        Ok(entry)
    }
}

/// Iterator over the entries in a log file. Create one using [entries].
///
/// [entries]: struct.LogFile.html#method.entries
pub struct Entries<'a> {
    inner: &'a mut LogFile,
    started: bool,
}

impl Iterator for Entries<'_> {
    type Item = io::Result<Vec<u8>>;

    fn next(&mut self) -> Option<Self::Item> {
        // Macro that is similar to try, but returns None or Some depending on
        // the error kind.
        macro_rules! try_eof {
            ($e:expr) => {{
                use std::io::ErrorKind::UnexpectedEof;
                match $e {
                    Ok(r)  => r,
                    Err(e) => if e.kind() == UnexpectedEof { return None }
                              else { return Some(Err(e)) },
                }
            }};
        }

        // Seek to the first entry the first time this subroutine is called. We
        // need to do that here instead of in entries, because only here do we
        // have a chance to return a Result.
        if !self.started {
            try_eof!(self.inner.seek_to_logical_start());
            self.started = true;
        }

        // Read the next entry and return it. This should leave the current
        // offset at the correct value for the next call to this iterator.
        let entry = try_eof!(self.inner.read_entry());
        Some(Ok(entry))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs;

    #[test]
    fn test_scenario() -> io::Result<()> {
        let path = "/tmp/alkane_log_file_test_scenario";

        drop(fs::remove_file(path));

        {
            let mut file = LogFile::create_new(path)?;
            file.append(b"")?;
            file.append(b"foo")?;
            file.append(b"bazqux")?;
        }

        {
            let mut file = LogFile::open(path)?;
            file.append(b"lorem ipsum")?;
            file.append(b"dolor sit amet")?;
            file.append(b"geef chips")?;
        }

        {
            let mut file = LogFile::open(path)?;
            let entries = file.entries().collect::<io::Result<Vec<Vec<u8>>>>()?;
            assert_eq!(
                &entries,
                &[b"".to_vec(), b"foo".to_vec(), b"bazqux".to_vec(),
                  b"lorem ipsum".to_vec(), b"dolor sit amet".to_vec(),
                  b"geef chips".to_vec()]
            );
        }

        Ok(())
    }
}
