use std::error;
use std::fmt;
use std::io;
use std::num;
use std::str::FromStr as _;

use crate::tiled3d_wfc::{self, Tiled3dAdjacency, Tiled3dAdjacencyKind};

#[derive(Debug)]
pub enum Tiled3dCsvImportError {
    InvalidRecordLength {
        row: usize,
        expected: usize,
        found: usize,
    },
    InvalidAdjacencyKind {
        adjacency_kind: String,
    },
    ParseInt {
        row: usize,
        col: usize,
        error: num::ParseIntError,
    },
    Csv(csv::Error),
}

impl fmt::Display for Tiled3dCsvImportError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidRecordLength {
                row,
                expected,
                found,
            } => write!(
                f,
                "CSV row {} contains invalid number of fields (expected {} but found {})",
                row, expected, found,
            ),
            Self::InvalidAdjacencyKind { adjacency_kind } => {
                write!(f, "Found invalid adjacency kind: {}", adjacency_kind)
            }
            Self::ParseInt { row, col, error } => write!(
                f,
                "Could not parse integer on row {}, column {}. Error: {}",
                row, col, error,
            ),
            Self::Csv(csv_error) => write!(f, "{}", csv_error),
        }
    }
}

impl error::Error for Tiled3dCsvImportError {}

impl From<csv::Error> for Tiled3dCsvImportError {
    fn from(csv_error: csv::Error) -> Self {
        Self::Csv(csv_error)
    }
}

pub fn import_tiled_csv<R: io::Read>(r: R) -> Result<Vec<Tiled3dAdjacency>, Tiled3dCsvImportError> {
    log::info!("Importing CSV");

    let mut adjacencies: Vec<Tiled3dAdjacency> = Vec::new();
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(true)
        .comment(Some(b'#'))
        .from_reader(r);

    for (i, result) in reader.records().enumerate() {
        let record = result?;

        let record_len = record.len();
        if record_len != 3 {
            return Err(Tiled3dCsvImportError::InvalidRecordLength {
                row: i,
                expected: 3,
                found: record_len,
            });
        }

        let adjacency_kind_str = record.get(0).unwrap();
        let module_low_str = record.get(1).unwrap();
        let module_high_str = record.get(2).unwrap();

        let adjacency_kind = match adjacency_kind_str {
            "x" | "X" => Tiled3dAdjacencyKind::X,
            "y" | "Y" => Tiled3dAdjacencyKind::Y,
            "z" | "Z" => Tiled3dAdjacencyKind::Z,
            _ => {
                return Err(Tiled3dCsvImportError::InvalidAdjacencyKind {
                    adjacency_kind: adjacency_kind_str.to_string(),
                });
            }
        };
        let module_low =
            u32::from_str(module_low_str).map_err(|err| Tiled3dCsvImportError::ParseInt {
                row: i,
                col: 1,
                error: err,
            })?;
        let module_high =
            u32::from_str(module_high_str).map_err(|err| Tiled3dCsvImportError::ParseInt {
                row: i,
                col: 2,
                error: err,
            })?;

        adjacencies.push(Tiled3dAdjacency {
            kind: adjacency_kind,
            module_low,
            module_high,
        });
    }

    Ok(adjacencies)
}

// FIXME: Error handling for writing

pub fn export_tiled_text<W: io::Write>(w: &mut W, dims: [u16; 3], slots: &[Option<u32>]) {
    assert!(dims[0] > 0);
    assert!(dims[1] > 0);
    assert!(dims[2] > 0);

    writeln!(w, "{} {} {}", dims[0], dims[1], dims[2]).unwrap();
    writeln!(w).unwrap();

    for z in (0..dims[2]).rev() {
        for y in (0..dims[1]).rev() {
            let mut first = true;
            for x in 0..dims[0] {
                let index = tiled3d_wfc::position_to_index(slots.len(), dims, [x, y, z]);
                let slot = slots[index].unwrap();

                if first {
                    write!(w, "{}", slot).unwrap();
                    first = false;
                } else {
                    write!(w, " {}", slot).unwrap();
                }
            }

            writeln!(w).unwrap();
        }

        writeln!(w).unwrap();
    }
}
