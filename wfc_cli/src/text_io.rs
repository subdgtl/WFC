use std::collections::hash_map::{Entry, HashMap};
use std::error;
use std::fmt;
use std::io;
use std::num;
use std::str::FromStr as _;

use regex::Regex;
use tinyvec::TinyVec;
use wfc_core::{Adjacency, AdjacencyKind, World};

use crate::convert::cast_u16;

const SLOT_NAME_WILDCARD: &str = "*";

#[derive(Debug)]
pub enum AdjacencyRulesImportError {
    InvalidRecordLength {
        row: usize,
        expected: usize,
        found: usize,
    },
    InvalidAdjacencyRuleKind {
        kind: String,
    },
    Csv(csv::Error),
}

impl fmt::Display for AdjacencyRulesImportError {
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
            Self::InvalidAdjacencyRuleKind { kind } => {
                write!(f, "Found invalid adjacency kind: {}", kind)
            }
            Self::Csv(csv_error) => write!(f, "{}", csv_error),
        }
    }
}

impl error::Error for AdjacencyRulesImportError {}

impl From<csv::Error> for AdjacencyRulesImportError {
    fn from(csv_error: csv::Error) -> Self {
        Self::Csv(csv_error)
    }
}

pub struct AdjacencyRulesImportResult {
    pub adjacencies: Vec<Adjacency>,
    pub name_to_module: HashMap<String, u8>,
    pub module_to_name: HashMap<u8, String>,
}

pub fn import_adjacency_rules<R: io::Read>(
    r: R,
) -> Result<AdjacencyRulesImportResult, AdjacencyRulesImportError> {
    log::info!("Importing adjacency rules CSV");

    let mut next_module = 0;
    let mut module_to_name: HashMap<u8, String> = HashMap::new();
    let mut name_to_module: HashMap<String, u8> = HashMap::new();

    let mut adjacencies: Vec<Adjacency> = Vec::new();
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(false)
        .comment(Some(b'#'))
        .from_reader(r);

    for (i, result) in reader.records().enumerate() {
        let record = result?;

        let record_len = record.len();
        if record_len != 3 {
            return Err(AdjacencyRulesImportError::InvalidRecordLength {
                row: i,
                expected: 3,
                found: record_len,
            });
        }

        let kind_str = record.get(0).unwrap();
        let module_low_str = record.get(1).unwrap();
        let module_high_str = record.get(2).unwrap();

        if let ("axis", "low", "high") = (
            kind_str.trim(),
            module_low_str.trim(),
            module_high_str.trim(),
        ) {
            log::info!("Skipping over CSV header");
            continue;
        }

        let kind = match kind_str {
            "x" | "X" => AdjacencyKind::X,
            "y" | "Y" => AdjacencyKind::Y,
            "z" | "Z" => AdjacencyKind::Z,
            _ => {
                return Err(AdjacencyRulesImportError::InvalidAdjacencyRuleKind {
                    kind: kind_str.to_string(),
                });
            }
        };

        let module_low = match name_to_module.entry(module_low_str.to_string()) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let module = next_module;
                vacant_entry.insert(module);
                module_to_name.insert(module, module_low_str.to_string());

                next_module += 1;

                module
            }
        };

        let module_high = match name_to_module.entry(module_high_str.to_string()) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let module = next_module;
                vacant_entry.insert(module);
                module_to_name.insert(module, module_high_str.to_string());

                next_module += 1;

                module
            }
        };

        adjacencies.push(Adjacency {
            kind,
            module_low,
            module_high,
        });
    }

    Ok(AdjacencyRulesImportResult {
        adjacencies,
        name_to_module,
        module_to_name,
    })
}

#[derive(Debug)]
pub enum InitialStateImportError {
    InvalidHeader,
    DimensionMismatch,
    UnexpectedModule(String),
    InvalidNumberOfEntries {
        line: usize,
        expected: u16,
        found: u16,
    },
    ParseInt {
        line: usize,
        error: num::ParseIntError,
    },
    Io(io::Error),
}

impl fmt::Display for InitialStateImportError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidHeader => write!(f, "Invalid header (does not contain dimensions)"),
            Self::DimensionMismatch => {
                write!(f, "Dimension mismatch between constrain file and input")
            }
            Self::UnexpectedModule(id) => write!(
                f,
                "Found module not mentioned by the adjacency rules: {}",
                id,
            ),
            Self::InvalidNumberOfEntries {
                line,
                expected,
                found,
            } => write!(
                f,
                "Found invalid number of entries on line {}. Expected {}, but found {}.",
                line, expected, found,
            ),
            Self::ParseInt { line, error } => write!(
                f,
                "Could not parse integer on line {}. Error: {}",
                line, error,
            ),
            Self::Io(io_error) => write!(f, "IO error: {}", io_error),
        }
    }
}

impl error::Error for InitialStateImportError {}

impl From<io::Error> for InitialStateImportError {
    fn from(io_error: io::Error) -> Self {
        Self::Io(io_error)
    }
}

pub fn import_world_state<R: io::BufRead>(
    mut r: R,
    world: &mut World,
    dims: [u16; 3],
    name_to_module: &HashMap<String, u8>,
) -> Result<(), InitialStateImportError> {
    log::info!("Importing initial state text");

    let slot_regex = Regex::new(r"\*|\[([^\]]*)\]").unwrap();
    let mut line_buffer = String::with_capacity(1024);
    let mut line = 1;

    r.read_line(&mut line_buffer)?;
    let mut split_iter = line_buffer.trim().split(' ');

    let dim_x_str = split_iter
        .next()
        .ok_or(InitialStateImportError::InvalidHeader)?;
    let dim_y_str = split_iter
        .next()
        .ok_or(InitialStateImportError::InvalidHeader)?;
    let dim_z_str = split_iter
        .next()
        .ok_or(InitialStateImportError::InvalidHeader)?;

    let dim_x = u16::from_str(dim_x_str.trim())
        .map_err(|error| InitialStateImportError::ParseInt { line, error })?;
    let dim_y = u16::from_str(dim_y_str.trim())
        .map_err(|error| InitialStateImportError::ParseInt { line, error })?;
    let dim_z = u16::from_str(dim_z_str.trim())
        .map_err(|error| InitialStateImportError::ParseInt { line, error })?;

    if dim_x != dims[0] || dim_y != dims[1] || dim_z != dims[2] {
        return Err(InitialStateImportError::DimensionMismatch);
    }

    for z in (0..dim_z).rev() {
        line_buffer.clear();
        r.read_line(&mut line_buffer)?;
        line += 1;
        line_buffer.clear();

        for y in (0..dim_y).rev() {
            r.read_line(&mut line_buffer)?;
            line += 1;

            let mut line_slots = 0;
            for (x, slot_match) in slot_regex.find_iter(&line_buffer).enumerate() {
                let slot_str = slot_match.as_str();

                let mut slot: TinyVec<[u8; 8]> = TinyVec::new();
                match slot_str {
                    SLOT_NAME_WILDCARD => slot.extend(name_to_module.values().copied()),
                    _ => {
                        let slot_str_inner = slot_str.trim_start_matches('[').trim_end_matches(']');

                        // Only attempt to parse identifiers if the cell
                        // contains something else than whitespace.
                        if !slot_str_inner.trim().is_empty() {
                            for name in slot_str_inner.split(',') {
                                let module = name_to_module.get(name).ok_or_else(|| {
                                    InitialStateImportError::UnexpectedModule(name.to_string())
                                })?;

                                slot.push(*module);
                            }
                        }
                    }
                }

                let pos = [cast_u16(x), cast_u16(y), cast_u16(z)];
                world.set_slot_modules(pos, false);
                for module in slot {
                    world.set_slot_module(pos, module, true);
                }

                line_slots += 1;
            }

            if line_slots != dim_x {
                return Err(InitialStateImportError::InvalidNumberOfEntries {
                    line,
                    expected: dim_x,
                    found: line_slots,
                });
            }

            line_buffer.clear();
        }
    }

    Ok(())
}

// TODO(yan): Error handling for writing
pub fn export_world_state<W: io::Write>(
    w: &mut W,
    world: &World,
    module_to_name: &HashMap<u8, String>,
) {
    let dims = world.dims();

    assert!(dims[0] > 0);
    assert!(dims[1] > 0);
    assert!(dims[2] > 0);

    // TODO(yan): Formatting for multichar names
    let largest_slot = {
        let mut largest = 0;

        for z in (0..dims[2]).rev() {
            for y in (0..dims[1]).rev() {
                for x in 0..dims[0] {
                    let count = world.slot_modules_iter([x, y, z]).count();
                    if count > largest {
                        largest = count;
                    }
                }
            }
        }

        largest
    };

    let mut slot_buffer = String::with_capacity(32);

    writeln!(w, "{} {} {}", dims[0], dims[1], dims[2]).unwrap();
    writeln!(w).unwrap();

    for z in (0..dims[2]).rev() {
        for y in (0..dims[1]).rev() {
            for x in 0..dims[0] {
                let modules_in_slot: TinyVec<[u8; 8]> =
                    world.slot_modules_iter([x, y, z]).collect();

                let slot_width = if x == dims[0] - 1 {
                    0
                } else {
                    2 + largest_slot + (largest_slot - 1).max(0)
                };

                write_modules_in_slot(&mut slot_buffer, &modules_in_slot, module_to_name);
                write!(w, "{:<width$}", slot_buffer.trim_end(), width = slot_width).unwrap();
                slot_buffer.clear();
            }

            writeln!(w).unwrap();
        }

        writeln!(w).unwrap();
    }
}

fn write_modules_in_slot<W: fmt::Write>(
    w: &mut W,
    modules_in_slot: &[u8],
    module_to_name: &HashMap<u8, String>,
) {
    write!(w, "[").unwrap();

    let mut first = true;
    for module in modules_in_slot {
        let name = &module_to_name[module];

        if first {
            write!(w, "{}", name).unwrap();
            first = false;
        } else {
            write!(w, ",{}", name).unwrap();
        }
    }

    write!(w, "]").unwrap();
}
