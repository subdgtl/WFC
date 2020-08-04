use std::collections::hash_map::{Entry, HashMap};
use std::error;
use std::fmt;
use std::io;
use std::num::{self, NonZeroU32};
use std::str::FromStr as _;

use regex::Regex;
use tinyvec::TinyVec;

use crate::convert::cast_u16;
use crate::tiled3d_wfc::{self, Tiled3dAdjacency, Tiled3dAdjacencyKind};

const SLOT_NAME_VOID: &str = "?";
const SLOT_NAME_WILDCARD: &str = "*";

#[derive(Debug)]
pub enum Tiled3dAdjacencyRulesImportError {
    InvalidRecordLength {
        row: usize,
        expected: usize,
        found: usize,
    },
    InvalidAdjacencyKind {
        adjacency_kind: String,
    },
    Csv(csv::Error),
}

impl fmt::Display for Tiled3dAdjacencyRulesImportError {
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
            Self::Csv(csv_error) => write!(f, "{}", csv_error),
        }
    }
}

impl error::Error for Tiled3dAdjacencyRulesImportError {}

impl From<csv::Error> for Tiled3dAdjacencyRulesImportError {
    fn from(csv_error: csv::Error) -> Self {
        Self::Csv(csv_error)
    }
}

pub struct Tiled3dAdjacencyRulesImportResult {
    pub adjacencies: Vec<Tiled3dAdjacency>,
    pub name_to_module: HashMap<String, NonZeroU32>,
    pub module_to_name: HashMap<NonZeroU32, String>,
}

pub fn import_adjacency_rules<R: io::Read>(
    r: R,
) -> Result<Tiled3dAdjacencyRulesImportResult, Tiled3dAdjacencyRulesImportError> {
    log::info!("Importing adjacency rules CSV");

    let mut next_module = 1;
    let mut module_to_name: HashMap<NonZeroU32, String> = HashMap::new();
    let mut name_to_module: HashMap<String, NonZeroU32> = HashMap::new();

    let mut adjacencies: Vec<Tiled3dAdjacency> = Vec::new();
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(false)
        .comment(Some(b'#'))
        .from_reader(r);

    for (i, result) in reader.records().enumerate() {
        let record = result?;

        let record_len = record.len();
        if record_len != 3 {
            return Err(Tiled3dAdjacencyRulesImportError::InvalidRecordLength {
                row: i,
                expected: 3,
                found: record_len,
            });
        }

        let adjacency_kind_str = record.get(0).unwrap();
        let module_low_str = record.get(1).unwrap();
        let module_high_str = record.get(2).unwrap();

        if let ("axis", "low", "high") = (
            adjacency_kind_str.trim(),
            module_low_str.trim(),
            module_high_str.trim(),
        ) {
            log::info!("Skipping over CSV header");
            continue;
        }

        let adjacency_kind = match adjacency_kind_str {
            "x" | "X" => Tiled3dAdjacencyKind::X,
            "y" | "Y" => Tiled3dAdjacencyKind::Y,
            "z" | "Z" => Tiled3dAdjacencyKind::Z,
            _ => {
                return Err(Tiled3dAdjacencyRulesImportError::InvalidAdjacencyKind {
                    adjacency_kind: adjacency_kind_str.to_string(),
                });
            }
        };

        let module_low = match name_to_module.entry(module_low_str.to_string()) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let module = NonZeroU32::new(next_module).unwrap();
                vacant_entry.insert(module);
                module_to_name.insert(module, module_low_str.to_string());

                next_module += 1;

                module
            }
        };

        let module_high = match name_to_module.entry(module_high_str.to_string()) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let module = NonZeroU32::new(next_module).unwrap();
                vacant_entry.insert(module);
                module_to_name.insert(module, module_high_str.to_string());

                next_module += 1;

                module
            }
        };

        adjacencies.push(Tiled3dAdjacency {
            kind: adjacency_kind,
            module_low: module_low,
            module_high: module_high,
        });
    }

    Ok(Tiled3dAdjacencyRulesImportResult {
        adjacencies,
        name_to_module,
        module_to_name,
    })
}

#[derive(Debug)]
pub enum Tiled3dInitialStateImportError {
    InvalidHeader,
    DimensionMismatch,
    UnexpectedModule(String),
    ParseInt {
        line: usize,
        error: num::ParseIntError,
    },
    Io(io::Error),
}

impl fmt::Display for Tiled3dInitialStateImportError {
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
            Self::ParseInt { line, error } => write!(
                f,
                "Could not parse integer on line {}. Error: {}",
                line, error,
            ),
            Self::Io(io_error) => write!(f, "IO error: {}", io_error),
        }
    }
}

impl error::Error for Tiled3dInitialStateImportError {}

impl From<io::Error> for Tiled3dInitialStateImportError {
    fn from(io_error: io::Error) -> Self {
        Self::Io(io_error)
    }
}

pub fn import_initial_state<R: io::BufRead>(
    mut r: R,
    dims: [u16; 3],
    name_to_module: &HashMap<String, NonZeroU32>,
) -> Result<Vec<TinyVec<[u32; 4]>>, Tiled3dInitialStateImportError> {
    log::info!("Importing initial state text");

    let slot_regex = Regex::new(r"\*|\?|\[([^\]]*)\]").unwrap();
    let mut line_buffer = String::with_capacity(1024);
    let mut line = 1;

    r.read_line(&mut line_buffer)?;
    let mut split_iter = line_buffer.trim().split(' ');

    let dim_x_str = split_iter
        .next()
        .ok_or(Tiled3dInitialStateImportError::InvalidHeader)?;
    let dim_y_str = split_iter
        .next()
        .ok_or(Tiled3dInitialStateImportError::InvalidHeader)?;
    let dim_z_str = split_iter
        .next()
        .ok_or(Tiled3dInitialStateImportError::InvalidHeader)?;

    let dim_x = u16::from_str(dim_x_str.trim())
        .map_err(|error| Tiled3dInitialStateImportError::ParseInt { line, error })?;
    let dim_y = u16::from_str(dim_y_str.trim())
        .map_err(|error| Tiled3dInitialStateImportError::ParseInt { line, error })?;
    let dim_z = u16::from_str(dim_z_str.trim())
        .map_err(|error| Tiled3dInitialStateImportError::ParseInt { line, error })?;

    if dim_x != dims[0] || dim_y != dims[1] || dim_z != dims[2] {
        return Err(Tiled3dInitialStateImportError::DimensionMismatch);
    }

    let world_len = usize::from(dim_x) * usize::from(dim_y) * usize::from(dim_z);
    let mut world: Vec<TinyVec<[u32; 4]>> = vec![TinyVec::new(); world_len];

    for z in (0..dim_z).rev() {
        line_buffer.clear();
        r.read_line(&mut line_buffer)?;
        line += 1;
        line_buffer.clear();

        for y in (0..dim_y).rev() {
            r.read_line(&mut line_buffer)?;
            line += 1;

            for (x, slot_match) in slot_regex.find_iter(&line_buffer).enumerate() {
                let slot_str = slot_match.as_str();

                let mut slot: TinyVec<[u32; 4]> = TinyVec::new();
                match slot_str {
                    SLOT_NAME_WILDCARD => {
                        slot.extend(name_to_module.values().copied().map(NonZeroU32::get))
                    }
                    SLOT_NAME_VOID => {
                        slot.push(0);
                    }
                    _ => {
                        let slot_str_inner = slot_str.trim_start_matches('[').trim_end_matches(']');

                        // Only attempt to parse integers if the cell contains
                        // something else than whitespace.
                        if !slot_str_inner.trim().is_empty() {
                            for name in slot_str_inner.split(',') {
                                let module = name_to_module.get(name).ok_or_else(|| {
                                    Tiled3dInitialStateImportError::UnexpectedModule(
                                        name.to_string(),
                                    )
                                })?;

                                slot.push(module.get());
                            }
                        }
                    }
                }

                let index = tiled3d_wfc::position_to_index(
                    world_len,
                    [dim_x, dim_y, dim_z],
                    [cast_u16(x), cast_u16(y), cast_u16(z)],
                );
                world[index] = slot;
            }

            line_buffer.clear();
        }
    }

    Ok(world)
}

// FIXME: Error handling for writing

pub fn export_tiled_text<W: io::Write>(
    w: &mut W,
    dims: [u16; 3],
    slots: &[TinyVec<[u32; 4]>],
    module_to_name: &HashMap<NonZeroU32, String>,
) {
    assert!(dims[0] > 0);
    assert!(dims[1] > 0);
    assert!(dims[2] > 0);

    // FIXME: Formatting for multichar names
    let largest_slot = slots.iter().fold(0, |max, s| usize::max(max, s.len()));

    let mut slot_buffer = String::with_capacity(32);

    writeln!(w, "{} {} {}", dims[0], dims[1], dims[2]).unwrap();
    writeln!(w).unwrap();

    for z in (0..dims[2]).rev() {
        for y in (0..dims[1]).rev() {
            for x in 0..dims[0] {
                let index = tiled3d_wfc::position_to_index(slots.len(), dims, [x, y, z]);
                let modules_in_slot = &slots[index];

                let slot_width = if x == dims[0] - 1 {
                    0
                } else {
                    2 + largest_slot + (largest_slot - 1).max(0)
                };

                write_modules_in_slot(&mut slot_buffer, modules_in_slot, module_to_name);
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
    modules_in_slot: &[u32],
    module_to_name: &HashMap<NonZeroU32, String>,
) {
    if modules_in_slot.len() == 1 && modules_in_slot[0] == 0 {
        write!(w, " {} ", SLOT_NAME_VOID).unwrap();
    } else {
        write!(w, "[").unwrap();

        let mut first = true;
        for module in modules_in_slot {
            let name = &module_to_name[&NonZeroU32::new(*module).unwrap()];

            if first {
                write!(w, "{}", name).unwrap();
                first = false;
            } else {
                write!(w, ",{}", name).unwrap();
            }
        }

        write!(w, "]").unwrap();
    }
}
