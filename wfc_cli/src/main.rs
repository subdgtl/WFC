mod convert;
mod text_io;

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write as _};
use std::num::ParseIntError;
use std::path::Path;
use std::process;

use clap::Clap as _;
use wfc_core::{Rng, World, WorldStatus};

// This is the same random seed wfc_gh will produce for its default seed.
const DEFAULT_RANDOM_SEED: u128 = u128::from_le_bytes([
    62, 23, 186, 150, 174, 4, 205, 59, 153, 134, 158, 86, 240, 173, 191, 58,
]);

#[derive(clap::Clap)]
#[clap(name = "WFC", version, author)]
struct Options {
    /// Path to input CSV file.
    #[clap(parse(from_os_str))]
    pub input: OsString,
    /// Path to initial state of the world in textual form.
    #[clap(long, parse(from_os_str))]
    pub world_state: Option<OsString>,
    /// X world dimension.
    #[clap(long, short = "x", default_value = "10")]
    pub world_x: u16,
    /// Y world dimension.
    #[clap(long, short = "y", default_value = "10")]
    pub world_y: u16,
    /// Z world dimension.
    #[clap(long, short = "z", default_value = "10")]
    pub world_z: u16,
    /// The seed to initalize the random number generator.
    #[clap(long, parse(try_from_str = parse_number_literal))]
    pub random_seed: Option<u128>,
    /// How many times can the world be reset to initial world state when
    /// looking for a deterministic solution.
    #[clap(long, default_value = "100")]
    pub max_attempts: u32,
}

fn main() {
    env_logger::init();

    let options = Options::parse();
    let rng_seed = options.random_seed.unwrap_or(DEFAULT_RANDOM_SEED);
    let mut rng = Rng::new(rng_seed);

    let input_file_path = Path::new(&options.input);
    let input_file_stem = match input_file_path.file_stem() {
        Some(file_stem) => file_stem,
        None => {
            eprintln!(
                "Invalid input file name: {}",
                options.input.to_string_lossy(),
            );
            process::exit(1);
        }
    };

    let input_file = match File::open(input_file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to open input file: {}", err);
            process::exit(1);
        }
    };

    let import_result = match text_io::import_adjacency_rules(input_file) {
        Ok(import_result) => import_result,
        Err(err) => {
            eprintln!("Failed to extract adjacency rules from text: {}", err);
            process::exit(1);
        }
    };

    eprintln!("Found {} modules:", import_result.name_to_module.len());
    for (name, module) in &import_result.name_to_module {
        eprintln!("{:<10} (internal id = {})", name, module);
    }
    eprintln!("----------------------------------------");

    eprintln!("Found {} adjacency rules:", import_result.adjacencies.len());
    for adjacency in &import_result.adjacencies {
        let low = &import_result.module_to_name[&adjacency.module_low];
        let high = &import_result.module_to_name[&adjacency.module_high];
        eprintln!("{:?}    {:<10} {:<10}", adjacency.kind, low, high);
    }
    eprintln!("----------------------------------------");

    let dims = [options.world_x, options.world_y, options.world_z];

    let mut initial_world = if let Some(world_state_file_str) = options.world_state.as_ref() {
        let world_state_file_path = Path::new(world_state_file_str);
        let world_state_file = match File::open(world_state_file_path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("Failed to open world state file: {}", err);
                process::exit(1);
            }
        };

        let mut world = World::new(dims, import_result.adjacencies);

        let buf_reader = BufReader::new(world_state_file);
        if let Err(err) =
            text_io::import_world_state(buf_reader, &mut world, dims, &import_result.name_to_module)
        {
            eprintln!("Failed to extract world state from text: {}", err);
            process::exit(1);
        };

        // Since we are importing a custom world state, we can not be sure all
        // adjacency rule constraints are initially satisfied.
        let (world_changed, world_status) = world.ensure_constraints();

        if world_changed {
            eprintln!("WARNING: Provided world state does not initially satisfy adjacency rules");
        }

        if world_status == WorldStatus::Deterministic {
            eprintln!("World is already deterministic");
            process::exit(1);
        }

        if world_status == WorldStatus::Contradiction {
            eprintln!("World is contradictory");
            process::exit(1);
        }

        world
    } else {
        World::new(dims, import_result.adjacencies)
    };

    // XXX
    for x in 0..(dims[0] as u16) {
        for y in 0..(dims[1] as u16) {
            for z in 0..(dims[2] as u16) {
                initial_world.set_slot_module_weights([x, y, z], &[0.1, 0.9]);
            }
        }
    }

    let mut world = initial_world.clone();

    let mut found_deterministic_result = false;
    let mut attempts = 0;
    while attempts < options.max_attempts && !found_deterministic_result {
        let mut observations = 0;

        world.clone_from(&initial_world);

        let status = loop {
            let (_, status) = world.observe(&mut rng);

            match status {
                WorldStatus::Nondeterministic => {
                    if observations % 100 == 0 {
                        eprintln!(
                            "attempt: {:>4}, observation: {:>4}, {} (In progress)",
                            attempts, observations, status,
                        );
                    }
                }
                WorldStatus::Deterministic => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, status,
                    );

                    break WorldStatus::Deterministic;
                }
                WorldStatus::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, status,
                    );

                    break WorldStatus::Contradiction;
                }
            }

            observations += 1;
        };

        if status == WorldStatus::Deterministic {
            found_deterministic_result = true;
        }

        attempts += 1;
    }

    let output_file_path = input_file_path.with_file_name(format!(
        "{}_{}_{}.txt",
        input_file_stem.to_string_lossy(),
        if found_deterministic_result {
            "deterministic"
        } else {
            "contradiction"
        },
        chrono::Local::now().format("%Y-%m-%d-%H-%M-%S"),
    ));

    eprintln!("Exporting result to {}", output_file_path.to_string_lossy());
    let output_file = match File::create(output_file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to create output file: {}", err);
            process::exit(1);
        }
    };

    let mut output_writer = BufWriter::new(output_file);
    text_io::export_world_state(&mut output_writer, &world, &import_result.module_to_name);

    match output_writer.flush() {
        Ok(()) => (),
        Err(err) => {
            eprintln!("Failed to flush output: {}", err);
            process::exit(1);
        }
    }
}

fn parse_number_literal(input: &str) -> Result<u128, ParseIntError> {
    if input.starts_with("0x") || input.starts_with("0X") {
        u128::from_str_radix(&input[2..], 16)
    } else {
        u128::from_str_radix(input, 10)
    }
}
