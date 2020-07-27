/*

ROADMAP:

- 3D GH component (takes rule lists and returns grid)
- 2D Overlapped solver
- 2D Overlapped import
- Visualizer app

*/

mod convert;
mod tiled2d_image_io;
mod tiled2d_wfc;
mod tiled3d_text_io;
mod tiled3d_wfc;

use std::collections::HashSet;
use std::ffi::OsString;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write as _};
use std::path::Path;
use std::process;
use std::time::SystemTime;

use clap::Clap as _;
use rand::SeedableRng as _;
use tinyvec::ArrayVec;

use crate::tiled2d_image_io::Tiled2dImageImportOptions;
use crate::tiled2d_wfc::{Tiled2dObserveResult, Tiled2dWorld};
use crate::tiled3d_wfc::{Tiled3dObserveResult, Tiled3dWorld};

#[derive(clap::Clap)]
#[clap(name = "WFC", version, author)]
struct Options {
    /// The seed to initalize the random number generator.
    #[clap(long, default_value = "265923619194138669916783960638766038768")]
    pub random_seed: u128,
    /// How many times can the world be reset to initial state when looking for
    /// a deterministic solution.
    #[clap(long, default_value = "100")]
    pub max_attempts: u32,
    #[clap(long)]
    /// Whether the world has a wrapping topology.
    pub wrapping: bool,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(clap::Clap)]
enum Subcommand {
    /// Generate an image based on an input image.
    Tiled2dImage(Tiled2dImageOptions),
    /// Generate textual voxels based on CSV rules and (optional) input state in
    /// textual voxels.
    Tiled3dText(Tiled3dTextOptions),
}

#[derive(clap::Clap)]
struct Tiled2dImageOptions {
    /// Path to input image file.
    #[clap(parse(from_os_str))]
    pub input: OsString,
    /// X world dimension.
    #[clap(long, short = "x", default_value = "10")]
    pub x_size: u16,
    /// Y world dimension.
    #[clap(long, short = "y", default_value = "10")]
    pub y_size: u16,
    /// Whether configurations found in the input image can rotated to augment
    /// the input rules.
    #[clap(long)]
    pub allow_rotate: bool,
}

#[derive(clap::Clap)]
struct Tiled3dTextOptions {
    /// Path to input CSV file.
    #[clap(parse(from_os_str))]
    pub input: OsString,
    /// Path to initial state of the world in textual form.
    #[clap(long, parse(from_os_str))]
    pub initial_state: Option<OsString>,
    /// X world dimension.
    #[clap(long, short = "x", default_value = "10")]
    pub x_size: u16,
    /// Y world dimension.
    #[clap(long, short = "y", default_value = "10")]
    pub y_size: u16,
    /// Z world dimension.
    #[clap(long, short = "z", default_value = "10")]
    pub z_size: u16,
}

fn main() {
    env_logger::init();

    let options = Options::parse();
    let mut rng = rand_pcg::Pcg32::from_seed(options.random_seed.to_le_bytes());

    match options.subcommand {
        Subcommand::Tiled2dImage(tiled2d_image_options) => {
            run_tiled2d_image(
                &mut rng,
                options.max_attempts,
                options.wrapping,
                &tiled2d_image_options,
            );
        }
        Subcommand::Tiled3dText(tiled3d_text_options) => {
            run_tiled3d_text(
                &mut rng,
                options.max_attempts,
                options.wrapping,
                &tiled3d_text_options,
            );
        }
    }
}

fn run_tiled2d_image(
    rng: &mut rand_pcg::Pcg32,
    max_attempts: u32,
    wrapping: bool,
    options: &Tiled2dImageOptions,
) {
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

    let import_result = match tiled2d_image_io::import_tiled_image(
        input_file,
        &Tiled2dImageImportOptions {
            allow_rotate: options.allow_rotate,
        },
    ) {
        Ok(import_result) => import_result,
        Err(err) => {
            eprintln!("Failed to extract chunks from png: {}", err);
            process::exit(1);
        }
    };

    eprintln!("Found {} adjacency rules:", import_result.adjacencies.len());
    for adjacency in &import_result.adjacencies {
        eprintln!("{}", adjacency);
    }

    let mut world = Tiled2dWorld::new(
        [options.x_size, options.y_size],
        import_result.adjacencies,
        wrapping,
    );

    let mut found_deterministic_result = false;
    let mut attempts = 0;
    while attempts < max_attempts && !found_deterministic_result {
        let mut observations = 0;
        world.reset();

        let result = loop {
            let result = world.observe(rng);

            match result {
                Tiled2dObserveResult::Nondeterministic => {
                    if observations % 100 == 0 {
                        eprintln!(
                            "attempt: {:>4}, observation: {:>4}, {} (In progress)",
                            attempts, observations, result,
                        );
                    }
                }
                Tiled2dObserveResult::Deterministic => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, result,
                    );

                    break Tiled2dObserveResult::Deterministic;
                }
                Tiled2dObserveResult::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, result,
                    );

                    break Tiled2dObserveResult::Contradiction;
                }
            }

            observations += 1;
        };

        if result == Tiled2dObserveResult::Deterministic {
            found_deterministic_result = true;
        }

        attempts += 1;
    }

    let time_finished = SystemTime::now();
    let time_since_epoch = time_finished
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();

    let output_file_path = input_file_path.with_file_name(format!(
        "{}_{}_{}.png",
        input_file_stem.to_string_lossy(),
        if found_deterministic_result {
            "deterministic"
        } else {
            "contradiction"
        },
        time_since_epoch.as_secs()
    ));

    eprintln!("Exporting result to {}", output_file_path.to_string_lossy());

    let slots = world.export_slots();
    let output_file = match File::create(output_file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to create output file: {}", err);
            process::exit(1);
        }
    };

    let mut output_writer = BufWriter::new(output_file);
    tiled2d_image_io::export_tiled_image(
        &mut output_writer,
        options.x_size,
        options.y_size,
        &slots,
        &import_result.module_to_chunk,
    );

    match output_writer.flush() {
        Ok(()) => (),
        Err(err) => {
            eprintln!("Failed to flush output: {}", err);
            process::exit(1);
        }
    }
}

fn run_tiled3d_text(
    rng: &mut rand_pcg::Pcg32,
    max_attempts: u32,
    wrapping: bool,
    options: &Tiled3dTextOptions,
) {
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

    let adjacencies = match tiled3d_text_io::import_adjacency_rules(input_file) {
        Ok(adjacencies) => adjacencies,
        Err(err) => {
            eprintln!("Failed to extract adjacency rules from text: {}", err);
            process::exit(1);
        }
    };

    eprintln!("Found {} adjacency rules:", adjacencies.len());
    for adjacency in &adjacencies {
        eprintln!("{}", adjacency);
    }

    let dims = [options.x_size, options.y_size, options.z_size];

    let initial_state = if let Some(initial_state_file_str) = options.initial_state.as_ref() {
        let initial_state_file_path = Path::new(initial_state_file_str);
        let initial_state_file = match File::open(initial_state_file_path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("Failed to open initial state file: {}", err);
                process::exit(1);
            }
        };

        let all_module_ids: HashSet<u32> = adjacencies
            .iter()
            .flat_map(|adj| ArrayVec::from([adj.module_low, adj.module_high]).into_iter())
            .collect();

        let buf_reader = BufReader::new(initial_state_file);
        let initial_state =
            match tiled3d_text_io::import_initial_state(buf_reader, dims, &all_module_ids) {
                Ok(initial_state) => initial_state,
                Err(err) => {
                    eprintln!("Failed to extract initial state from text: {}", err);
                    process::exit(1);
                }
            };

        Some(initial_state)
    } else {
        None
    };

    let mut world = Tiled3dWorld::new(dims, adjacencies, wrapping);

    let mut found_deterministic_result = false;
    let mut attempts = 0;
    while attempts < max_attempts && !found_deterministic_result {
        let mut observations = 0;

        if let Some(initial_state) = &initial_state {
            world.reset_to_initial_state(initial_state);
        } else {
            world.reset();
        }

        let result = loop {
            let result = world.observe(rng);

            match result {
                Tiled3dObserveResult::Nondeterministic => {
                    if observations % 100 == 0 {
                        eprintln!(
                            "attempt: {:>4}, observation: {:>4}, {} (In progress)",
                            attempts, observations, result,
                        );
                    }
                }
                Tiled3dObserveResult::Deterministic => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, result,
                    );

                    break Tiled3dObserveResult::Deterministic;
                }
                Tiled3dObserveResult::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {}",
                        attempts, observations, result,
                    );

                    break Tiled3dObserveResult::Contradiction;
                }
            }

            observations += 1;
        };

        if result == Tiled3dObserveResult::Deterministic {
            found_deterministic_result = true;
        }

        attempts += 1;
    }

    let time_finished = SystemTime::now();
    let time_since_epoch = time_finished
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();

    let output_file_path = input_file_path.with_file_name(format!(
        "{}_{}_{}.txt",
        input_file_stem.to_string_lossy(),
        if found_deterministic_result {
            "deterministic"
        } else {
            "contradiction"
        },
        time_since_epoch.as_secs()
    ));

    eprintln!("Exporting result to {}", output_file_path.to_string_lossy());

    let slots = world.export_slots();
    let output_file = match File::create(output_file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to create output file: {}", err);
            process::exit(1);
        }
    };

    let mut output_writer = BufWriter::new(output_file);
    tiled3d_text_io::export_tiled_text(
        &mut output_writer,
        [options.x_size, options.y_size, options.z_size],
        &slots,
    );

    match output_writer.flush() {
        Ok(()) => (),
        Err(err) => {
            eprintln!("Failed to flush output: {}", err);
            process::exit(1);
        }
    }
}
