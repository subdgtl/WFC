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

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufWriter, Write as _};
use std::path::Path;
use std::process;

use clap::Clap as _;
use rand::SeedableRng as _;

use crate::tiled2d_image_io::Tiled2dImageImportOptions;
use crate::tiled2d_wfc::{Tiled2dObserveResult, Tiled2dWorld};
use crate::tiled3d_wfc::{Tiled3dObserveResult, Tiled3dWorld};

#[derive(clap::Clap)]
#[clap(name = "WFC", version, author)]
struct Options {
    #[clap(long, default_value = "265923619194138669916783960638766038768")]
    pub random_seed: u128,
    #[clap(long, default_value = "10")]
    pub max_attempts: u32,
    #[clap(long)]
    pub wrapping: bool,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(clap::Clap)]
enum Subcommand {
    Tiled2dImage(Tiled2dImageOptions),
    Tiled3dText(Tiled3dTextOptions),
}

#[derive(clap::Clap)]
struct Tiled2dImageOptions {
    #[clap(parse(from_os_str))]
    pub input: OsString,
    #[clap(long, default_value = "10")]
    pub x_size: u16,
    #[clap(long, default_value = "10")]
    pub y_size: u16,
    #[clap(long)]
    pub allow_rotate: bool,
}

#[derive(clap::Clap)]
struct Tiled3dTextOptions {
    #[clap(parse(from_os_str))]
    pub input: OsString,
    #[clap(long, default_value = "10")]
    pub x_size: u16,
    #[clap(long, default_value = "10")]
    pub y_size: u16,
    #[clap(long, default_value = "10")]
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
    let mut observations = 0;
    while attempts < max_attempts && !found_deterministic_result {
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
                        "attempt: {:>4}, observation: {:>4}, {} (Found final result)",
                        attempts, observations, result,
                    );

                    break Tiled2dObserveResult::Deterministic;
                }
                Tiled2dObserveResult::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {} (Discarding contradictory result)",
                        attempts, observations, result,
                    );

                    break Tiled2dObserveResult::Contradiction;
                }
            }

            observations += 1;
        };

        if result == Tiled2dObserveResult::Deterministic {
            found_deterministic_result = true;
        } else {
            observations = 0;
            world.reset();
        }

        attempts += 1;
    }

    if found_deterministic_result {
        let output_file_path = input_file_path
            .with_file_name(format!("{}_output.png", input_file_stem.to_string_lossy()));

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

    let adjacencies = match tiled3d_text_io::import_tiled_csv(input_file) {
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

    let mut world = Tiled3dWorld::new(
        [options.x_size, options.y_size, options.z_size],
        adjacencies,
        wrapping,
    );

    let mut found_deterministic_result = false;
    let mut attempts = 0;
    let mut observations = 0;
    while attempts < max_attempts && !found_deterministic_result {
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
                        "attempt: {:>4}, observation: {:>4}, {} (Found final result)",
                        attempts, observations, result,
                    );

                    break Tiled3dObserveResult::Deterministic;
                }
                Tiled3dObserveResult::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {} (Discarding contradictory result)",
                        attempts, observations, result,
                    );

                    break Tiled3dObserveResult::Contradiction;
                }
            }

            observations += 1;
        };

        if result == Tiled3dObserveResult::Deterministic {
            found_deterministic_result = true;
        } else {
            observations = 0;
            world.reset();
        }

        attempts += 1;
    }

    if found_deterministic_result {
        let output_file_path = input_file_path
            .with_file_name(format!("{}_output.txt", input_file_stem.to_string_lossy()));

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
}
