/*

ROADMAP:

- Polish cli
- 2D Overlapped solver
- 2D Overlapped import
- Define data input format for 3D
- 3D Tiled import
- 3D Overlapped solver
- 2D GH component (sample image)
- 3D GH component (sample voxel image)

*/

mod convert;
mod tiled2_image_io;
mod tiled3_wfc;

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufWriter, Write as _};
use std::path::Path;
use std::process;

use clap::Clap as _;
use rand::SeedableRng as _;

use crate::tiled2_image_io::Tiled2ImageImportOptions;
use crate::tiled3_wfc::{Tiled3ObserveResult, Tiled3World};

#[derive(clap::Clap)]
#[clap(name = "WFC", version, author)]
struct Options {
    #[clap(long, default_value = "265923619194138669916783960638766038768")]
    pub random_seed: u128,
    #[clap(long, default_value = "10")]
    pub max_attempts: u32,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(clap::Clap)]
enum Subcommand {
    Tiled2(Tiled2Options),
}

#[derive(clap::Clap)]
struct Tiled2Options {
    #[clap(parse(from_os_str))]
    pub input: OsString,
    #[clap(long, default_value = "10")]
    pub x_size: u16,
    #[clap(long, default_value = "10")]
    pub y_size: u16,
    #[clap(long)]
    pub allow_rotate: bool,
}

fn main() {
    env_logger::init();

    let options = Options::parse();

    let mut rng = rand_pcg::Pcg32::from_seed(options.random_seed.to_le_bytes());
    let Subcommand::Tiled2(tiled2_options) = &options.subcommand;

    let input_file_path = Path::new(&tiled2_options.input);
    let input_file_stem = match input_file_path.file_stem() {
        Some(file_stem) => file_stem,
        None => {
            eprintln!(
                "Invalid input file name: {}",
                tiled2_options.input.to_string_lossy(),
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

    let import_result = match tiled2_image_io::import_tiled_image(
        input_file,
        &Tiled2ImageImportOptions {
            allow_rotate: tiled2_options.allow_rotate,
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

    let mut world = Tiled3World::new(
        [tiled2_options.x_size, tiled2_options.y_size, 1],
        import_result.adjacencies,
    );

    let mut found_deterministic_result = false;
    let mut attempts = 0;
    let mut observations = 0;
    while attempts < options.max_attempts && !found_deterministic_result {
        let result = loop {
            let result = world.observe(&mut rng);

            match result {
                Tiled3ObserveResult::Nondeterministic => {
                    if observations % 100 == 0 {
                        eprintln!(
                            "attempt: {:>4}, observation: {:>4}, {} (In progress)",
                            attempts, observations, result,
                        );
                    }
                }
                Tiled3ObserveResult::Deterministic => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {} (Found final result)",
                        attempts, observations, result,
                    );

                    break Tiled3ObserveResult::Deterministic;
                }
                Tiled3ObserveResult::Contradiction => {
                    eprintln!(
                        "attempt: {:>4}, observation: {:>4}, {} (Discarding contradictory result)",
                        attempts, observations, result,
                    );

                    break Tiled3ObserveResult::Contradiction;
                }
            }

            observations += 1;
        };

        if result == Tiled3ObserveResult::Deterministic {
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
        tiled2_image_io::export_tiled_image(
            &mut output_writer,
            tiled2_options.x_size,
            tiled2_options.y_size,
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
