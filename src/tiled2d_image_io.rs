use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::error;
use std::fmt;
use std::io;
use std::mem;

use crate::convert::{cast_u32, cast_usize};
use crate::tiled2d_wfc::{Tiled2dAdjacency, Tiled2dAdjacencyKind};

pub const PIXEL_SAMPLES: usize = 3;
pub const CHUNK_SIZE: usize = 3;

#[derive(Debug)]
pub struct Tiled2dImageImportOptions {
    pub allow_rotate: bool,
    // FIXME: Implement mirroring. Decide whether the resulting combinatorial
    // explosion should be controlled somehow (e.g. has to be specifically
    // allowed)
    // pub allow_mirror_horizontal: bool,
    // pub allow_mirror_vertical: bool,
}

#[derive(Debug)]
pub struct Tiled2dImageImportResult {
    pub adjacencies: Vec<Tiled2dAdjacency>,
    pub module_to_chunk: HashMap<u32, [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE]>,
}

#[derive(Debug)]
pub enum Tiled2dImageImportError {
    ImageTooSmall,
    ImageColorTypeUnsupported(png::ColorType),
    Decoding(png::DecodingError),
}

impl fmt::Display for Tiled2dImageImportError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ImageTooSmall => write!(f, "Image too small for chosen chunk size"),
            Self::ImageColorTypeUnsupported(color_type) => {
                write!(f, "Image color type not supported: {:?}", color_type)
            }
            Self::Decoding(decoding_error) => write!(f, "{}", decoding_error),
        }
    }
}

impl error::Error for Tiled2dImageImportError {}

impl From<png::DecodingError> for Tiled2dImageImportError {
    fn from(decoding_error: png::DecodingError) -> Tiled2dImageImportError {
        Self::Decoding(decoding_error)
    }
}

pub fn import_tiled_image<R: io::Read>(
    r: R,
    options: &Tiled2dImageImportOptions,
) -> Result<Tiled2dImageImportResult, Tiled2dImageImportError> {
    log::info!("Importing PNG...");
    log::info!("Decoding PNG...");

    let decoder = png::Decoder::new(r);
    let (info, mut reader) = decoder.read_info()?;

    log::info!("PNG color type {:?}", info.color_type);
    if info.color_type != png::ColorType::RGB {
        return Err(Tiled2dImageImportError::ImageColorTypeUnsupported(
            info.color_type,
        ));
    }

    let mut buffer = vec![0; info.buffer_size()];

    // Read the next frame. An APNG might contain multiple frames.
    reader.next_frame(&mut buffer).unwrap();

    let pixel_width = cast_usize(info.width);
    let pixel_height = cast_usize(info.height);
    let chunk_width = pixel_width / CHUNK_SIZE;
    let chunk_height = pixel_height / CHUNK_SIZE;

    log::info!("Pixel dimensions {}x{}", pixel_width, pixel_height);
    log::info!("Chunk dimensions {}x{}", chunk_width, chunk_height);

    if chunk_width * CHUNK_SIZE != pixel_width || chunk_height * CHUNK_SIZE != pixel_width {
        log::warn!(
            "Image dimensions not multiple of chunk size ({}) ({}x{}), ignoring borders",
            CHUNK_SIZE,
            pixel_width,
            pixel_height,
        );
    }

    let mut next_id = 0_u32;
    let mut adjacencies = HashSet::new();
    let mut module_to_chunk = HashMap::new();
    let mut chunk_to_module = HashMap::new();

    let mut ids = vec![0; chunk_width * chunk_height];
    for y in 0..chunk_height {
        for x in 0..chunk_width {
            let chunk = read_chunk(x, y, pixel_width, pixel_height, &buffer);

            match chunk_to_module.entry(chunk) {
                Entry::Occupied(occupied_entry) => {
                    ids[x + y * chunk_width] = *occupied_entry.get();
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(next_id);
                    module_to_chunk.insert(next_id, chunk);

                    ids[x + y * chunk_width] = next_id;

                    next_id += 1;
                }
            }

            if options.allow_rotate {
                let chunk90 = rotate_chunk_ccw(chunk);
                if let Entry::Vacant(vacant_entry) = chunk_to_module.entry(chunk90) {
                    vacant_entry.insert(next_id);
                    module_to_chunk.insert(next_id, chunk90);

                    next_id += 1;
                }

                let chunk180 = rotate_chunk_ccw(chunk90);
                if let Entry::Vacant(vacant_entry) = chunk_to_module.entry(chunk180) {
                    vacant_entry.insert(next_id);
                    module_to_chunk.insert(next_id, chunk180);

                    next_id += 1;
                }

                let chunk270 = rotate_chunk_ccw(chunk180);
                if let Entry::Vacant(vacant_entry) = chunk_to_module.entry(chunk270) {
                    vacant_entry.insert(next_id);
                    module_to_chunk.insert(next_id, chunk270);

                    next_id += 1;
                }
            }
        }
    }

    for y in 0..chunk_height {
        for x in 0..chunk_width {
            let low = ids[x.wrapping_sub(1) % chunk_width + y * chunk_width];
            let high = ids[x + y * chunk_width];

            adjacencies.insert(Tiled2dAdjacency {
                kind: Tiled2dAdjacencyKind::X,
                module_low: low,
                module_high: high,
            });

            if options.allow_rotate {
                let chunk_low = module_to_chunk[&low];
                let chunk_high = module_to_chunk[&high];

                let chunk_low90 = rotate_chunk_ccw(chunk_low);
                let chunk_high90 = rotate_chunk_ccw(chunk_high);
                let low90 = chunk_to_module[&chunk_low90];
                let high90 = chunk_to_module[&chunk_high90];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::Y,
                    module_low: low90,
                    module_high: high90,
                });

                let chunk_low180 = rotate_chunk_ccw(chunk_low90);
                let chunk_high180 = rotate_chunk_ccw(chunk_high90);
                let low180 = chunk_to_module[&chunk_low180];
                let high180 = chunk_to_module[&chunk_high180];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::X,
                    module_low: high180,
                    module_high: low180,
                });

                let chunk_low270 = rotate_chunk_ccw(chunk_low180);
                let chunk_high270 = rotate_chunk_ccw(chunk_high180);
                let low270 = chunk_to_module[&chunk_low270];
                let high270 = chunk_to_module[&chunk_high270];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::Y,
                    module_low: high270,
                    module_high: low270,
                });
            }
        }
    }

    for x in 0..chunk_width {
        for y in 0..chunk_height {
            // Reverse high/low, because the solver works in a right-handed system
            let high = ids[x + (y.wrapping_sub(1) % chunk_height) * chunk_width];
            let low = ids[x + y * chunk_width];

            adjacencies.insert(Tiled2dAdjacency {
                kind: Tiled2dAdjacencyKind::Y,
                module_low: low,
                module_high: high,
            });

            if options.allow_rotate {
                let chunk_low = module_to_chunk[&low];
                let chunk_high = module_to_chunk[&high];

                let chunk_low90 = rotate_chunk_ccw(chunk_low);
                let chunk_high90 = rotate_chunk_ccw(chunk_high);
                let low90 = chunk_to_module[&chunk_low90];
                let high90 = chunk_to_module[&chunk_high90];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::X,
                    module_low: high90,
                    module_high: low90,
                });

                let chunk_low180 = rotate_chunk_ccw(chunk_low90);
                let chunk_high180 = rotate_chunk_ccw(chunk_high90);
                let low180 = chunk_to_module[&chunk_low180];
                let high180 = chunk_to_module[&chunk_high180];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::Y,
                    module_low: high180,
                    module_high: low180,
                });

                let chunk_low270 = rotate_chunk_ccw(chunk_low180);
                let chunk_high270 = rotate_chunk_ccw(chunk_high180);
                let low270 = chunk_to_module[&chunk_low270];
                let high270 = chunk_to_module[&chunk_high270];
                adjacencies.insert(Tiled2dAdjacency {
                    kind: Tiled2dAdjacencyKind::X,
                    module_low: low270,
                    module_high: high270,
                });
            }
        }
    }

    // If we wanted to input 2d adjacencies to the 3d solver, we would need to
    // enrich the adjacencies such that they could always connect themselves

    // for id in module_to_chunk.keys() {
    //     adjacencies.insert(Tiled3Adjacency {
    //         kind: Tiled3AdjacencyKind::Z,
    //         module_low: *id,
    //         module_high: *id,
    //     });
    // }

    Ok(Tiled2dImageImportResult {
        adjacencies: adjacencies.into_iter().collect(),
        module_to_chunk,
    })
}

// FIXME: Error handling for writing

pub fn export_tiled_image<W: io::Write>(
    w: &mut W,
    chunk_width: u16,
    chunk_height: u16,
    slots: &[Option<u32>],
    module_to_chunk: &HashMap<u32, [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE]>,
) {
    const ZERO_CHUNK: [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] =
        [[255, 0, 255]; CHUNK_SIZE * CHUNK_SIZE];

    let pixel_width = cast_u32(usize::from(chunk_width) * CHUNK_SIZE);
    let pixel_height = cast_u32(usize::from(chunk_height) * CHUNK_SIZE);

    let mut buffer = vec![0; cast_usize(pixel_width) * cast_usize(pixel_height) * PIXEL_SAMPLES];

    for (i, slot) in slots.iter().enumerate() {
        let x = i % usize::from(chunk_width);
        let y = i / usize::from(chunk_width);

        let chunk = slot
            .and_then(|s| module_to_chunk.get(&s))
            .unwrap_or(&ZERO_CHUNK);

        write_chunk(
            x,
            y,
            cast_usize(pixel_width),
            cast_usize(pixel_height),
            *chunk,
            &mut buffer,
        );
    }

    let mut encoder = png::Encoder::new(w, pixel_width, pixel_height);
    encoder.set_color(png::ColorType::RGB);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().unwrap();

    writer.write_image_data(&buffer).unwrap();
}

// FIXME: Expand to both RGB and RGBA

// fn read_chunk2(
//     pixel_x: usize,
//     pixel_y: usize,
//     pixel_width: usize,
//     pixel_height: usize,
//     buffer: &[u8],
// ) -> [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] {
//     debug_assert!(x < pixel_width);
//     debug_assert!(y < pixel_height);

//     let x1 = x;
//     let x2 = x.wrapping_add(1) % pixel_width;
//     let x3 = x.wrapping_add(2) % pixel_width;

//     let y1 = y;
//     let y2 = y.wrapping_add(1) % pixel_height;
//     let y3 = y.wrapping_add(2) % pixel_height;

//     let mut result = [[0; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE];
//     for y_offset in 0..3 {
//         let y = pixel_y.wrapping_add(y_offset) % pixel_height;

//         for x_offset in 0..3 {
//             let x = pixel_x.wrapping_add(x_offset) % pixel_width;

//             let index_base = x + y * pixel_width;
//             for (i, index) in index_base..(index_base + PIXEL_SAMPLES).iter().enumerate() {
//                 result[x_offset + y_offset * CHUNK_SIZE][i] = buffer[index];
//             }
//         }
//     }

//     result
// }

fn read_chunk(
    chunk_x: usize,
    chunk_y: usize,
    pixel_width: usize,
    pixel_height: usize,
    buffer: &[u8],
) -> [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] {
    debug_assert!(chunk_x < pixel_width / CHUNK_SIZE);
    debug_assert!(chunk_y < pixel_height / CHUNK_SIZE);

    let row1_begin =
        chunk_x * CHUNK_SIZE * PIXEL_SAMPLES + pixel_width * chunk_y * CHUNK_SIZE * PIXEL_SAMPLES;
    let row1_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * chunk_y * CHUNK_SIZE * PIXEL_SAMPLES;

    let row2_begin = chunk_x * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 1) * PIXEL_SAMPLES;
    let row2_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 1) * PIXEL_SAMPLES;

    let row3_begin = chunk_x * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 2) * PIXEL_SAMPLES;
    let row3_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 2) * PIXEL_SAMPLES;

    log::trace!(
        "Read chunk [{:>3},{:>3}] R1 {:>3} .. {:>3} R2: {:>3} .. {:>3} R3: {:>3} .. {:>3}",
        chunk_x,
        chunk_y,
        row1_begin,
        row1_end,
        row2_begin,
        row2_end,
        row3_begin,
        row3_end,
    );

    let mut result = [0; PIXEL_SAMPLES * CHUNK_SIZE * CHUNK_SIZE];

    for (i, image_i) in (row1_begin..row1_end).enumerate() {
        result[i] = buffer[image_i];
    }

    for (i, image_i) in (row2_begin..row2_end).enumerate() {
        result[PIXEL_SAMPLES * CHUNK_SIZE + i] = buffer[image_i];
    }

    for (i, image_i) in (row3_begin..row3_end).enumerate() {
        result[PIXEL_SAMPLES * CHUNK_SIZE * 2 + i] = buffer[image_i];
    }

    // FIXME: Be safe!
    unsafe { mem::transmute(result) }
}

fn write_chunk(
    chunk_x: usize,
    chunk_y: usize,
    pixel_width: usize,
    pixel_height: usize,
    chunk: [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE],
    buffer: &mut [u8],
) {
    debug_assert!(chunk_x < pixel_width / CHUNK_SIZE);
    debug_assert!(chunk_y < pixel_height / CHUNK_SIZE);

    let row1_begin =
        chunk_x * CHUNK_SIZE * PIXEL_SAMPLES + pixel_width * chunk_y * CHUNK_SIZE * PIXEL_SAMPLES;
    let row1_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * chunk_y * CHUNK_SIZE * PIXEL_SAMPLES;

    let row2_begin = chunk_x * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 1) * PIXEL_SAMPLES;
    let row2_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 1) * PIXEL_SAMPLES;

    let row3_begin = chunk_x * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 2) * PIXEL_SAMPLES;
    let row3_end = (chunk_x + 1) * CHUNK_SIZE * PIXEL_SAMPLES
        + pixel_width * (chunk_y * CHUNK_SIZE + 2) * PIXEL_SAMPLES;

    log::trace!(
        "Write chunk [{:>3},{:>3}] R1 {:>3} .. {:>3} R2: {:>3} .. {:>3} R3: {:>3} .. {:>3}",
        chunk_x,
        chunk_y,
        row1_begin,
        row1_end,
        row2_begin,
        row2_end,
        row3_begin,
        row3_end,
    );

    // FIXME: Be safe!
    let chunk_flat: [u8; PIXEL_SAMPLES * CHUNK_SIZE * CHUNK_SIZE] =
        unsafe { mem::transmute(chunk) };

    for (i, image_i) in (row1_begin..row1_end).enumerate() {
        buffer[image_i] = chunk_flat[i];
    }

    for (i, image_i) in (row2_begin..row2_end).enumerate() {
        buffer[image_i] = chunk_flat[PIXEL_SAMPLES * CHUNK_SIZE + i];
    }

    for (i, image_i) in (row3_begin..row3_end).enumerate() {
        buffer[image_i] = chunk_flat[PIXEL_SAMPLES * CHUNK_SIZE * 2 + i];
    }
}

fn rotate_chunk_ccw(
    mut chunk: [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE],
) -> [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] {
    let t1 = chunk[0];
    chunk[0] = chunk[2];
    chunk[2] = chunk[8];
    chunk[8] = chunk[6];
    chunk[6] = t1;

    let t2 = chunk[1];
    chunk[1] = chunk[5];
    chunk[5] = chunk[7];
    chunk[7] = chunk[3];
    chunk[3] = t2;

    chunk
}

#[allow(dead_code)]
fn mirror_chunk_horz(
    mut chunk: [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE],
) -> [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] {
    let t0 = chunk[0];
    let t2 = chunk[2];
    chunk[0] = t2;
    chunk[2] = t0;

    let t3 = chunk[3];
    let t5 = chunk[5];
    chunk[3] = t5;
    chunk[5] = t3;

    let t6 = chunk[6];
    let t8 = chunk[8];
    chunk[6] = t8;
    chunk[8] = t6;

    chunk
}

#[allow(dead_code)]
fn mirror_chunk_vert(
    mut chunk: [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE],
) -> [[u8; PIXEL_SAMPLES]; CHUNK_SIZE * CHUNK_SIZE] {
    let t0 = chunk[0];
    let t6 = chunk[6];
    chunk[0] = t6;
    chunk[6] = t0;

    let t1 = chunk[1];
    let t7 = chunk[7];
    chunk[1] = t7;
    chunk[7] = t1;

    let t2 = chunk[2];
    let t8 = chunk[8];
    chunk[2] = t8;
    chunk[8] = t2;

    chunk
}
