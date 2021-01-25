# wfc_cli

A command-line prototype for exploring Wave Function Collapse. Run with `--help`
to see options.

The app has a mandatory rules CSV input file (the `<input>` positional
argument), an optional initial state TXT file (the `--initial-state` argument),
and produces an output TXT file.

## Adjacency rules CSV file

This file defines both all existing modules and their spatial relationships.

```
# Comments are behind "#"
axis,low,high # Header is optional and can be omitted

# Empty lines are ok and are ignored

# Each row looks like this:
# First row is the axis, can be either X, Y, or Z
# Second row is the name of the lower (in Z-up right-handed coordinates) module
# Third row is the name of the higher (in Z-up right-handed coordinates) module
x,thingy,stuff
y,thingy,stuff
z,thingy,stuff

x,thingy,thingy
y,thingy,thingy
z,thingy,thingy

x,stuff,stuff
y,stuff,stuff
z,stuff,stuff
```

The above file will detect 2 modules ("thingy" and "stuff") and 9 adjacency rules.

## Initial state TXT file

This file describes how the initial state of WFC looks like. If not supplied,
every slot is allowed to contain every module. The positions of slots in the
file are "visual", in a right-handed Z-up coordinate system.

- First line contains the dimensions, they must match the dimensions provided as
  commandline arguments

- Second line is a mandatory newline

- The rest of the file contains values for each module in the following spatial
  configuration:

    - Z-layers are separated by newlines and the topmost layer comes first in
      the file (RHS coords)

    - On each layer, there are `X * Y` values. X grows to the right, Y grows up
      (the first row of a layer had the highest Y - RHS coords)

    - Each value is one of:

        - `*`: The wildcard, allows every module to be present

        - `[thingy]`: A slot containing one module

        - `[thingy,another_thingy,stuff]`: A slot containing multiple (comma
          separated) modules


Even though the following example has comments, they are not supported in the file format.

```
3 3 2                          <-- First line contains the dimensions (X=3,Y=3,Z=2)
                               <-- Mandatory newline
[thingy][thingy][thingy]       <-- Values for (X=0,Y=2,Z=1), (X=1,Y=2,Z=1) and (X=2,Y=2,Z=1)
[ thingy]   [stuff][ thingy  ] <-- whitespace within a line is ok and ignored
[thingy][thingy][thingy]
                               <-- Mandatory newline separating two Z-layers
[thingy][thingy,stuff][thingy] <-- (X=1,Y=2,Z=0) has two possible values: thingy and stuff
[thingy] * [thingy]            <-- (X=1,Y=1,Z=0) has ALL possible values specified in the CSV
[thingy][thingy][thingy]
```

## Output TXT file

This file looks almost identical to the __initial state TXT file__, with two differences:

- It will never contain the wildcard (`*`) and instead all modules in a slot will be
  listed explicitly for easier parsing

- It might contain the empty slot `[]` if WFC ended in a contradictory state

If WFC found a deterministic result, each slot will contain exactly one module.
