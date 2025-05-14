# RISC-V ISA Semantics for Lean

These semantics are generated from the official RISC-V SPEC available at
https://github.com/riscv/sail-riscv/.

⚠️ While this repository covers the full RISC-V SPEC, our Lean backend for sail
is still work-in-progress. As a result, our semantics are still full of warnings
and errors. Similarly, our output is not yet polished for readability.

## How to build this model

This repository generates automatically the RISC-V Lean model from the official Sail model. To generate it locally, follow the following steps.

### Build Sail

The released version of Sail does not support Lean output, so we need to build
Sail from its git repository. All the details are given in the [official
instructions](https://github.com/rems-project/sail/blob/sail2/INSTALL.md#installing-development-versions-of-sail).
What follows here are a summary.

#### Install opan using your package manager
#### Install the depdencies

On Debian/Ubuntu
```
sudo apt-get install build-essential libgmp-dev z3 pkg-config
```
or on MacOS
```
brew install gmp z3 pkgconf
```

#### Install sail
```
git clone https://github.com/rems-project/sail.git
```

Using opam:
```
opam pin add sail
```

With dune, inside the Sail root directory:
```
opam install . --deps-only
dune build --release
dune install
```

### Build the RISC-V model

To build the RISC-V model, first clone the repository
```
git clone https://github.com/riscv/sail-riscv.git
```

and then build it using the cmake:
```
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build build/ --target generated_lean_rv64d
```

The resulting Lean model will be in `build/model/Lean_RV64D` and can be built using `lake build`

# Statistics

Lines: 129791  
Definitions: 3966  
Inductive definitions: 165  
Abbreviations: 143  

# Warnings and Errors

Errors found: 0  
Warnings found: 1  

## Error Classes

