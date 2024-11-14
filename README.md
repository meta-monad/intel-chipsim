# Intel Chipsim

The goal of this project is to to simulate legacy Intel CPUs. The current focus is the intel 8008, but in the future it may expand to include the 8080 and 8086. Some information regarding the intel 8008's operation can be found under [i8008.md](i8008.md).

## Usage

The project is written in Rust in its entirety. While the chip's logic is separate, it is integrated with memory and some very simplistic control logic.

To use it, invoke the command like:
```sh
cargo run -- --arch i8008
```
Or alternatively with the executable:
```sh
intel-chipsim --arch i8008
```
Additional usage information on the interactive shell can be found by entering `help` into the CLI. More help on the program's usage can also be found by passing the `--help` flag.

## Project structure

The overall structure of the codebase is as follows:
```text
.
└── src
    ├── assembler.rs
    ├── commands.rs
    ├── i8008.rs
    ├── main.rs
    └── utils.rs
```

The aim of this endeveaour was a truthful implementation of the Intel 8008 microcontroller, accordingly the chip's entire state is represented within a struct inside `src/i8008.rs`. This file is responsible for actually simulating the state transitions that that chip *should* experience by implementing methods on the struct. The bulk of that logic is located within the `step` method, which looks at the current *internal state* (alongside the indicated cycle) and proceeds accordingly.

It's important to point out that when the chip will signal that it has entered a particular state, it means that it's ready to decode that state by the next call of the `step` function. This is due to architectural reasons rather than historical ones as the `step` function works in the following manner:
1. decode cycle and state
2. processing
3. set new state (there is a choice)

Other files include the assembler located in `assembler.rs`, which is a primitive 2-pass assembler that provides errors and warnings as well as a few assembler directives. As well as `commands.rs`, which decodes and potentially executes commands supplied in the interactive shell. Conceptually, the two files are very similar with data structures for a well-formed term for their context (an instruction/a command), errors, and function `parse :: [Tokens] -> Either Term Error` and `runTerm :: Term -> ()`.

The other two files - `main.rs` and `utils.rs` - are mostly just wrapper functions.
