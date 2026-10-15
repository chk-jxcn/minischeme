# MiniScheme Core Interpreter & Compiler

MiniScheme is a lightweight, embeddable, ChezScheme-compatible Scheme interpreter and bytecode virtual machine written in strict ISO C99.

## Features
- **Strict ISO C99 Compliance**: Clean compilation under `-Wall -Wextra` with zero warnings.
- **Ultra-low RAM Footprint**: Generational Cheney copying garbage collection (4 KB Eden + configurable Tenured heap), sliding mark-compact for full collections.
- **Arbitrary Precision Arithmetic**: Built-in BigInt math supporting arbitrarily large numbers.
- **Hygienic Macros & First-Class Closures**: Macro expansion via `define-syntax` and tail-call optimization (TCO).
- **FASL / MSBC Bytecode Virtual Machine**: Fast streaming deserialization and Execute-In-Place (XIP) bytecode execution.
- **Cross-Platform**: Windows (MinGW/MSYS), Linux (GCC/Clang), macOS, WebAssembly (`wasm32-unknown-unknown`), and microcontroller RTOS (ESP32, RP2040, STM32).

## Building

### Quick Build with Makefile
```bash
make all
```

### Build with CMake
```bash
mkdir build && cd build
cmake ..
cmake --build .
```

### Running REPL
```bash
./minischeme
```

### Running Scheme Expressions
```bash
./minischeme -e "(display (+ 10 (* 3 5)))"
```

## Repository Structure
- `include/`: Public API headers (`minischeme.h`, `minischeme_internal.h`, etc.).
- `src/`: Core virtual machine, garbage collector, reader, primitives, and BigInt arithmetic.
- `scm/`: Core Scheme libraries (`init.scm`, `compiler.scm`).
- `tests/`: Regression and stress test suites.

## License
MIT License
