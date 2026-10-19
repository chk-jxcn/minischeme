# MiniScheme Core Interpreter & Compiler

MiniScheme is a small, embeddable, ChezScheme-flavoured Scheme: a reader, a compiler to bytecode and a bytecode virtual machine in strict ISO C99. It runs the apps of the MiniScheme ESP32 desk display ([minischeme-esp32](https://github.com/chk-jxcn/minischeme-esp32)) and the WebREPL's WASM simulator ([minischeme-webrepl](https://github.com/chk-jxcn/minischeme-webrepl)).

## Features
- **ISO C99**: zero warnings under `-Wall -Wextra -pedantic`; CI builds with `-Werror` (gcc, clang, 32-bit).
- **Small heap**: generational GC: a 4 KB Eden copied (Cheney) into the tenured heap on a minor collection, and the tenured heap compacted in place (sliding mark-compact) on a full one, so no semispace is wasted. On the ESP32 the VM gets about 102 KB.
- **Bignums**: arbitrary-precision integers.
- **Macros and tail calls**: `define-syntax` / `syntax-rules`, `define-macro`, proper tail calls.
- **MSBC bytecode**: portable images that run in place from flash (XIP); constants and symbols stay in ROM.
- **Optional stack traces**: `MiniConfig.enable_stack_trace` records the call stack on errors (1 KB, allocated only while enabled).
- **Targets**: Windows (MinGW), Linux, macOS, WebAssembly (`wasm32`), ESP32 and other microcontrollers.

## Building and testing

```bash
make            # libminischeme.a (no main) and the minischeme command line
make test       # checks the command line's answers, then runs the MSBC,
                # compiler/VM and stress suites (each fails on any failed check)
make M32=-m32 test        # the 32-bit device profile
make WERROR= all          # without -Werror
```
On Windows with MinGW: `mingw32-make SHELL=sh CC=gcc test`.

`make test` first builds `tools/gen_msbc.c`, which compiles `scm/init.scm` and `scm/compiler.scm` into the MSBC images the suites load (`build/init_msbc.h`, `build/compiler_msbc.h`).

The GUI, WiFi and peripheral primitives are not part of this core (it has stubs); their tests run with the device sources (`-DMINISCHEME_DEVICE_TESTS`).

### Running expressions
```bash
./minischeme -e "(display (+ 10 (* 3 5)))"
```

## Repository structure
- `include/`: public and internal headers (`minischeme.h`, `minischeme_internal.h`, `msbc.h`, ...).
- `src/`: VM, GC, reader, primitives, bignums, FASL/MSBC.
- `scm/`: `init.scm` (runtime library) and `compiler.scm` (the Scheme compiler).
- `tests/`: `test_msbc.c`, `test_compiler.c`, `test_stress.c`.
- `tools/gen_msbc.c`: generator of the test suites' MSBC images.

See [AGENT_DEVELOPMENT_GUIDE.md](AGENT_DEVELOPMENT_GUIDE.md) for the architecture across the five repositories.

## License
MIT License
