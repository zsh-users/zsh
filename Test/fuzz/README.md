# Fuzz Testing for Zsh

This directory contains fuzz targets for zsh's parsing subsystems,
designed for use with [libFuzzer](https://llvm.org/docs/LibFuzzer.html)
and [OSS-Fuzz](https://github.com/google/oss-fuzz).

## Fuzz targets

| Target | Description |
|--------|-------------|
| `fuzz_zsh_pattern` | Fuzzes zsh-style pattern/glob matching with character classes, wildcards, and escapes |

## Building locally

```bash
clang -g -fsanitize=fuzzer,address fuzz_zsh_pattern.c -o fuzz_zsh_pattern
./fuzz_zsh_pattern corpus/
```

## OSS-Fuzz integration

These targets are continuously fuzzed via Google's OSS-Fuzz infrastructure.
