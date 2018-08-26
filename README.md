# ggv-checker

This is a type checker for an external language of Gradual GV (GGV).

There are some sample programs in `examples/` directory.

## Requirements

- Menhir

## Build instructions

```sh
cd src
omake
./tycheck
```

## References

- Atsushi Igarashi, Peter Thiemann, Vasco T. Vasconcelos, and Philip Wadler.
  Gradual session types. ICFP 2017.
