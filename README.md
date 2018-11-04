# ggv-checker

This is a type checker for an external language GGV<sub>e</sub> of Gradual GV (GGV).

There are some sample programs in [examples](examples/) directory.

## Requirements

- OMake
- Menhir

## Build instructions

```sh
cd src
omake
./tycheck
```

## References

- Atsushi Igarashi, Peter Thiemann, Vasco T. Vasconcelos, and Philip Wadler.
  Gradual Session Types. ICFP 2017.
- https://arxiv.org/abs/1809.05649 (the extended version of the paper above)
