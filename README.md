# KogAcc
A Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

This software is a supplementary material for:
- the preprint arXiv:[2407.13116](https://arxiv.org/abs/2407.13116 "Arithmetical enhancements of the Kogbetliantz method for the SVD of order two").

## Prerequisites

Several routines and executables require having quadruple precision (`KIND=REAL128`) fully supported by the compiler.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/KogAcc`).
Then, build the `libpvn` library, with the same family of compilers and (no-)debug mode as it is meant to be used here (e.g., with `icx` if `ifx` is desired).
Please set the option `SAFE=sv2` for `libpvn`.

When building `KogAcc`, if the Intel's oneMKL library is not installed, please set the `LAPACK` variable to a Reference LAPACK installation (see the makefiles).

For now, only little-endian platforms are supported, with the `gfortran` or `ifx` compilers.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl) and [Graphviz](https://graphviz.org).
Many routines are documented only rudimentary for now.

The correctly-rounded `cr_hypot` and `cr_hypotf` functions are expected to be provided by the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Please consult the description of `libpvn` for more information.
All testing has been performed with the correctly rounded functions.
Even though it is technically feasible not to use them, this should be attempted only if necessary.

## Building

Run `make help` (GNU make assumed) in the `src` subdirectory.

Setting `NDEBUG` to, e.g., `3` is recommened.

## Running

The available Jacobi strategies:

| J |                    description |
| - | ------------------------------ |
| 0 |        row-cyclic (sequential) |
| 1 |     column-cyclic (sequential) |
| 2 | generalized Mantharam-Eberlein |
| 3 |                        dynamic |
| 4 |               modified modulus |
| 5 |              same as 2 for now |
| 6 |              same as 3 for now |
| 7 |              same as 4 for now |

A block strategy pair is computed as:
```Fortran
J_inner + J_outer * 8
```
where `J_inner` and `J_outer` are taken from the table above.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
