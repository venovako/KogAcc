# KogAcc
A Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

This software is a supplementary material for:
- the paper doi:[10.1007/s11075-025-02035-7](https://doi.org/10.1007/s11075-025-02035-7 "Arithmetical enhancements of the Kogbetliantz method for the SVD of order two").

## Prerequisites

Several routines and executables require having quadruple precision (`KIND=REAL128`) fully supported by the compiler.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/KogAcc`).
Then, build the `libpvn` library, with the same family of compilers and (no-)debug mode as it is meant to be used here.
Please set the make options `SAFE=SV2` and `OPENMP=0` for `libpvn`.
See and adapt the examples in `etc/build_gf.sh` and `etc/build_ifx.sh`.

For now, only little-endian platforms are supported, with the `gfortran` or `ifx` compilers.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl) and [Graphviz](https://graphviz.org).
Many routines are documented only rudimentary for now.

The correctly-rounded `cr_hypot` and `cr_hypotf` functions are expected to be provided by the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Please consult the description of `libpvn` for more information.
All testing has been performed with the correctly rounded functions.
Even though it is technically feasible not to use them, this should be attempted only if necessary.

## Building

See above; also, run `make help` (GNU make assumed) in the `src` subdirectory.

## Running

The available Jacobi strategies:

| J |                    description |
| - | ------------------------------ |
| 0 |        row-cyclic (sequential) |
| 1 |     column-cyclic (sequential) |
| 2 | generalized Mantharam-Eberlein |
| 3 |    dynamic (max ``N/2`` pairs) |
| 4 | modified modulus: quasi-cyclic |
| 5 |   2, but executed sequentially |
| 6 |   3, but executed sequentially |
| 7 |   4, but executed sequentially |

A block strategy pair is computed as:
```Fortran
J_inner + J_outer * 8
```
where `J_inner` and `J_outer` are taken from the table above.
The GNU Fortran compiler is recommended for the ``J_outer = 3`` case.

Please, set the block size `B` to at least `4`, for now.
For both the blocked and the pointwise routines it is recommended that the matrix order be even.

The `etc/env.sh` script should be sourced, without arguments or with a single argument `old` or `new`, as:
```bash
source etc/env.sh
```
before running any OpenMP-parallel executable.
The presence of the second argument enables the experimental, nested, two-level OpenMP parallelism, with an older or a newer set of OpenMP environment variables, for the block-method executables `?ksvd1.exe`.

### Selecting a proper method

| routines |                                       description |
| -------- | ------------------------------------------------- |
| `xKSVD0` | a pointwise method with a (quasi-)cyclic ordering |
| `xKSVDD` | a pointwise method with the dynamic ordering      |
| `xKSVD1` | a blocked method with any available ordering      |

In general, the in-out `INFO` argument of the above routines should be preset in one of two ways:
- ``INFO=M``, for a faster but less accurate/reliable algorithm, or
- ``INFO=-M-1``, for a slower but more accurate/reliable one, where
``M≥0`` is the maximal number of (block-)steps, either parallel or sequential, to be performed.

The test executables always choose the latter option, and should be consulted for examples of properly allocating the various buffers and calling the routines.

Please note that the `xKSVD2` routines are just wrappers around the ``2×2`` SVD routines from `libpvn`.

## TODO

The complex routines have not been tested as thoroughly as the real ones.
Use them with care.

More testing is generally needed.
If something seems wrong, recompiling without the `NDEBUG` option should turn on the error checking and might help with locating the issue.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
