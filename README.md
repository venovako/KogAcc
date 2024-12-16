# KogAcc
A Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

This software is a supplementary material for:
- the preprint arXiv:[2407.13116](https://arxiv.org/abs/2407.13116 "Arithmetical enhancements of the Kogbetliantz method for the SVD of order two").

## Prerequisites

Several routines and executables require having quadruple precision (`KIND=REAL128`) fully supported by the compiler.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/KogAcc`).
Then, build the `libpvn` library, with the same family of compilers and (no-)debug mode as it is meant to be used here (e.g., with `icx` if `ifx` is desired).
Please set the option `SAFE=sv2` for `libpvn`, and avoid using `gfortran` compiler for `KogAcc` until some issues are resolved.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl) and [Graphviz](https://graphviz.org).
Many routines are documented only rudimentary for now.

The correctly-rounded `cr_hypot` and `cr_hypotf` functions are expected to be provided by the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Please consult the description of `libpvn` for more information.
All testing has been performed with the correctly rounded functions.
Even though it is technically feasible not to use them, this should be attempted only if necessary.

## Building

Run `make help` (GNU make assumed) in the `src` subdirectory.

Setting `NDEBUG` to, e.g., `3` is recommened.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
