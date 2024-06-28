# KogAcc
A Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

## Prerequisites

Several routines and executables require having quadruple precision (`KIND=REAL128`) fully supported by the compiler.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/KogAcc`).
Then, build the `libpvn` library, with the same family of compilers and (no-)debug mode as it is meant to be used here (e.g., with `icx` if `ifx` is desired).
Please set the option `SAFE=sv2`.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl) and [Graphviz](https://graphviz.org).
Many routines are documented only rudimentary for now.

The correctly-rounded `cr_hypot` and `cr_hypotf` functions might optionally be used if provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Please consult the description of `libpvn` for more information.
All testing has been performed with the correctly rounded functions.
It is strongly recommended to use them whenever possible.

## Building

Run `make help` (GNU make assumed) in the `src` subdirectory.

Setting `NDEBUG` to, e.g., `3` is recommened.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
