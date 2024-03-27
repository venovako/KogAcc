# KogAcc
A Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

## Prerequisites

The oneAPI Intel Math Kernel Library (MKL) is assumed to be installed on Linux or macOS x86_64 platforms when the Intel or the GNU compilers are used.
Other 64-bit OSes (e.g., Oracle Solaris) and platforms (e.g., arm64 or ppc64le) might also be supported to a certain extent, if the reference BLAS and LAPACK libraries have been built.

Intel's `ifx` is *strongly recommended* for testing the `n × n` SVD routines!
Other compilers might produce unreliable binaries in these cases.
Several routines and executables require having quadruple precision (`KIND=REAL128`) fully supported by the compiler, and therefore not all popular compilers have been tested.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/KogAcc`).
Then, build the `libpvn` library, with the same family of compilers and (no-)debug mode as it is meant to be used here (e.g., with `icx` if `ifx` is desired).

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl) and [Graphviz](https://graphviz.org).
Many routines are documented only rudimentary for now.

The correctly-rounded `cr_hypot` and `cr_hypotf` functions might optionally be used if provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Please consult the description of `libpvn` for more information.
All testing has been performed with the correctly rounded functions.
It is strongly recommended to use them whenever possible.

## Building

On Linux or macOS, run `make help` (GNU make assumed) in the `src` subdirectory.

On antoher \*nix OS where GNU `make` is not the default one, run `gmake help` instead.

Setting `NDEBUG` to, e.g., `3` is recommened.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
