# KogAcc
A portable Kogbetliantz-type SVD for general matrices of order two.

(... work in progress ...)

## Building

### Prerequisites

A recent Linux (e.g., CentOS 7.9 with devtoolset-11), macOS (e.g., Big Sur), or Windows (e.g., 10) is needed, and the oneAPI Intel Math Kernel Library (MKL) is assumed to be installed on x86_64 platforms.

Other 64-bit OSes (e.g., Oracle Solaris with gfortran) and non-x86_64 platforms might also be supported to a certain extent, if the reference BLAS and LAPACK libraries have been built.

The recent GNU (gfortran) and Intel (ifort) Fortran compilers should work, while the others might require tweaking the build system.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl).

### Make options

On Linux or macOS, run ``make help`` (GNU make assumed).

On antoher \*nix OS where GNU `make` is not the default one, run ``gmake help`` instead.

On Windows, run ``nmake help`` in the Intel oneAPI command prompt.
