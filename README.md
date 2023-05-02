# KogAcc
A portable Kogbetliantz-type SVD for general matrices.

(... work in progress ...)

## Building

### Prerequisites

A recent Linux, macOS, or Windows is needed, and the oneAPI Intel Math Kernel Library (MKL) is assumed to be installed on x86_64 platforms when the GNU or the Intel compilers are used.

The recent Intel (``ifx`` 2023.1+) Fortran compilers should work , while ``ifort`` 2021.9+ needs further testing, as well as the GNU ``gfortran`` 13+.

Other 64-bit OSes (e.g., Oracle Solaris) and non-x86_64 (e.g., ppc64le) platforms might also be supported to a certain extent, if the reference BLAS and LAPACK libraries have been built.

First, clone [JACSD](https://github.com/venovako/JACSD) repository, with the same parent directory as this one has (e.g., ``venovako/JACSD`` and ``venovako/KogAcc``).
Then, build the ``jstrat`` library, with the same compilers and (no-)debug mode as it is meant to be used here.

Building the documentation requires a recent version of [Doxygen](https://doxygen.nl).

The correctly-rounded ``cr_hypot`` and ``cr_hypotf`` functions might optionally be used if provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
If their implementation is to be linked with, set the ``CR_MATH`` variable in ``[g]make`` invocation to the cloned ``core-math`` source code directory path.
Note, ``hypot*_noerrno.c`` files, referenced here in ``src/x86_64.mk``, are not provided there but can be easily modified from the corresponding ``hypot*.c`` files by eliminating all references to ``errno``.

### Make options

On Linux or macOS, run ``make help`` (GNU make assumed).

On antoher \*nix OS where GNU `make` is not the default one, run ``gmake help`` instead.

On Windows, run ``nmake help`` in the Intel oneAPI command prompt.
