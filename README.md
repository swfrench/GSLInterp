Cubic-spline interpolation using the GNU Scientific Library
===========================================================

A Haskell interface for cubic-spline interpolation using the [GNU Scientific Library](http://www.gnu.org/software/gsl/).

This package consists of an inner C-based wrapper and outer FFI-based Haskell
layer. Actual interpolation is performed by the GSL `gsl_interp_*_e` family of
functions documented [here](http://www.gnu.org/software/gsl/manual/html_node/Interpolation.html).
