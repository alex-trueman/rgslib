[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP ? Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/contactr)](https://cran.r-project.org/package=contactr)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.2-orange.svg)](commits/master)

# rgslib

`rgslib` provides an interface to the GSLIB and CCG Fortran programes managed and developed by the CCG. These are geostatistical programes and other associated utility programes for spatial modelling of mineral deposits.

I am slowly adding additional functionality as I need it. If there is something specific that you require please add an issue on the [GitHub reporitory](https://github.com/truemoid/rgslib).

## Installation

`rgslib` is not available on CRAN but can be installed from Github using the
`devtools` package.

``` r
# install.packages("devtools")
devtools::install_github("truemoid/rgslib")
```

Various compiled Fortran programes are required for many `rgslib` functions. The location of the executables must be in the `path` environment variable of your OS. I have only tested this on Windows 10, but if you have programes compiled for another OS this package should work.

The programs required are all compiled by the CCG and are: `addcoord`, `declus`, `unscore`, `varcalc`, and `varmap`.

## Code of conduct

Please note that the 'rgslib' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

