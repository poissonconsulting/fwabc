
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/fwabc.svg?branch=master)](https://travis-ci.org/poissonconsulting/fwabc)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/fwabc?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/fwabc)
[![Coverage
status](https://codecov.io/gh/poissonconsulting/fwabc/branch/master/graph/badge.svg)](https://codecov.io/github/poissonconsulting/fwabc?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# fwabc

`fwabc` is an R package to read subsets of data from various layers in
the BC Freshwater Atlas.

There are three main motivations to developing this package:

1.  Some of the layers are huge and difficult to work with in their
    entirety in memory.
2.  It is tedious to subset features by common names (e.g. “Porcher
    Island”, “Kaslo River”).
3.  It is not immediately obvious how to get all tributaries of a
    particular feature in the network.

We provide solutions by:

1.  Subsetting data with SQL queries before reading into memory.
2.  Translating between various coding systems (e.g. GNIS\_NAME and
    BLUE\_LINE\_KEY).
3.  Parsing FWA\_WATERSHED\_CODES to determine tributaries.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/fwabc)

    install.packages("devtools")
    devtools::install_github("poissonconsulting/err")
    devtools::install_github("poissonconsulting/checkr")
    devtools::install_github("poissonconsulting/fwabc")

To install the latest development version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

    install.packages("drat")
    drat::addRepo("poissonconsulting")
    install.packages("fwabc")

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/fwabc/issues).

[Pull requests](https://github.com/poissonconsulting/fwabc/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
