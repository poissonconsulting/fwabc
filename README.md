
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

`fwabc` is an R package to read data from the [BC Freshwater
Atlas](https://www2.gov.bc.ca/assets/gov/data/geographic/topography/fwa/fwa_user_guide.pdf).
Reference tables and functions are provided to simplify working with the
database.

## Installation

Install the latest development version of `bcdata` from
[GitHub](https://github.com/bcgov/bcdata) with `remotes` package:

    install.packages("remotes")
    remotes::install_github("bcgov/bcdata")

Install the latest development version of `fwabc` from
[GitHub](https://github.com/poissonconsulting/fwabc)

    remotes::install_github("poissonconsulting/fwabc")

## Functions

  - `fwa_read_` - Read features from BC Freshwater Atlas layers.
  - `fwa_pull_` - Provide an input and return some modified version of
    it (e.g. another input type, or tributaries of that input).
  - `fwa_search_` Match a regular expression to official named features
    (`GNIS_NAME` and `WATERSHED_GROUP_NAME`).

## Usage

### Read

Read features from available layers using the `fwa_read()` function:

``` r
library(ggplot2)
library(magrittr)
library(fwabc)

x <- fwa_read(c("SKGT", 356439092), layer = "stream-network") 
#> Warning: It is advised to use the permanent id ('92344413-8035-4c08-
#> b996-65a9b3f62fca') rather than the name of the record ('freshwater-atlas-
#> stream-network') to guard against future name changes

ggplot() + 
  geom_sf(data = x, size = 0.2) +
  ggtitle("Skagit River stream-network layer")
```

![](man/figures/README-stream-1.png)<!-- --> All `fwa_read_` functions
return a [sf](https://github.com/r-spatial/sf) object and require any
combination of `WATERSHED_KEY` and `WATERSHED_GROUP_CODE` as input. The
`glaciers` and `watershed-groups` layers do not accept `WATERSHED_KEY`
as input.

Read features from multiple layers for Skagit River watershed group:

``` r
layers <- c("stream-network", "rivers", "lakes", "watershed-groups")
x <- lapply(layers, function(x) fwa_read("SKGT", layer = x))
names(x) <- layers

ggplot() + 
    geom_sf(data = x[["watershed-groups"]], size = 0.2) +
    geom_sf(data = x[["lakes"]], size = 0.3, fill = "steelblue") +
    geom_sf(data = x[["rivers"]], size = 0.3, fill = "steelblue") +
    geom_sf(data = x[["stream-network"]][x[["stream-network"]]$STREAM_ORDER > 2,], size = 0.07) +
    geom_sf_label(data = x[["rivers"]], aes(label = GNIS_NAME_1)) +
    ggtitle("Skagit River Watershed Group")
#> Warning: Removed 1 rows containing missing values (geom_label).
```

![](man/figures/README-layers-1.png)<!-- -->

There is a convenience function for each layer,
e.g. `fwa_read_stream_network()`, `fwa_read_watershed_groups()`,
`fwa_read_coastlines()` etc. To read an entire layer, leave the default
`x = NULL`. Some layers (e.g. `stream-network` and `watersheds`) are
very large.

``` r
library(rmapshaper)
x <- fwa_read_watershed_groups(ask = FALSE) %>%
  # simplify with rmapshaper package
  rmapshaper::ms_simplify()  

ggplot() + 
  geom_sf(data = x, size = 0.05, aes(fill = WATERSHED_GROUP_NAME), show.legend = FALSE) +
  ggtitle("watershed-groups layer")
```

![](man/figures/README-wsgroup-1.png)<!-- -->

### Search and pull

Often, the `WATERSHED_KEY` or `WATERSHED_GROUP_CODE` is unknown. Use the
`fwa_search_` and `fwa_pull_` functions to help.

Match a regular expression to an official stream name (`GNIS_NAME`) or
watershed group name (`WATERSHED_GROUP_NAME`):

``` r
fwa_search_gnis("skagi|scagi")
#> [1] "Skagit River"
fwa_search_watershed_group("skagi|scagi")
#> [1] "Skagit River"
```

Pull the `WATERSHED_KEY` or `WATERSHED_GROUP_CODE` from the official
name and provide to `fwa_read_`:

``` r
stream <- fwa_search_gnis("skagi|scagi") %>%
  fwa_pull_watershed_key() %>%
  fwa_read_stream_network()

wshed <- fwa_search_watershed_group("skagi|scagi") %>%
  fwa_pull_watershed_group_code() %>%
  fwa_read_watershed_groups()

ggplot() +
  geom_sf(data = wshed, size = 0.2) +
  geom_sf(data = stream) +
  ggtitle("Skagit River")
```

![](man/figures/README-pipe-1.png)<!-- -->

### Tributaries

Use `fwa_pull_tributaries()` to get tributaries from a `WATERSHED_KEY`:

``` r
tribs <- fwa_search_gnis("skagit") %>%
  fwa_pull_watershed_key() %>%
  fwa_pull_tributaries(order = 1L) %>%
  fwa_read_stream_network()

ggplot() +
  geom_sf(data = wshed, size = 0.2) +
  geom_sf(data = stream) +
  geom_sf(data = tribs, size = 0.1) +
  ggtitle("Skagit River and Primary Tributaries")
```

![](man/figures/README-tribs-1.png)<!-- -->

### Lookup tables

`fwabc` provides three lookup tables (`fwa_lookup_gnis`,
`fwa_lookup_watershed_group`, `fwa_lookup_layer`), which can be useful
for finding layer names, named features or which layers have data for
particular features.

## Other packages

`fwabc` read functions use the
[`bcdata`](https://github.com/bcgov/bcdata) package (specifically,
`bcdata::bcdc_query_geodata()`) to query the freshwater atlas remotely.
`fwabc` provides convenience for common requests

``` r
fwa_read_stream_network(c("SKGT", 356439092))

#### ---- returns the same result as ---- ####

bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
  bcdata::filter(WATERSHED_GROUP_CODE == "SKGT" | WATERSHED_KEY == 356439092) %>%
  bcdata::collect()
```

and resources/functions to get tributaries and official `GNIS_NAME`,
`WATERSHED_KEY` and `WATERSHED_GROUP_CODE`.

However, if you are very familiar with the atlas and require greater
filtering flexibility (or even want to use your own CQL), then we
suggest trying the fantastic `bcdata` package.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/fwabc/issues).

[Pull requests](https://github.com/poissonconsulting/fwabc/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
