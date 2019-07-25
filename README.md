
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

`fwabc` uses the [`bcdata`](https://github.com/bcgov/bcdata) package to
query the atlas remotely. It provides convenience for common queries and
tools to facilitate working with the data.

Let’s say we want to use the bcdata package to get the Fraser River
stream-network polyline plus all its named tributaries with stream order
\> 5.

We could do:

``` r
# frasr river stream
bcdata::bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") %>%
  bcdata::filter(CQL("FWA_WATERSHED_CODE LIKE '100-%'")) %>%
  bcdata::filter(STREAM_ORDER > 5, !is.na(GNIS_NAME))
  bcdata::collect()
```

This requires us to know: 1. that the record id for the ‘stream-network’
dataset is ‘92344413-8035-4c08-b996-65a9b3f62fca’; 2. that any
tributaries of the Fraser River have FWA\_WATERSHED\_CODE starting with
“100-”; 3. some CQL; 4. variable names in the ‘stream-network’
dataset.

With the `fwabc` package we make these queries easier by doing much of
the work in the background and by providing functions and lookup tables
to search for valid feature names. For example, the above can be
accomplished with:

``` r
fwa_search_gnis("fraser river") %>%
  fwa_read_stream_network(named_only = TRUE, tributaries = TRUE, min_stream_order = 5L)
```

If you prefer to review the result without collecting (which can take a
long time for large requests), use the argument `collect = FALSE`.

We currently provide functions for 11 datasets. Since each dataset has
different attributes, acceptable x input and function arguments differ
slightly for each. For example, fwa\_read\_watershed\_group() accepts
only valid WATERSHED\_GROUP\_CODE or WATERSHED\_GROUP\_NAME as input and
does not provide the option to get tributaries or filter by named/stream
order.

If you are familiar with the atlas and require greater query
flexibility, then we suggest using the fantastic `bcdata` package
directly.

## Installation

To install the latest development version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

    if(!"drat" %in% installed.packages()[,1]) 
      install.packages("drat")
    drat::addRepo("poissonconsulting")
    install.packages("fwabc")

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/fwabc)

    if(!"devtools" %in% installed.packages()[,1]) 
      install.packages("devtools")
    devtools::install_github("bcgov/bcdata")
    devtools::install_github("poissonconsulting/yesno")
    devtools::install_github("poissonconsulting/err")
    devtools::install_github("poissonconsulting/checkr")
    devtools::install_github("poissonconsulting/fwabc")

<!-- ## Usage -->

<!-- ### Function families -->

<!-- + `fwa_read_` - read features from BC Freshwater Atlas layers. -->

<!-- + `fwa_pull_` - return a modified version of some input (e.g. another input type, or tributaries of that input). -->

<!-- + `fwa_search_` - match a regular expression to official named features. -->

<!-- ### Lookup tables -->

<!-- `fwa_lookup_gnis`, `fwa_lookup_watershed_group`, `fwa_lookup_layer` provide resources for finding layer names, named features and which layers have data for particular features.  -->

<!-- ### Read  -->

<!-- Read features from available layers using the `fwa_read()` function: -->

<!-- ```{r stream} -->

<!-- library(ggplot2) -->

<!-- library(magrittr) -->

<!-- library(fwabc) -->

<!-- fwa_read(c("SKGT", 356439092), layer = "stream-network")  -->

<!-- ``` -->

<!-- All `fwa_read_` functions return a [sf](https://github.com/r-spatial/sf) object and require some combination of `WATERSHED_KEY` and `WATERSHED_GROUP_CODE` as input. -->

<!-- ```{r lookup} -->

<!-- fwa_lookup_layer -->

<!-- ``` -->

<!-- Read features from multiple layers for Skagit River watershed group: -->

<!-- ```{r layers} -->

<!-- layers <- c("stream-network", "rivers", "lakes", "watershed-groups") -->

<!-- x <- lapply(layers, function(x) fwa_read("SKGT", layer = x)) -->

<!-- names(x) <- layers -->

<!-- ggplot() +  -->

<!--     geom_sf(data = x[["watershed-groups"]], size = 0.2) + -->

<!--     geom_sf(data = x[["lakes"]], size = 0.3, fill = "steelblue") + -->

<!--     geom_sf(data = x[["rivers"]], size = 0.3, fill = "steelblue") + -->

<!--     geom_sf(data = x[["stream-network"]][x[["stream-network"]]$STREAM_ORDER > 2,], size = 0.07)  -->

<!-- ``` -->

<!-- There is a convenience function for each layer, e.g. `fwa_read_stream_network()`, `fwa_read_watershed_groups()`, `fwa_read_coastlines()` etc. -->

<!-- To read an entire layer, leave the default `x = NULL`. Some layers (e.g. `stream-network` and `watersheds`) are very large. -->

<!-- ```{r wsgroup} -->

<!-- library(rmapshaper) -->

<!-- x <- fwa_read_watershed_groups(ask = FALSE) %>% -->

<!--   # simplify with rmapshaper package -->

<!--   rmapshaper::ms_simplify()   -->

<!-- ggplot() +  -->

<!--   geom_sf(data = x, size = 0.05, aes(fill = WATERSHED_GROUP_NAME), show.legend = FALSE)  -->

<!-- ``` -->

<!-- ### Search and pull -->

<!-- `fwa_search_` and `fwa_pull_` functions can be used to find `WATERSHED_KEY` or `WATERSHED_GROUP_CODE`. -->

<!-- Match a regular expression to an official stream name (`GNIS_NAME`) or watershed group name (`WATERSHED_GROUP_NAME`): -->

<!-- ```{r search} -->

<!-- fwa_search_gnis("skagi|scagi") -->

<!-- fwa_search_watershed_group("skagi|scagi") -->

<!-- ``` -->

<!-- Pull the `WATERSHED_KEY` or `WATERSHED_GROUP_CODE` from the official name and provide to `fwa_read_`: -->

<!-- ```{r pipe} -->

<!-- stream <- fwa_search_gnis("skagi|scagi") %>% -->

<!--   fwa_pull_watershed_key() %>% -->

<!--   fwa_read_stream_network() -->

<!-- wshed <- fwa_search_watershed_group("skagi|scagi") %>% -->

<!--   fwa_pull_watershed_group_code() %>% -->

<!--   fwa_read_watershed_groups() -->

<!-- ggplot() + -->

<!--   geom_sf(data = wshed, size = 0.2) + -->

<!--   geom_sf(data = stream)  -->

<!-- ``` -->

<!-- ### Tributaries -->

<!-- Use `fwa_pull_tributaries()` to get tributaries from a `WATERSHED_KEY`: -->

<!-- ```{r tribs} -->

<!-- tribs <- fwa_search_gnis("skagit") %>% -->

<!--   fwa_pull_watershed_key() %>% -->

<!--   fwa_pull_tributaries(order = 1L) %>% -->

<!--   fwa_read_stream_network() -->

<!-- ggplot() + -->

<!--   geom_sf(data = wshed, size = 0.2) + -->

<!--   geom_sf(data = stream) + -->

<!--   geom_sf(data = tribs, size = 0.1)  -->

<!-- ``` -->

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/fwabc/issues).

[Pull requests](https://github.com/poissonconsulting/fwabc/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
