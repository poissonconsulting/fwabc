
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
the BC Freshwater Atlas. Currently, the package supports retrieving
streams, watersheds, watershed groups, and coastline.

We simplify working with the databases by:

  - Writing SQL queries required to read only a subset of a layer prior
    to loading into memory.

  - Matching regular expressions (e.g. “columbia|koot”) to GNIS\_NAME,
    BLUE\_LINE\_KEY and FWA\_WATERSHED\_CODE.

  - Parsing FWA\_WATERSHED\_CODE to get all tributaries of a particular
    feature in the network.

Let’s take a look at two of my neighbourhoud streams, whose official
GNIS names are Sangan River and Hiellen River.

``` r
library(fwabc)
library(ggplot2)
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

streams <- fwa_stream("sangan|hiellen")
#> Reading layer `FWA_ROUTES_SP' from data source `/Users/sebastiandalgarno/Dropbox (Poisson Consulting)/Data/spatial/fwa/gdb/FWA_BC.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 2 features and 7 fields
#> geometry type:  MULTILINESTRING
#> dimension:      XYZM
#> bbox:           xmin: 608521.1 ymin: 1002883 xmax: 622980 ymax: 1023092
#> epsg (SRID):    3005
#> proj4string:    +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
```

Notice that `fwa_stream()` takes a regular expression to internally
search for matching GNIS names. It is useful to check which streams we
have included.

``` r
fwa_search_gnis("sangan|hiellen")
#> [1] "Hiellen River" "Sangan River"
```

Now let’s get all of the tributaries and plot.

``` r
tribs <- fwa_stream("sangan|hiellen", tributaries = TRUE)
#> Reading layer `FWA_ROUTES_SP' from data source `/Users/sebastiandalgarno/Dropbox (Poisson Consulting)/Data/spatial/fwa/gdb/FWA_BC.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 362 features and 7 fields
#> geometry type:  MULTILINESTRING
#> dimension:      XYZM
#> bbox:           xmin: 601301.2 ymin: 1002872 xmax: 629519.9 ymax: 1023092
#> epsg (SRID):    3005
#> proj4string:    +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

ggplot() +
  geom_sf(data = tribs, color = "black", size = 0.2) +
  geom_sf(data = streams, color = "steelblue", size = 0.5) 
```

![](man/figures/README-stream%20tribs-1.png)<!-- -->

Finally let’s get the watershed polygons and coastline for Graham
Island.

``` r
wshed <- fwa_watershed("sangan|hiellen", tributaries = TRUE) %>% 
  sf::st_union()
#> Warning in grepl(a, fwa_lookup_watershed$WatershedCode, fixed = TRUE):
#> argument 'pattern' has length > 1 and only the first element will be used
#> Warning in gsub(paste0(a, "-"), "", .): argument 'pattern' has length > 1
#> and only the first element will be used
#> Reading layer `GRAI' from data source `/Users/sebastiandalgarno/Dropbox (Poisson Consulting)/Data/spatial/fwa/gdb/FWA_WATERSHEDS_POLY.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 247 features and 36 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: 608008.4 ymin: 1003117 xmax: 629651.3 ymax: 1023355
#> epsg (SRID):    3005
#> proj4string:    +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

coastline <- fwa_coastline("Graham Island")
#> Reading layer `FWA_COASTLINES_SP' from data source `/Users/sebastiandalgarno/Dropbox (Poisson Consulting)/Data/spatial/fwa/gdb/FWA_BC.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 2901 features and 13 fields
#> geometry type:  MULTILINESTRING
#> dimension:      XY
#> bbox:           xmin: 529104.2 ymin: 922281.6 xmax: 633150.8 ymax: 1051407
#> epsg (SRID):    3005
#> proj4string:    +proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

bbox <- sf::st_bbox(sf::st_buffer(wshed, 8000))

ggplot() +
  geom_sf(data = coastline, size = 0.3, color = "black") +
  geom_sf(data = wshed, fill = "white", size = 0.4, color = "black") +
  geom_sf(data = tribs, size = 0.1, color = "black") +
  geom_sf(data = streams, size = 0.5, color = "steelblue") +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))
```

![](man/figures/README-stream%20map-1.png)<!-- -->

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/fwabc)

    install.packages("devtools")
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
