
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/r-gris/tidync.svg?branch=master)](https://travis-ci.org/r-gris/tidync) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/tidync)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)

tidync
======

**NOTE:** this package is development and subject to change.

The goal of tidync is to ease exploring the contents of a NetCDF file and constructing efficient queries to extract arbitrary hyperslabs.

The data extracted can be used directly in array contexts, or in "long form" form "tidy" analysis and visualization contexts.

There are two main ways of using tidync.

These examples are for illustration only, see the vignettes for more details, and please try on your own files!

Interactive
-----------

Use `tidync()` and `hyper_filter()` to discern what variables and dimensions are available, and to craft axis-filtering expressions by value or by index. (Use the name of the variable on the LHS to target it, use its name to filter by value and the special name `step` to filter it by index).

``` r
## discover the variables, and the active variable's dimensions
tidync(filename)

## activate a different variable
tidync(filename) %>% activate(varname)

## get a dimension-focus on the space occupied by the variable
tidync(filename) %>% hyper_filter()

## pass named expressions to subset dimension by value or index (step)
tidync(filename) %>% hyper_filter(lat = lat < -30, time = time == 20)
```

Extractive
----------

Use what we learned interactively to extract the data, either in data frame or raw-array (hyper slice) form. It's important to not actual request the data extraction until the expressions above would result in an efficient size (don't try a data frame version of a 20Gb ROMs variable ...).

``` r
## we'll see a column for sst, lat, time, and whatever other dimensions sst has
tidync(filename) %>% activate(sst) %>% 
  hyper_filter(lat = lat < -30, time = time == 20) %>% 
  hyper_tibble()


## raw array form, we'll see an R array with a dimension for each seen by tidync(filename) %>% activate(sst)
tidync(filename) %>% activate(sst) %>% 
  hyper_filter(lat = lat < -30, time = time == 20) %>% 
  hyper_slice()
```

There is another function `hyper_index` that build the actual index values required by the NetCDF library. This can be used to debug the process or to define your own tools for the extraction. Currently each `hyper_*` function can take the filtering expressions, but it's not obvious if this is a good idea or not.

Development
===========

Wishlist. Submit your own to the [Issues tab](https://github.com/r-gris/tidync)

-   install useful example files, and steps to download good files
-   figure out whether "activate" is the right function, how to use that name (sorry Thomas)
-   wrappers for returning raster brick
-   helpers for dimension values as lists of coordinates for the slice form
-   delayed extraction to show the tibble you would get after collect()
-   consider better function names like `hyper_df`, `hyper_dbl` and `hyper_int` (differentiate scaled and unscaled?)

Support for tbl\_cube is in bare-bones form

    f <- "eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/1981/AVHRR/avhrr-only-v2.19810901.nc"
    tidync(f) %>% hyper_tbl_cube(lat = lat > -30)
    Source: local array [691,200 x 4]
    D: lon [dbl, 1440]
    D: lat [dbl, 480]
    D: zlev [dbl, 1]
    D: time [dbl, 1]
    M: sst [dbl]

    tidync(f) %>% activate(anom) %>% hyper_tbl_cube(lat = lat > -30)
    Source: local array [691,200 x 4]
    D: lon [dbl, 1440]
    D: lat [dbl, 480]
    D: zlev [dbl, 1]
    D: time [dbl, 1]
    M: anom [dbl]

Installation
------------

You can install tidync from github with:

``` r
# install.packages("devtools")
devtools::install_github("r-gris/tidync")
```

Example
-------

This is a basic example which shows you how to connect to a file.

``` r
file <- system.file("extdata", "unidata", "test_hgroups.nc", package = "tidync")
library(tidync)
tidync(file) 
#> Variables: UTC_time, (mozaic_flight_2012030403540535_ascent/air_press, mozaic_flight_2012030403540535_ascent/CO, mozaic_flight_2012030403540535_ascent/O3, mozaic_flight_2012030403540535_ascent/altitude, mozaic_flight_2012030403540535_ascent/lat, mozaic_flight_2012030403540535_ascent/lon, mozaic_flight_2012030321335035_descent/CO, mozaic_flight_2012030321335035_descent/O3, mozaic_flight_2012030321335035_descent/altitude, mozaic_flight_2012030321335035_descent/UTC_time, mozaic_flight_2012030321335035_descent/lat, mozaic_flight_2012030321335035_descent/lon, mozaic_flight_2012030403540535_descent/CO, mozaic_flight_2012030403540535_descent/O3, mozaic_flight_2012030403540535_descent/altitude, mozaic_flight_2012030403540535_descent/UTC_time, mozaic_flight_2012030403540535_descent/lat, mozaic_flight_2012030403540535_descent/lon, mozaic_flight_2012030412545335_ascent/CO, mozaic_flight_2012030412545335_ascent/O3, mozaic_flight_2012030412545335_ascent/altitude, mozaic_flight_2012030412545335_ascent/UTC_time, mozaic_flight_2012030412545335_ascent/lat, mozaic_flight_2012030412545335_ascent/lon, mozaic_flight_2012030419144751_ascent/CO, mozaic_flight_2012030419144751_ascent/O3, mozaic_flight_2012030419144751_ascent/altitude, mozaic_flight_2012030419144751_ascent/UTC_time, mozaic_flight_2012030419144751_ascent/lat, mozaic_flight_2012030419144751_ascent/lon, mozaic_flight_2012030319051051_descent/CO, mozaic_flight_2012030319051051_descent/O3, mozaic_flight_2012030319051051_descent/altitude, mozaic_flight_2012030319051051_descent/UTC_time, mozaic_flight_2012030319051051_descent/lat, mozaic_flight_2012030319051051_descent/lon, mozaic_flight_2012030421382353_ascent/CO, mozaic_flight_2012030421382353_ascent/O3, mozaic_flight_2012030421382353_ascent/altitude, mozaic_flight_2012030421382353_ascent/UTC_time, mozaic_flight_2012030421382353_ascent/lat, mozaic_flight_2012030421382353_ascent/lon) 
#> Dimensions: 
#> # A tibble: 2 x 5
#>   variable_name .variable_ .dimension_ dimension_name dimension_length
#>           <chr>      <dbl>       <int>          <chr>            <int>
#> 1      UTC_time          0           0         recNum               74
#> 2      UTC_time          0           0         recNum               74
```

See this article for more: <https://r-gris.github.io/tidync/articles/static-vignettes/tidync-examples.html>

Stay tuned.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.