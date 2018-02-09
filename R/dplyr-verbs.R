#' dplyr methods
#' 
#' @param .data tidync object
#' @export
#' @name filter
filter.tidync <- function(.data, ...) {
  hyper_filter(.data, ...)
}

#' Hyper slice
#' 
#' Slice out a "hyper" array, returning a list of variables in native 
#' array form. 
#' @inheritParams hyper_slice
#' 
hyper_array <- function(x, select_var = NULL, ..., raw_datavals = FALSE, force = FALSE) {
  hyper_slice(x, select_var = select_vart, ..., raw_datavals = raw_datavals, force = force)
}

#' Dplyr 'tbl' cubes
#' 
#' Convert a tidync object to a dplyr tbl_cube
#' @param x tidync object
#' @inheritDotParams dplyr::as.tbl_cube
#' @name as.tbl_cube
as.tbl_cube.tidync <- function(x, ...) {
  hyper_tbl_cube(x, ...)  
}

#' Tibble
#' 
#' Convert a tidync object to a tibble (data frame). 
#' @param x dplyr object
#' @inheritDotParams dplyr::as_tibble 
as_tibble.tidync <- function(x, ..., na.rm = TRUE) {
  hyper_tibble(x, ..., na.rm = na.rm)
}

