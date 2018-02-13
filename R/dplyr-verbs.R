#' hyper index
#' 
#' Perform a traditional unnamed array slice operation. This assumes that 
#' no previous selection has been made. 
#' @export
#' @inheritParams hyper_array
hyper_index <- function(x, ..., select_var = NULL, raw_datavals = FALSE) {
  UseMethod("hyper_index")
}
#' @export
#' @name hyper_index
hyper_index.character <- function(x, ..., select_var = NULL, raw_datavals = FALSE) {
  tidync(x) %>% hyper_index(..., select_var = select_var, raw_datavals = raw_datavals)
}
#' @export
#' @name hyper_index
hyper_index.tidync <- function(x, ..., select_var = NULL, raw_datavals = FALSE) {
  list_index <- list(...)
  axes <- active_axis_transforms(x)
  if (length(list_index) == (length(axes) - 1L)) {
    warning(sprintf("assuming singleton degenerate dimension in last position (%i)", length(axes)))
    list_index <- c(list_index, TRUE)
  }
  if (!length(list_index) == length(axes)) {
    stop(sprintf("expecting %i input slice indexes", length(axes)))
  }
 for (i in seq_along(list_index)) {
   index <- list_index[[i]]
   if (isTRUE(index)) list_index[[i]] <- seq_len(nrow(axes[[i]]))
   if (length(index) < 1) stop(sprintf("index %i is invalid", i))
   if (any(index > nrow(axes[[i]]) || any(index < 1L))) {
     stop(sprintf("index %i is out of bounds", i))
   }
   axes[[i]]$selected <- FALSE
   axes[[i]]$selected[index] <- TRUE
   
 }
  x$transforms[names(axes)] <- axes
  ## todo remove need for verbs to update_slices like this
  hyper_array(update_slices(x), select_var = select_var, raw_datavals = raw_datavals)
}




#' dplyr methods
#' 
#' @param .data tidync object
#' @export
#' @name filter
#' @importFrom dplyr filter
#' @export
#' @export filter
filter.tidync <- function(.data, ...) {
  hyper_filter(.data, ...)
}

#' Hyper slice
#' 
#' Slice out a "hyper" array, returning a list of variables in native 
#' array form. 
#' @inheritParams hyper_slice
#' @export
hyper_array <- function(x, select_var = NULL, ..., raw_datavals = FALSE, force = FALSE) {
  hyper_slice(x, select_var = select_var, ..., raw_datavals = raw_datavals, force = force)
}

#' Dplyr 'tbl' cubes
#' 
#' Convert a tidync object to a dplyr tbl_cube
#' @param x tidync object
#' @inheritDotParams dplyr::as.tbl_cube
#' @name as.tbl_cube
#' @importFrom dplyr as.tbl_cube
#' @export as.tbl_cube
#' @export
as.tbl_cube.tidync <- function(x, ...) {
  hyper_tbl_cube(x, ...)  
}

#' Tibble
#' 
#' Convert a tidync object to a tibble (data frame). 
#' @param x dplyr object
#' @param ... 
#' @export
as_tibble.tidync <- function(x, ..., na.rm = TRUE) {
  hyper_tibble(x, ..., na.rm = na.rm)
}


