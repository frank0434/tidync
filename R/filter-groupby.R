## a version of between() that returns the logical test
logibetween <- function(x, range) {
  x >= range[1L] & x <= range[2L]
}
## a version of between() that returns the values
numbetween <- function(x, range) {
  x[logibetween(x, range)]
}


raster_from_nominalspace <- function(spacetransforms) {
  stopifnot(length(spacetransforms) ==2)
  nms <- names(spacetransforms)
  xs <- spacetransforms[[1]][[nms[1]]]
  ys <- spacetransforms[[2]][[nms[2]]]
  dx <- diff(xs[1:2])
  dy <- diff(ys[1:2])
  if ((dx - mean(diff(xs))) > dx/10 || (dy - mean(diff(ys))) > dy/10) {
    warning("not a regular grid")
  }
  raster::raster(raster::extent(c(range(xs)) + c(-1, 1) * dx / 2, 
                       c(range(ys)) + c(-1, 1) * dy / 2), 
                       nrows = length(ys), ncols = length(xs))
}
#' @importFrom rlang .data
#' @importFrom raster setValues
#' @importFrom fasterize fasterize
cellnumbers_from_nominalspace <- function(x, space) {
  p <- space$object
  ns <- space$space
  p$ID <- seq_len(nrow(p))
  r <- raster_from_nominalspace(x$transforms[ns])
  rr <- fasterize::fasterize(p, r, field = "ID")
  tabularaster::as_tibble(rr) %>% #dplyr::filter(!is.na(.data$cellvalue)) %>%
    dplyr::rename(object_ = .data$cellvalue, cell_ = .data$cellindex)
}
## Hyper group by
##
## Group the nominated space within a grid by an polygon object
## @param x a tidync object
## @param object a polygon object
## @param ns the dimensions of the active grid to use as the nominal space
## @importFrom dplyr between filter
## @importFrom raster extent xmin xmax ymin ymax
## @noRd
hyper_space <- function(x, spaceobject, ...) {
 # ns <- spaceobject$nsindex
 #nominal_space <- active_axis_transforms(x)[ns]
  cellnumbers_from_nominalspace(x, spaceobject)
}

spaceobj <- function(object, space = 1:2) {
  structure(list(object = object, space = space), class = "spaceobj")
}

hyper_raster <- function(x, space = 1:2) {
  raster_from_nominalspace(active_axis_transforms(x)[ns])
}

hyper_group_by <- function(x, object, space = 1:2, ...) {
  
}