
## @importFrom rlang .data
## @importFrom raster setValues
fast_cellnumbers <- function(pts, p) {
#  pts <- sp::spTransform(sp::coordinates(r, sp = TRUE), raster::projection(r))
  spts <- sp::SpatialPoints(as.matrix(pts[c("x", "y")]))
#  sp::proj4string(pts) <- sp::CRS(NA_character_)
  sp::proj4string(p) <- sp::CRS(NA_character_)
  
  pts$object_ <- sp::over(spts, sp::geometry(p))
  pts %>%   dplyr::filter(!is.na(object_))
 # p <- sf::st_as_sf(p)
  #p$ID <- seq_len(nrow(p))
  #r <- raster::setValues(r, 0)
  #rr <- fasterize::fasterize(p, r, field = "ID")
  #tabularaster::as_tibble(rr) %>% dplyr::filter(!is.na(.data$cellvalue)) %>%
  # dplyr::rename(object_ = .data$cellvalue, cell_ = .data$cellindex)
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
hyper_group_by <- function(x, object, ...) {
  ns <- 1:2  ## hard coded for now
  nominal_space <- active_axis_transforms(x)
  ext <- spex::spex(object)
  ext <- as.vector(t(ext@bbox))
  logibetween <- function(x, range) {
    x >= range[1L] & x <= range[2L]
  }
  
  numbetween <- function(x, range) {
    x[logibetween(x, range)]
  }
  
  xynames <- names(nominal_space)[ns]
#  xs <- numbetween(nominal_space[[xynames[1L]]][[xynames[1L]]], ext[1:2])
#  ys <- numbetween(nominal_space[[xynames[2L]]][[xynames[2L]]], ext[3:4])
  xs <- nominal_space[[xynames[1L]]][[xynames[1L]]]
  ys <- nominal_space[[xynames[2L]]][[xynames[2L]]]
  
  nspts  <- expand.grid(x = xs, y = ys)
nspts$index_1 <- rep(seq_along(xs), length.out = nrow(nspts))
nspts$index_2 <- rep(seq_along(ys), each = length(xs))

  nominal_space <- fast_cellnumbers(nspts, object)
  x$nominal_space <- nominal_space
  #x$transforms[[ns[1]]]$selected <- x$transforms[[ns[1]]]$selected & logibetween(nspts[[1L]], ext[1:2])
  #x$transforms[[ns[2]]]$selected <- x$transforms[[ns[2]]]$selected & logibetween(nspts[[2L]], ext[3:4])
  
 
  x$ns <- ns
  ##class(x) <- c("hyper_group", "tidync")
  x
}

