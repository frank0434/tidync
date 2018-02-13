context("test-dplyr-align.R")

# test_that("aliases and renames works", {
#   hanpp <- "data-raw/hanpp_2005.nc"
#   hf <- tidync(hanpp)  %>% 
#     filter(Lons = Lons < -100, Lats = Lats > 40)
#   ## get the array/s
#   arr <- hyper_array(hf)
#   #as_tibble(hf)
#   axs_lon <- hf %>% activate("Lons") %>% hyper_array()
#   axs_lat <- hf %>% activate("Lats") %>% hyper_array()
#   #flipx <- function(x) x[, ncol(x):1]
#   #arrx <- flipx(arr[[1]])
#   #arrx[arrx<0] <- NA
#   #image(axs_lon[[1]], rev(axs_lat[[1]]), arrx)
#   #maps::map(add = TRUE)
#   #cube <- as.tbl_cube(hf)
#   ## this a bit bad because we have to expand everything before
#   ## trimming down
# #  ggplot(cube %>% as_tibble() %>% dplyr::filter(HANPP > 0), 
# #         aes(Lons, Lats, fill = HANPP)) + geom_raster()
#   
# })
