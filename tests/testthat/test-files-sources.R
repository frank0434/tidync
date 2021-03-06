context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  expect_error(tidync(fname), "failed to open")
})

test_that("files and bad files are handled", {
  l3b_file <- system.file("extdata/oceandata/S2008001.L3b_DAY_RRS.nc", package = "tidync")
  expect_error(suppressWarnings(tidync(l3b_file)), "no variables or dimension recognizable") 
  expect_warning(try(tidync(l3b_file), silent = TRUE), "no dimensions found")
  expect_warning(try(tidync(l3b_file), silent = TRUE), "no variables found")
  expect_warning(try(tidync(l3b_file), silent = TRUE), "no variables recognizable")  
  

})

test_that("verbs have a character method", {
  f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
  l3file <- system.file("extdata/oceandata", f, package= "tidync")
  hyper_tibble(l3file, lon = lon < -140, lat = lat > 85) %>% expect_s3_class("tbl_df")
  hyper_filter(l3file, lon = lon < -140, lat = lat > 85) %>% expect_s3_class("tidync")
  hyper_slice(l3file, lon = lon < -140, lat = lat > 85, select_var = "chlor_a") %>% expect_type("list")
  hyper_tbl_cube(l3file, lon = lon < -140, lat = lat > 85) %>% expect_s3_class("tbl_cube")
  
})
test_that("RNetCDF fall back works", {
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
  nc_get(ufile, "invTime") %>% expect_is("array") %>% 
    expect_length(1176) #%>% expect_type("double")
  nc_get(ufile, "invTime", test = TRUE) %>% expect_is("array") %>% 
    expect_length(1176) #%>% expect_type("integer")
  
})

