test_that("get_rasters works for different raster inputs", {

  #Fulanus_Ecorasters_download <-
  #  get_rasters(
  #    ext = FulanusDistribution,
  #    margin = 5,
  #    non_fixed_var = c('prec', 'tmin', 'tmax'),
  #    years = c("present", '2050', '2070'),
  #    scenarios = c('rcp26', 'rcp45', 'rcp85'),
  #    alert = 6)

  Fulanus_Ecorasters_dir <-
    get_rasters(
      raster_source = "/Users/gabriel/Documents/Mapinguari/global_grids_10_minutes",
      ext = FulanusDistribution,
      margin = 5,
      non_fixed_var = c('prec', 'tmin', 'tmax'),
      fixed_var = "alt",
      years = c("present", '2050', '2070'),
      scenarios = c('rcp26', 'rcp45', 'rcp85'),
      reorder = TRUE,
      alert = 4)

# Fix this
#
#  Fulanus_Ecorasters_list <-
#    get_rasters(
#      raster_source = Fulanus_Ecorasters_dir,
#      ext = c(20, 40, 1, 4),
#      margin = 5,
#      non_fixed_var = c('prec'),
#      fixed_var = "alt",
#      years = c("present", '2050'),
#      scenarios = c('rcp26', 'rcp85'),
#      alert = 1)

#  expect_is(Fulanus_Ecorasters_download, "list")
  testthat::expect_is(Fulanus_Ecorasters_dir, "list")
#  expect_is(Fulanus_Ecorasters_list, "list")

#  expect_is(Fulanus_Ecorasters_download[[1]], "RasterStack")
  testthat::expect_is(Fulanus_Ecorasters_dir[[1]], "RasterStack")
#  expect_is(Fulanus_Ecorasters_list[[1]], "RasterStack")

#  expect_equal(sum(Fulanus_Ecorasters_download[[1]][[1]]@data@values), 426947)
  testthat::expect_equal(sum(Fulanus_Ecorasters_dir[[1]][[1]]@data@values), 426947)
#  expect_equal(sum(Fulanus_Ecorasters_list[[1]][[1]]@data@values), 426947)

#  expect_equal(names(Fulanus_Ecorasters_download[[1]])[[1]], "prec_1")
  testthat::expect_equal(names(Fulanus_Ecorasters_dir[[1]])[[1]], "prec_1")
#  expect_equal(names(Fulanus_Ecorasters_list[[1]])[[1]], "prec_1")

})
