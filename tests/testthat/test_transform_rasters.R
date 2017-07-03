test_that("transform_rasters works", {

  Fulanus_Ecorasters_download <-
    get_rasters(
      ext = FulanusDistribution,
      margin = 5,
      non_fixed_var = c('prec', 'tmin', 'tmax'),
      years = c("present", '2050', '2070'),
      scenarios = c('rcp26', 'rcp45', 'rcp85'),
      alert = 6)

   perf_functions <-
   fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
    data = FulanusPhysiology,
    type = 'GAMM',
    random = list(id = ~ 1),
    separator = '_'
   )

   Perf_rasters <-
   transform_rasters(raster_source = Fulanus_Ecorasters_download[[1]],
    Perf_args = list(temp = 'tmax', size = 10),
    separator = '_',
    PerfFUN = perf_functions$predict$model_1)

   Perf_rasters <- lapply(Fulanus_Ecorasters_download, transform_rasters,
     transformFUN = perf_functions$predict$model_1,
     transformFUN_args = list(temp = 'tmax', size = 10),
     separator = '_')

  expect_is(Ecology_download, "list")
  expect_is(Ecology_dir, "list")
  expect_is(Ecology_list, "list")

  expect_is(Ecology_download[[1]], "RasterStack")
  expect_is(Ecology_dir[[1]], "RasterStack")
  expect_is(Ecology_list [[1]], "RasterStack")

  expect_equal(sum(Ecology_download[[1]][[1]]@data@values), 426947)
  expect_equal(sum(Ecology_dir[[1]][[1]]@data@values), 426947)
  expect_equal(sum(Ecology_list[[1]][[1]]@data@values), 426947)

  expect_equal(names(Ecology_download[[1]])[[1]], "prec_1")
  expect_equal(names(Ecology_dir[[1]])[[1]], "prec_1")
  expect_equal(names(Ecology_list[[1]])[[1]], "prec_1")

})
