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
   fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
                             tpc_no_size = performance ~ s(temp, bs = 'cs')),
    data = FulanusPhysiology,
    type = 'GAMM',
    random = list(id = ~ 1),
    separator = '_'
   )

   Perf_rasters <-
   transform_rasters(raster_stack = Fulanus_Ecorasters_download[[1]],
     transformFUN = list(perf = perf_functions$predict$model_1),
     transformFUN_args = list(temp = 'tmax', size = 10)
    )

   Perf_rasters_list <- lapply(Fulanus_Ecorasters_download, transform_rasters,
     transformFUN = list(perf = perf_functions$predict$model_1),
     transformFUN_args = list(temp = 'tmax', size = 10)
     )

   Perf_rasters_compare_models <- lapply(Fulanus_Ecorasters_download, transform_rasters,
     transformFUN = list(perf_model1 = perf_functions$predict$model_1,
                         perf_model2 = perf_functions$predict$model_2),
     transformFUN_args = list(temp = 'tmax', size = 10)
   )

   Perf_rasters_age <-
     transform_rasters(raster_stack = Fulanus_Ecorasters_download[[1]],
       transformFUN = list(perf = perf_functions$predict$model_1),
       transformFUN_args = list(young = list(temp = 'tmax', size = 5),
                                adult = list(temp = 'tmax', size = 5))
     )

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
