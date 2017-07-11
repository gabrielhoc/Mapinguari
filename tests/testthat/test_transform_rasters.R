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
      fitFUN = mgcv::gamm,
      args_list = list(random = list(id = ~ 1))
    )

   Perf_rasters <-
   transform_rasters(raster_stack = Fulanus_Ecorasters_download[[1]],
     transformFUN = list(perf = perf_functions$tpc_size$predict),
     transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
    )

# If the functions in `summarize_rasters`` are commutative, you can summarize before transforming, which is much faster.

   Phenology_mean <-
     lapply(Fulanus_Ecorasters_download, summarize_rasters,
       seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
       summaryFUN = list(tmax = c("mean"), tmin = c("mean"), prec = "sum"))

   Perf_rasters_mean <-
     transform_rasters(raster_stack =  Phenology_mean[[1]],
       transformFUN = list(perf = perf_functions$tpc_size$predict),
       transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
     )

# The function works on RasterStacks, if you have a list of stacks, you can apply the function to each element using lapply

   Perf_rasters_list <- lapply(Phenology_mean, transform_rasters,
     transformFUN = list(perf = perf_functions$tpc_size$predict),
     transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
     )

# Make these work
#
#   Perf_rasters_compare_models <- lapply(Phenology_mean, transform_rasters,
#     transformFUN = list(perf_model1 = perf_functions$tpc_size$predict,
#                         perf_model2 = perf_functions$tpc_no_size$predict),
#     transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
#   )
#
#   Perf_rasters_age <- lapply(Phenology_mean, transform_rasters,
#     transformFUN = list(perf = perf_functions$tpc_size$predict),
#     transformFUN_args = list(young = list(temp = 'tmax', size = min(FulanusPhysiology$size)),
#       adult = list(temp = 'tmax', size = max(FulanusPhysiology$size)))
#   )

  expect_is(Perf_rasters, "RasterStack")
  expect_is(Perf_rasters_mean, "RasterStack")
  expect_is(Perf_rasters_list, "list")
  expect_is(Perf_rasters_list[[1]], "RasterStack")

  expect_equal(sum(Perf_rasters[[1]]@data@values), -120683580)
  expect_equal(sum(Perf_rasters_mean[[1]]@data@values), -117251487)
  expect_equal(sum(Perf_rasters_list[[1]][[1]]@data@values), -117251487)

  expect_equal(names(Perf_rasters)[[1]], "perf_1")
  expect_equal(names(Perf_rasters_mean)[[1]], "perf_breeding_mean")
  expect_equal(names(Perf_rasters_list[[1]])[[1]], "perf_breeding_mean")

})
