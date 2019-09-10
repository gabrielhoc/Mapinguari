test_that("transform_rasters works", {

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

  perf_functions <-
    fit_curves(models = list(tpc_size = mgcv::gamm(performance ~ s(temp, bs = 'cs') + size,
                                                   random = list(id = ~ 1),
                                                   data = FulanusPhysiology),
                             tpc_no_size = mgcv::gamm(performance ~ s(temp, bs = 'cs'),
                                                      random = list(id = ~ 1),
                                                      data = FulanusPhysiology)))

  my_tpc <- perf_functions$tpc_size$predict

  library(mgcv)

   Perf_rasters <-
   transform_rasters(raster_stack = Fulanus_Ecorasters_dir$present,
                     FUN_qlist = qlist(perf = my_tpc(tmax, size = mean(FulanusPhysiology$size))))

  expect_is(Perf_rasters, "RasterStack")

  expect_equal(sum(Perf_rasters[[1]]@data@values), -120683580)

  expect_equal(names(Perf_rasters)[[1]], "perf_1")

})
