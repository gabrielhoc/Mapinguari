test_that("summarize_rasters works for different season inputs", {

  Fulanus_Ecorasters_download <-
    get_rasters(
      ext = FulanusDistribution,
      margin = 5,
      non_fixed_var = c('prec', 'tmin', 'tmax'),
      years = c("present", '2050', '2070'),
      scenarios = c('rcp26', 'rcp45', 'rcp85'),
      alert = 6)

  Phenology_mean_sd <-
    lapply(Fulanus_Ecorasters_download, summarize_rasters,
      seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
      summaryFUN = list(tmax = c("mean", "sd"), tmin = c("mean", "sd"), prec = "sum"))

  Phenology_weighted_mean <-
    lapply(Fulanus_Ecorasters_download, summarize_rasters,
      seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
      summaryFUN = "weighted.mean",
      summary_args = list(w = c(0.5, rep(1, 4), 0.5))
    )

  PhenFUN <- function(x) 1/(1 + exp((150 - x)/2))

  Phenology_by_precFUN <-
    lapply(Fulanus_Ecorasters_download, summarize_rasters,
      seasons = list(rainy_season = PhenFUN),
      seasons_args = list(x = 'prec'),
      summaryFUN = "raster::weighted.mean",
      summary_args = list(w = "rainy_season")
    )

  expect_is(Phenology_mean_sd, "list")
  expect_is(Phenology_weighted_mean, "list")
  expect_is(Phenology_by_precFUN, "list")

  expect_is(Phenology_mean_sd[[1]], "RasterStack")
  expect_is(Phenology_weighted_mean[[1]], "RasterStack")
  expect_is(Ecology_workspace[[1]], "RasterStack")

  expect_equal(sum(Phenology_mean_sd[[1]][[1]]@data@values), 6798675)
  expect_equal(sum(Phenology_weighted_mean[[1]][[1]]@data@values), 1128901)
  expect_equal(sum(Phenology_by_precFUN[[1]][[1]]@data@values), 1422204)

  expect_equal(names(Phenology_mean_sd[[1]])[[1]], "prec_breeding_sum")
  expect_equal(names(Phenology_weighted_mean[[1]])[[1]], "prec_breeding_weighted.mean")
  expect_equal(names(Phenology_by_precFUN[[1]])[[1]], "prec_rainy_season_raster..weighted.mean")

})
