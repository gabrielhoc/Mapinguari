test_that("fit_curves works for different combination of inputs", {

   perf_functions <-
     fit_curves(models = list(tpc_size = mgcv::gamm(performance ~ s(temp, bs = 'cs') + size,
                                                    random = list(id = ~ 1),
                                                    data = FulanusPhysiology),
                              tpc_no_size = mgcv::gamm(performance ~ s(temp, bs = 'cs'),
                                                       random = list(id = ~ 1),
                                                       data = FulanusPhysiology)))

  testthat::expect_is(perf_functions$tpc_size, "list")
  testthat::expect_is(perf_functions$tpc_size$output[[1]], "lme")
  testthat::expect_is(perf_functions$tpc_size$output[[2]], "gam")
  testthat::expect_is(perf_functions$tpc_size$predict, "function")

})
