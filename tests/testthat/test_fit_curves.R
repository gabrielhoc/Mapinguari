test_that("fit_curves works for different combination of inputs", {

   perf_functions <-
     fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
                               tpc_no_size = performance ~ s(temp, bs = 'cs')),
       data = FulanusPhysiology,
       fitFUN = mgcv::gamm,
       args_list = list(random = list(id = ~ 1))
     )

   perf_functions2 <-
     fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
                               tpc_no_size = performance ~ s(temp, bs = 'cs')),
       data = FulanusPhysiology,
       fitFUN = gamm,
       args_list = list(random = list(id = ~ 1),
                        correlation = nlme::corAR1(form = ~ 1 | id))
     )

   formula_list <-
     list(
       performance ~ s(temp, bs = 'cs'),
       performance ~ s(temp, bs = 'cs') + size,
       performance ~ s(temp, bs = 'cs') + hydration,
       performance ~ s(temp, bs = 'cs') + hydration + size
       )

   perf_functions <-
     fit_curves(formula = formula_list,
       data = FulanusPhysiology,
       fitFUN = mgcv::gamm,
       args_list = list(random = list(id = ~ 1))
     )

   # allow multiple arguments

   correlation_list <- list(correlation = nlme::corAR1(form = ~ 1 | id),
                            correlation = nlme::corAR1(0.1, form = ~ 1 | id),
                            correlation = nlme::corARMA(form = ~ 1 | id),
                            correlation = nlme::corARMA(0.1, form = ~ 1 | id))

   perf_functions <-
     fit_curves(formula = formula_list,
       data = FulanusPhysiology,
       fitFUN = mgcv::gamm,
       args_list = correlation_list
     )

  expect_is(perf_functions, "data.frame")
  expect_is(perf_functions$output[[1]], "gamm")
  expect_is(perf_functions$predict[[1]], "function")

})
