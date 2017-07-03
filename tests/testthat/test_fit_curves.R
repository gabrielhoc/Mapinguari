test_that("fit_curves works for different types of algorthim (GAMM, NLME, FlexParammCurve)", {

   perf_functions <-
     fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
       data = FulanusPhysiology,
       type = 'GAMM',
       random = list(id = ~ 1)
     )

   perf_functions <-
     fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
       data = FulanusPhysiology,
       type = 'GAMM',
       random = list(id = ~ 1),
       correlation = list(nlme::corAR1(form = ~ 1 | id))
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
       type = 'GAMM',
       random = list(id = ~ 1),
       correlation = list(nlme::corAR1(form = ~ 1 | id)
     ))

   correlation_list <- list(a = nlme::corAR1(form = ~ 1 | id),
     b = nlme::corAR1(0.1, form = ~ 1 | id),
     c = nlme::corARMA(form = ~ 1 | id),
     d = nlme::corARMA(0.1, form = ~ 1 | id))

   perf_functions <-
     fit_curves(formula = formula_list,
       data = FulanusPhysiology,
       type = 'GAMM',
       random = list(id = ~ 1),
       correlation = correlation_list
     )

  expect_is(perf_functions, "data.frame")
  expect_is(perf_functions$output[[1]], "gamm")
  expect_is(perf_functions$predict[[1]], "function")

})
