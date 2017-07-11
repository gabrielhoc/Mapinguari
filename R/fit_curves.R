#' Creates functions relating environment to physiology.
#'
#' \code{fit_curves} Creates functions relating environment to physiology. You can run multiple models if you input their arguments as lists.
#'
#' @param formula formula. Model formula.
#' @param data data frame. Table containing variables in model formula.
#' @param fitFUN call. Algorithm used for model.
#' @param args_list named list. list of arguments to be passed to the function in fitFUN.
#' @param separator character. Character that separates variable names, years and scenarios.
#'
#' @return Returns a tibble containing model specifications, statistics and a predictor function.
#' @examples
#' perf_functions <-
#'   fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
#'     tpc_no_size = performance ~ s(temp, bs = 'cs')),
#'     data = FulanusPhysiology,
#'     fitFUN = mgcv::gamm,
#'     args_list = list(random = list(id = ~ 1))
#'   )
#'
#' perf_functions2 <-
#'   fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
#'     tpc_no_size = performance ~ s(temp, bs = 'cs')),
#'     data = FulanusPhysiology,
#'     fitFUN = gamm,
#'     args_list = list(random = list(id = ~ 1),
#'       correlation = nlme::corAR1(form = ~ 1 | id))
#'   )
#'
#' formula_list <-
#'   list(
#'     performance ~ s(temp, bs = 'cs'),
#'     performance ~ s(temp, bs = 'cs') + size,
#'     performance ~ s(temp, bs = 'cs') + hydration,
#'     performance ~ s(temp, bs = 'cs') + hydration + size
#'   )
#'
#' perf_functions <-
#'   fit_curves(formula = formula_list,
#'     data = FulanusPhysiology,
#'     fitFUN = mgcv::gamm,
#'     args_list = list(random = list(id = ~ 1))
#'   )
#'
#' # allow multiple arguments
#'
#' correlation_list <- list(correlation = nlme::corAR1(form = ~ 1 | id),
#'   correlation = nlme::corAR1(0.1, form = ~ 1 | id),
#'   correlation = nlme::corARMA(form = ~ 1 | id),
#'   correlation = nlme::corARMA(0.1, form = ~ 1 | id))
#'
#' perf_functions <-
#'   fit_curves(formula = formula_list,
#'     data = FulanusPhysiology,
#'     fitFUN = mgcv::gamm,
#'     args_list = correlation_list
#'   )
#'
#' @export
fit_curves <- function(formula,
  data,
  fitFUN,
  args_list,
  separator = '_'
  ) {

  # make this work for functions that don't have a 'formula' argument (e. g. nlme)
  formula_arg <- 'formula'

  if (class(formula) != 'list') formula <- list(formula)

  if (is.null(names(formula))) names(formula) <- paste("model", 1:length(formula), sep = separator)

  if (!identical(names(formula_arg), names(formula))) formula_arg <- lapply(formula, function(x) formula_arg )

  if (!identical(names(fitFUN), names(formula))) fitFUN <- lapply(formula, function(x) fitFUN )

  if (!identical(names(args_list), names(formula))) args_list <- lapply(formula, function(x) args_list )

  fitFUN_args <-
    lapply(names(formula), function(x){

      args_minus_formula <-
      fitFUN[[x]] %>%
        formalArgs() %>%
        match(., names(args_list[[x]]), nomatch = FALSE) %>%
        `[`(args_list[[x]], .) %>%
        append(list(data = data))

      formula_list <- list(formula[[x]])

      names(formula_list) <- formula_arg[[x]]

      append(args_minus_formula, formula_list)
    }
    )

  names(fitFUN_args) <- names(formula)

  fit_result <-
  lapply(names(formula), function(x){

    do.call(fitFUN[[x]], args = fitFUN_args[[x]])

  }
    )

  names(fit_result) <- names(formula)

  model_stats <-
  lapply(fit_result, function(x) {

    if (any(class(x) == 'gamm')) x <- x$lme

    list(
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
      logLik = stats::logLik(x)
    ) # close list

  }
  )

  # predict functions

  predict_list <- lapply(fit_result, function(x){

    if (any(class(x) == 'gamm')) x <- x$gam

    args_names <-
      x$formula %>%
      all.vars() %>%
      `[`(-1)

    args_list <- vector("list", length(args_names))

    names(args_list) <- args_names

    TPC_function <- function() {

      input <- as.list(match.call())[-1]

      frmls <- formals()

      all_args_list <-
        `!`(names(frmls) %in% names(input)) %>%
        `[`(frmls, .) %>%
        append(input, .) %>%
        lapply(eval)

      args_mat <- do.call(data.frame, all_args_list)

      plyr::alply(args_mat, 1, function(y) stats::predict(x, y)) %>%
        unlist()

    } # close TPC function

    formals(TPC_function) <- args_list

    TPC_function

  } # close factory
  ) # close lapply

  args <- args_list
  output <- fit_result
  predict <- predict_list

  AIC <- unlist(lapply(model_stats, function(x) x$AIC))
  BIC <- unlist(lapply(model_stats, function(x) x$BIC))
  logLik <- unlist(lapply(model_stats, function(x) x$logLik))

  dAIC <- AIC - min(AIC)
  dBIC <- BIC - min(BIC)

  rankAIC <- rank(dAIC)
  rankBIC <- rank(dBIC)

  output_stats <-
    data.frame(logLik = logLik, AIC = AIC, BIC = BIC, dAIC, dBIC, rankAIC, rankBIC)

  output_list <- lapply(names(formula), function(x){

    list(formula = formula[[x]], args = args[[x]], output = output[[x]], predict = predict[[x]])

  }
    )

  names(output_list) <- names(formula)

  final_output <- append(output_list, list(stats = output_stats))

  print(output_stats)

  return(final_output)

}
