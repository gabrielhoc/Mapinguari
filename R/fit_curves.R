#' Creates functions relating environment to physiology.
#'
#' \code{fit_curves} Creates functions relating environment to physiology. You can run multiple models if you input their arguments as lists.
#'
#' @param formula formula. Model formula.
#' @param data data frame. Table containing variables in model formula.
#' @param type character. Algorithm used for model. 'GAMM' or 'NLME'
#' @param fixed named list. See \code{nlme::nlme}
#' @param random named list. See \code{mgcv::gamm} and \code{nlme::nlme}
#' @param start named list. See \code{nlme::nlme}
#' @param correlation named list. See \code{mgcv::gamm} and \code{nlme::nlme}
#' @param ... further parameters for \code{mgcv::gamm} or \code{nlme::nlme}
#'
#' @return Returns a tibble containing model specifications, statistics and a predictor function.
#' @examples
#' perf_functions <-
#'   fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
#'     data = FulanusPhysiology,
#'     type = 'GAMM',
#'     random = list(id = ~ 1)
#'   )
#'
#' perf_functions <-
#'   fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
#'     data = FulanusPhysiology,
#'     type = 'GAMM',
#'     random = list(id = ~ 1),
#'     correlation = list(nlme::corAR1(form = ~ 1 | id))
#'   )
#'
#' formula_list <-
#'   list(
#'     performance ~ s(temp, bs = 'cs'),
#'     performance ~ s(temp, bs = 'cs') + size,
#'     performance ~ s(temp, bs = 'cs') + hydration,
#'     performance ~ s(temp, bs = 'cs') + hydration + size
#'     )
#'
#' perf_functions <-
#'   fit_curves(formula = formula_list,
#'     data = FulanusPhysiology,
#'     type = 'GAMM',
#'     random = list(id = ~ 1),
#'     correlation = list(nlme::corAR1(form = ~ 1 | id)
#'   ))
#'
#' correlation_list <- list(a = nlme::corAR1(form = ~ 1 | id),
#'   b = nlme::corAR1(0.1, form = ~ 1 | id),
#'   c = nlme::corARMA(form = ~ 1 | id),
#'   d = nlme::corARMA(0.1, form = ~ 1 | id))
#'
#' perf_functions <-
#'   fit_curves(formula = formula_list,
#'     data = FulanusPhysiology,
#'     type = 'GAMM',
#'     random = list(id = ~ 1),
#'     correlation = correlation_list
#'   )
#'
#' @export
fit_curves <- function(formula,
  data,
  type, # GAMM or NLME
  fixed = NULL,
  random = NULL,
  start = NULL,
  correlation = NULL,
  ...
) {

  ellipsis_args <- list(...)

  gamm_ellipsis <-
    mgcv::gamm %>%
    formals() %>%
    names() %>%
    match(., names(ellipsis_args), nomatch = FALSE) %>%
    `[`(ellipsis_args, .)

  nlme_ellipsis <-
    nlme::nlme %>%
    formals() %>%
    names() %>%
    match(., names(ellipsis_args), nomatch = FALSE) %>%
    `[`(ellipsis_args, .)

  # if argument is not in a list, put it in a list

  if (class(formula) != 'list') formula <- list(formula)
  if (class(type) != 'list') type <- list(type)

  # list arguments

  table_args <- list(formula = formula,
    type = unlist(type),
    fixed = fixed,
    random = random,
    start = start,
    correlation = correlation)

  # Create key for table

  models_key <- paste("model", seq_len(length(formula)), sep = "_")

  # remove NULLs and create table

  model_table <-
    table_args %>%
    rlist::list.clean(fun = is.null, recursive = FALSE) %>%
    tibble::as_tibble()

  ######################################### run models ###################################

  gamm_or_nlme <- function(table){

    plyr::alply(1:nrow(table), 1, function(i){

      table_slice <- table[i, ]

      switch(table_slice$type,

        GAMM = {

          gamm_args <-
            table_slice %>%
            unlist(recursive = FALSE) %>%
            c(gamm_ellipsis)

          gamm_args$data <- as.list(data)

          do.call(mgcv::gamm, args = gamm_args)

        }, # close GAMM

        NLME = {

          nlme_args <-
            table_slice %>%
            unlist(recursive = FALSE) %>%
            c(nlme_ellipsis)

          nlme_args$data <- as.list(data)

          do.call(nlme::nlme, args = nlme_args)

        } # close NLME

      ) # close switch
    } # close alply function
    ) # close alply
  } # close function

  model_output <-
    model_table %>%
    gamm_or_nlme()

  names(model_output) <- models_key

  model_table_output <-
    dplyr::mutate(model_table, output = model_output)

  # model evaluation

  model_stats <-
  plyr::alply(1:nrow(model_table_output), 1, function(i){

  table_slice <- model_table_output[i,]

  switch(table_slice$type,
    GAMM = {

      list(
        AIC = stats::AIC(table_slice$output[[1]]$lme),
        BIC = stats::BIC(table_slice$output[[1]]$lme),
        logLik = stats::logLik(table_slice$output[[1]]$lme)
      ) # close list

    }, # close GAMM
    NLME = {

      # insert nlme

    } # close NLME
    ) # close switch
  } # close function
  )# close alply

  names(model_stats) <- models_key

  model_table_output_stats <-
    dplyr::mutate(model_table_output, stats = model_stats)

  # predict functions

  predict_list <- lapply(model_table_output_stats$output, function(x){

    formula_gam <- x$gam

    args_names <-
      formula_gam$formula %>%
      all.vars() %>%
      `[`(-1)

    args_list <- vector("list", length(args_names))

    names(args_list) <- args_names

    TPC_function <- function() {

      pred_data <-
        match.call() %>%
        as.list() %>%
        `[`(-1)

      is_raster <-
        sapply(pred_data, function(x) {
        x %>%
        class() %>%
            grepl('^Raster', .)
        }
        ) %>%
        any()

      if (is_raster) {

        args_stack <-
          pred_data %>%
          sapply(., function(x) {

            x %>%
              class() %>%
              grepl('^Raster', .)

          }
            ) %>%
          `[`(pred_data, .) %>%
          raster::stack()

        args_constant <-
          pred_data %>%
          sapply(., function(x) class(x) == "numeric") %>%
          `[`(pred_data, .)

        P <-
        raster::predict(model = formula_gam, object = args_stack, const = args_constant)

      } else {

      P <-
        predict(formula_gam, pred_data) %>%
        as.vector()

      }

      return(P)

    } # close TPC function

    formals(TPC_function) <- args_list

    TPC_function

  } # close factory
  ) # close lapply

  model_table_output_stats_predict <-
    dplyr::mutate(model_table_output_stats, predict = predict_list)

  model_table_final <-
    model_table_output_stats_predict %>%
    dplyr::mutate(model = models_key) %>%
    dplyr::select(model, dplyr::everything())

  names(model_table_final$formula) <- models_key

  return(model_table_final)

}
