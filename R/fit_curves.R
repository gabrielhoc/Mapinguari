#' Creates vectorized predict functions from models.
#'
#' \code{fit_curves} Takes inputed models and create vectorized functions able to get the model predictionsfor any value inputed. Also outputs a table comparing models.
#'
#' @param models list. List with models that have methods for function `predict`.
#' @param predict_formals character. Names of variables necessary for predict function. If NULL, function will attempt to retrieve them from the model's formula.
#' @param separator character. Character that separates variable names, years and scenarios.
#'
#' @return Returns a list with a table of statistics for model comparision, and a list for each model containing the original call, its output and a vectorized function that gets predictions for the model.
#'
#' @examples
#'
#' library(mgcv)
#'
#'   fit_curves(FUN_qlist = qlist(tpc_size = gamm(performance ~ s(temp, bs = 'cs') + size,
#'                                                random = list(id = ~ 1),
#'                                                data = FulanusPhysiology),
#'                                tpc_no_size = gamm(performance ~ s(temp, bs = 'cs'),
#'                                                   random = list(id = ~ 1)),
#'                                                   data = FulanusPhysiology))
#'
#' @export
fit_curves <- function(
  models,
  predict_formals = NULL,
  separator = '_'
) {

  if (any(class(models) != 'list')) models <- list(models)

  if (is.null(names(models))) names(models) <- paste("model", 1:length(models), sep = separator)

  model_stats <-
    lapply(models, function(x) {

      if (any(class(x) == 'gamm')) x <- x$lme

      list(
        AIC = stats::AIC(x),
        BIC = stats::BIC(x),
        logLik = stats::logLik(x)
      ) # close list

    }
    )

  # predict functions

  predict_list <- lapply(models, function(x){

    if (any(class(x) == 'gamm')) x <- x$gam

    if (is.null(predict_formals)) {
      predict_formals <-
        x$formula %>%
        all.vars() %>%
        `[`(-1)
    }

    if (is.null(predict_formals)) stop("Please input `predict_formals`")

    predict_formals_list <- vector("list", length(predict_formals))

    names(predict_formals_list) <- predict_formals

    TPC_function <- function() {

      args_mat <- do.call(data.frame, as.list(environment()))

      #output_list <- plyr::alply(args_mat, 1, function(y) stats::predict(x, y, type = 'response'))

      # make appropriate predict for each method

      output_list <- plyr::alply(args_mat, 1, function(y) { switch(class(x)[1],
        glm = predict.glm(x, y, type = 'response'),
        lm = predict.lm(x, y, type = 'response'),
        nls = predict(x, y)[1],
        gam = predict.gam(x, y, type = 'response'),
        gamm = predict.gam(x, y, type = 'response'),
        predict(x, y))
      }
      )

      unlist(output_list)

    } # close TPC function

    formals(TPC_function) <- predict_formals_list

    TPC_function

  } # close factory
  ) # close lapply

  output <- models
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

  output_list <- lapply(names(models), function(x){

    list(model = models[[x]], output = output[[x]], predict = predict[[x]])

  }
  )

  names(output_list) <- names(models)

  final_output <- append(output_list, list(stats = output_stats))

  print(output_stats)

  return(final_output)

}
