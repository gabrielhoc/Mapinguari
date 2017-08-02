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
#'   fit_curves(models = list(tpc_size = gamm(performance ~ s(temp, bs = 'cs') + size,
#'                                            random = list(id = ~ 1),
#'                                            data = FulanusPhysiology),
#'                            tpc_no_size = gamm(performance ~ s(temp, bs = 'cs'),
#'                                               random = list(id = ~ 1),
#'                                               data = FulanusPhysiology)))
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
    lapply(models, function(xx) {

      if (any(class(xx) == 'gamm')) xx <- xx$lme

      list(
        AIC = try(stats::AIC(xx), silent = TRUE),
        BIC = try(stats::BIC(xx), silent = TRUE),
        logLik = try(stats::logLik(xx), silent = TRUE)
      ) # close list

    }
    )

  # predict functions

  predict_list <- lapply(models, function(xx){

    if (any(class(xx) == 'gamm')) xx <- xx$gam

    if (is.null(predict_formals)) {
      try(predict_formals <-
        xx$formula %>%
        all.vars() %>%
        `[`(-1), silent = TRUE)
    }

    if (is.null(predict_formals)) stop("Please input `predict_formals`")

    predict_formals_list <- vector("list", length(predict_formals))

    names(predict_formals_list) <- predict_formals

    TPC_function <- function(...) {

      args_mat <- do.call(data.frame, append(as.list(environment()), list(...)))

      output <- lapply(1:nrow(args_mat), function(i) {

        zzz <- as.data.frame(args_mat[i, ])
        names(zzz) <- names(args_mat)

        switch(class(xx)[1],
          glm = predict.glm(xx, newdata = zzz, type = "response", ...),
          lm = predict.lm(xx, newdata = zzz, type = "response", ...),
          nls = predict(xx, newdata = zzz, ...)[1],
          gam = mgcv::predict.gam(xx, newdata = zzz, type = "response", ...),
          gamm = mgcv::predict.gam(xx, newdata = zzz, type = "response", ...),
          gbm = gbm::predict.gbm(xx, newdata = zzz, type = "response", ...),
          randomForest = randomForest::predict.randomForest(xx, newdata = zzz, type = "response", ...),
          predict(xx, newdata = zzz, type = "response", ...))
      })

      unlist(output)

    } # close TPC function

    formals(TPC_function) <-  append(predict_formals_list, formals(TPC_function))

    TPC_function

  } # close factory
  ) # close lapply

  output <- models
  predict <- predict_list

  AIC <- unlist(lapply(model_stats, function(x) x$AIC))
  BIC <- unlist(lapply(model_stats, function(x) x$BIC))
  logLik <- unlist(lapply(model_stats, function(x) x$logLik))

  dAIC <- try(AIC - min(AIC), silent = TRUE)
  dBIC <- try(BIC - min(BIC), silent = TRUE)

  rankAIC <- try(rank(dAIC), silent = TRUE)
  rankBIC <- try(rank(dBIC), silent = TRUE)

  if (class(logLik) == 'try-error' | class(logLik) == 'character') logLik <- NA
  if (class(AIC) == 'try-error' | class(AIC) == 'character') AIC <- NA
  if (class(BIC) == 'try-error' | class(BIC) == 'character') BIC <- NA
  if (class(dAIC) == 'try-error' | class(dAIC) == 'character') dAIC <- NA
  if (class(dBIC) == 'try-error' | class(dBIC) == 'character') dBIC <- NA
  if (class(rankAIC) == 'try-error' | class(rankAIC) == 'character') rankAIC <- NA
  if (class(rankBIC) == 'try-error' | class(rankBIC) == 'character') rankBIC <- NA

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
