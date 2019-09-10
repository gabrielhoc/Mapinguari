#' Creates vectorized predict functions from models.
#'
#' \code{get_predict} Takes inputed models and create vectorized functions able to get the model predictionsfor any value inputed. Also outputs a table comparing models.
#' nls
#' gam
#' glm
#' lm
#' randomForest
#' gbm
#' gls
#' bam
#'
#' @param models list. List with models to create the prediction function. The model objects must have methods for function `predict`.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param ... additional arguments to be passed to predict function (specific for the method of the models supplied).
#'
#' @return Returns a list of vectorized functions that get predictions for the models inputted. The functions generated do not perform lazy evaluation, the user must be explicit
#'
#' @examples
#'
#' library(mgcv)
#'
#' perf_no_size <-
#'   gamm(performance ~ s(temp, bs = 'cs'),
#'        random = list(id = ~ 1),
#'        data = TtorquatusPerformance)
#'
#' perf_size <-
#'   gamm(performance ~ s(temp, bs = 'cs') + size,
#'        random = list(id = ~ 1),
#'        data = TtorquatusPerformance)
#'
#' perf_functions <- get_predict(list(perf_s = perf_size,
#'                                    perf_ns = perf_no_size),
#'                               type = "response")
#'
#' perf_nsFUN <- perf_functions$perf_ns
#' perf_sFUN <- perf_functions$perf_s
#'
#' perf_nsFUN(temp = 30)
#' perf_sFUN(temp = 30, size = 70)
#' perf_nsFUN(temp = 30:35)
#' perf_sFUN(temp = 30, size = 70:75)
#' perf_sFUN(temp = 30:35, size = 70:75)
#'
#' @export

get_predict <- function(
  models,
  separator = '_',
  ...
) {

  add_arg <- as.list(substitute(list(...))[-1])

  # put models in a list if they are not already
  if (any(class(models) != 'list')) models <- list(models)

  # if the list is not named, give standard names
  if (is.null(names(models))) names(models) <- paste("model", 1:length(models), sep = separator)

  # crate predict functions

  predict_list <- lapply(models, function(xx){

    if (any(class(xx) == "gamm")) xx <- xx$gam

    predict_formals <-
      if ("gls" %in% class(xx)) names(xx$parAssign)[names(xx$parAssign) != "(Intercept)"] else
        if ("gbm" %in% class(xx)) xx$var.names else
          if ("nls" %in% class(xx)) names(xx$dataClasses) else
            attr(xx$terms, "term.labels")

    predict_formals_list <- vector("list", length(predict_formals))

    names(predict_formals_list) <- predict_formals

    pred_method_try <-
      lapply(class(xx), function(m){
      try(utils::getS3method("predict", m), silent = TRUE)
      })

    pred_method <-
      which(sapply(pred_method_try, class) == "function")[1] %>%
      `[[`(pred_method_try, .)

    original_formals <- formals(pred_method)
    original_formals$object <- NULL
    original_formals$newdata <- NULL

    modified_formals <-
    utils::modifyList(original_formals, add_arg[intersect(names(add_arg), names(original_formals))])

    pred_function <- function() {}

    formals(pred_function) <-
      c(predict_formals_list,
      modified_formals)

    body(pred_function) <-
      expression(as.vector(do.call(
        pred_method,
        c(list(object = xx,
             newdata = data.frame(as.list(environment())[names(environment()) %in% names(predict_formals_list)])),
             modified_formals))))

    pred_function

  } # close factory
  ) # close lapply

  # get models AIC, BIC and log likelihood
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

  print(output_stats)

  if (length(predict_list) > 1) {predict_list} else {predict_list[[1]]}

}
