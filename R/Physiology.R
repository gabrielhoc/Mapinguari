#' Creates functions relating environment to physiology.
#'
#' \code{Physiology} Creates functions relating environment to physiology.
#'
#' @param formula
#' @param data
#' @param type
#' @param fixed
#' @param random
#' @param start
#' @param correlation
#' @param x_axis
#' @param y_axis
#' @param z_axis
#' @param scale
#' @param ...
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#' perf_functions <-
#' Physiology(formula = performance ~ s(temp, bs = 'cs') + size,
#'  data = FulanusPhysiology,
#'  type = 'GAMM',
#'  random = list(id = ~ 1)
#' )
#'
#' @export
Physiology <- function(formula,
  data,
  type, # GAMM or NLME
  fixed = NULL,
  random = NULL,
  start = NULL,
  correlation = NULL,
  x_axis = NULL,
  y_axis = NULL,
  z_axis = NULL,
  scale = NULL,
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

  models_key <- paste("model", seq_len(length(formula)), sep = separator)

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

  # graphs

  if (!is.null(x_axis) & !is.null(y_axis)) {

    # print dispersion plot for data checking

    if (is.null(scale)) scale <- y_axis

    dispersion_plot <- ggplot2::ggplot(data,
                                       ggplot2::aes(y = eval(as.name(y_axis)),
                                                    x = eval(as.name(x_axis)),
                                                    color = eval(as.name(scale)))) +
                       ggplot2::geom_point(size = 6, alpha = 0.7) +
                       ggplot2::theme_bw() +
                       ggplot2::theme(legend.text = ggplot2::element_text(size = 7),
                                      legend.key.size = ggplot2::unit(0.2, "cm"),
                                      legend.spacing = ggplot2::unit(0, "cm"),
                                      panel.grid.major = ggplot2::element_line(colour = "gray"),
                                      panel.grid.minor = ggplot2::element_line(colour = "gray", linetype = "dotted")) +
                       ggplot2::geom_rug(col = "darkred",alpha = .7) +
                       ggplot2::xlab(x_axis) +
                       ggplot2::ylab(y_axis) +
                       ggplot2::labs(colour = scale)

    plot(dispersion_plot)

  plot_2D_list <- lapply(model_table_output_stats$output, function(x){

    plot2D <- plot(x$gam, pages = 1,residuals = TRUE, pch = 19, seWithMean = TRUE, shade = TRUE, shade.col = "gray") #
    title(names(x))

    recordPlot()

  } # close function
  ) # close lapply

  plot_3D_list <- lapply(model_table_output_stats$output, function(x){

    plot_3D <-
      mgcv::vis.gam(x$gam,
        view = c('size','temp'),
        type = "response",
        theta = 55,
        phi = 30,
        color = "heat",
        xlab = "size mm",
        ylab = "Temperature °C",
        zlab = "Performance",
        ticktype = "detailed",
        n.grid = 50) # color: gray; theta is the perspective angle of the graph
    title(names(x))

  } # close function
  ) # close lapply

  } # close graph if

  model_table_final <-
    model_table_output_stats_predict %>%
    dplyr::mutate(model = models_key) %>%
    dplyr::select(model, dplyr::everything())

  return(model_table_final)

}
