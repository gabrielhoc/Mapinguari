#' Transform environmental rasters in ecophysiological rasters.
#'
#' \code{transform_rasters} Get model predictions for a ras
#'
#' @param raster_stack character or list of RasterStack. You can either input a path to a folder with the required rasters or a list of RasterStack organized by year/scenario.
#' @param transformFUN function. Function used to modify rasters.
#' @param transformFUN_args named list of strings. Correspondence between transformFUN arguments and raster names.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param time_res How many layers do time varying variables have? Default is 12, as in 12 months in a year, the time resolution in WorldClim.
#' @param alert integer. Plays a sound alert when function is done running. See documentation of package beepr for description of sound options.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#' Fulanus_Ecorasters_download <-
#'   get_rasters(
#'     ext = FulanusDistribution,
#'     margin = 5,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     alert = 6)
#'
#' perf_functions <-
#'   fit_curves(formula = list(tpc_size = performance ~ s(temp, bs = 'cs') + size,
#'     tpc_no_size = performance ~ s(temp, bs = 'cs')),
#'     data = FulanusPhysiology,
#'     fitFUN = mgcv::gamm,
#'     args_list = list(random = list(id = ~ 1))
#'   )
#'
#' Perf_rasters <-
#'   transform_rasters(raster_stack = Fulanus_Ecorasters_download[[1]],
#'     transformFUN = list(perf = perf_functions$tpc_size$predict),
#'     transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
#'   )
#'
#' # If the functions in `summarize_rasters`` are commutative, you can summarize before transforming, which is much faster.
#'
#' Phenology_mean <-
#'   lapply(Fulanus_Ecorasters_download, summarize_rasters,
#'     seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
#'     summaryFUN = list(tmax = c("mean"), tmin = c("mean"), prec = "sum"))
#'
#' Perf_rasters_mean <-
#'   transform_rasters(raster_stack =  Phenology_mean[[1]],
#'     transformFUN = list(perf = perf_functions$tpc_size$predict),
#'     transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
#'   )
#'
#' # The function works on RasterStacks, if you have a list of stacks, you can apply the function to each element using lapply
#'
#' Perf_rasters_list <- lapply(Phenology_mean, transform_rasters,
#'   transformFUN = list(perf = perf_functions$tpc_size$predict),
#'   transformFUN_args = list(temp = 'tmax', size = mean(FulanusPhysiology$size))
#' )
#'
#'
#' @export

transform_rasters <- function(raster_stack,
  transformFUN,
  transformFUN_args,
  separator = '_',
  time_res = 12,
  alert = NULL
) {

  # split stack by variable

  split_vars <-
    raster_stack %>%
    names() %>%
    stringr::str_split(separator) %>%
    lapply(`[`, 1)

  unique_split_vars <- unique(split_vars)

  separate_list <-
    lapply(unique_split_vars, function(y){

      which(split_vars == y) %>%
        raster_stack[[.]] %>%
        raster::stack()
    }
    )

  names(separate_list) <- unique_split_vars

  # separate fixed from non fixed variables

  non_fixed_logical <-
    lapply(separate_list, function(u) length(names(u)) == time_res) %>%
    unlist()

  non_fixed_list <- separate_list[which(non_fixed_logical)]

  non_fixed_names <- names(separate_list)[which(non_fixed_logical)]

  names(non_fixed_list) <- non_fixed_names

  fixed_list <- separate_list[which(!non_fixed_logical)]

  fixed_names <- names(separate_list)[which(!non_fixed_logical)]

  names(fixed_list) <- fixed_names

  if (class(transformFUN) != 'list') transformFUN <- list(transformFUN)
  if (is.null(names(transformFUN))) names(transformFUN) <- paste("var", 1:length(transformFUN),  sep = separator)

  args_rasters <-
    transformFUN_args %>%
    sapply(., function(x) class(x) == "character" | class(x) == "call") %>%
    `[`(transformFUN_args, .)

  args_constant <-
    transformFUN_args %>%
    sapply(., function(x) class(x) == "numeric") %>%
    `[`(transformFUN_args, .)

  raster_by_arg <-
    lapply(args_rasters, function(y) {

      if (class(y) == 'call') {

        eval(y)

      } else {

        raster_stack %>%
          names() %>%
          grep(paste("^", y, sep = ""), .) %>%
          `[[`(raster_stack, .)

      }

    } # close function
    ) # close laply

  raster_by_arg_by_rep <-
    lapply(raster_by_arg, function(x){

      repeated_names <- lapply(strsplit(names(x), separator), function(y){

        paste(y[-1], collapse = separator)

      }
      )

      if (class(x) == 'RasterStack') {
        repeated_list <- raster::unstack(x)
      } else {
        repeated_list <- list(x)
      }

      names(repeated_list) <- repeated_names

      repeated_list

    }
    )

  reversed_list <-
    raster_by_arg_by_rep %>%
    purrr::transpose()

  list_names <-
  plyr::alply(reversed_list, 1, function(x){

      strsplit(names(x[[1]]), separator)[[1]][-1] %>%
      as.list() %>%
      append(list(sep = separator)) %>%
      do.call(paste, .)

  }
    )

  names(reversed_list) <- list_names

  transformed_rasters <- lapply(reversed_list, function(x){

    prediction <-
      lapply(transformFUN, function(y) {

        new_default <-
        names(args_constant) %in% formalArgs(y) %>%
          `[`(args_constant, .)

        formals(y)[names(new_default)] <- new_default

        calc_stack <- raster::stack(x)

        ncores <- parallel::detectCores() - 1
        raster::beginCluster(ncores, type = 'SOCK')

        output <- raster::clusterR(calc_stack, raster::overlay, args = list(fun = y))

        raster::endCluster()

        output

      }
      )

    prediction_named <-
      plyr::alply(1:length(prediction), 1, function(i){

        names(prediction[[i]]) <- names(prediction)
        prediction[[i]]

      }
      )

    names(prediction_named) <- names(prediction)
    prediction_named

  } # close function
  ) # close lapply

  unreversed_list <-
    transformed_rasters %>%
    purrr::transpose()

  transformed_stack <-
    lapply(unreversed_list, function(x){

      single_var_stack <-
        raster::stack(x)

      names(single_var_stack) <- paste(unlist(lapply(x, names)), names(x), sep = separator)

      single_var_stack

    }
    )

  return(raster::stack(transformed_stack))

}

lagged <- function(var, lag, stack) {

  substitute(lagged_q(var, lag, stack))

}

lagged_q <- function(var, lag, stack = NULL) {

  if (is.null(stack)) stack <- get("x", envir = parent.frame())

  var_stack <-
    stack %>%
    names() %>%
    grep(paste("^", var, sep = ""), .) %>%
    `[[`(stack, .)

  index_lag <- 1:raster::nlayers(var_stack) + lag

  index_lag[index_lag < 1] <- index_lag[index_lag < 1] + raster::nlayers(var_stack)

  if (!is.null(alert)) {beepr::beep(alert)}

  return(var_stack[[index_lag]])

}
