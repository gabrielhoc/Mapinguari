#' Transform environmental rasters in ecophysiological rasters.
#'
#' \code{transform_rasters} Transform environmental rasters in ecophysiological rasters.
#'
#' @param raster_stack character or list of RasterStack. You can either input a path to a folder with the required rasters or a list of RasterStack organized by year/scenario.
#' @param transformFUN_args named list of strings. Correspondence between transformFUN arguments and raster names.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param transformFUN function. Function used to modify rasters.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#' FulanusEcoRasters <-
#'   get_rasters(
#'     raster_stack = "/Users/gabriel/Documents/Mapinguari-development/global_grids_10_minutes",
#'     ext = FulanusDistribution,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     fixed_var = 'alt',
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     reorder = TRUE,
#'     separator = '_')
#'
#' perf_functions <-
#' fit_curves(formula = performance ~ s(temp, bs = 'cs') + size,
#'  data = FulanusPhysiology,
#'  type = 'GAMM',
#'  random = list(id = ~ 1),
#'  separator = '_'
#' )
#'
#' Perf_rasters <-
#' transform_rasters(raster_stack = FulanusEcoRasters,
#'  transformFUN_args = list(temp = 'tmax', size = 10),
#'  separator = '_',
#'  transformFUN = perf_functions$predict$model_1)
#'
#' @export

transform_rasters <- function(raster_stack,
  transformFUN,
  transformFUN_args,
  separator,
  time_res = 12
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
  if (is.null(names(transformFUN))) names(transformFUN) <- paste("variable", 1:length(transformFUN),  sep = separator)

    if (class(raster_stack[[1]]) == 'RasterStack') {

      raster_list <-
        raster_stack

    }

  args_rasters <-
    transformFUN_args %>%
    sapply(., function(x) class(x) == "character" | class(x) == "call") %>%
    `[`(transformFUN_args, .)

  args_constant <-
    transformFUN_args %>%
    sapply(., function(x) class(x) == "numeric") %>%
    `[`(transformFUN_args, .)

    Perf_list <-
      lapply(raster_list, function(x) {

        raster_by_arg <-
          lapply(args_rasters, function(y) {

            if (class(y) == 'call') {

              eval(y)

            } else {

            x %>%
              names() %>%
              grep(paste("^", y, sep = ""), .) %>%
              `[[`(x, .)

            }

          } # close function
          ) # close laply

        raster_by_arg_by_rep <-
          lapply(raster_by_arg, function(x){

            repeated_names <- 1:raster::nlayers(x)

            repeated_list <- raster::unstack(x)

            names(repeated_list) <- repeated_names

            repeated_list

          } # close function
          ) # close lapply

        reversed_list <-
          raster_by_arg_by_rep %>%
          purrr::transpose()

        Perf_rasters <- lapply(reversed_list, function(x){

          call_list <-
            x %>%
            append(args_constant)

          prediction <-
            lapply(transformFUN, function(y) {

              do.call(y, args = call_list)

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

        names(Perf_rasters) <- paste("Perf", names(Perf_rasters), sep = separator)

        raster::stack(Perf_rasters)

      } # close function
      ) # close lapply

    return(Perf_list)
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

  return(var_stack[[index_lag]])

}
