#' Transform environmental rasters in ecophysiological rasters.
#'
#' \code{EcoPhysiology} Transform environmental rasters in ecophysiological rasters.
#'
#' @param raster_source character or list of RasterStack. You can either input a path to a folder with the required rasters or a list of RasterStack organized by year/scenario.
#' @param Perf_args named list of strings. Correspondence between PerfFUN arguments and raster names.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param PerfFUN function. Function used to modify rasters.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#'
#' FulanusEcoRasters <-
#'   Ecology(
#'     raster_source = "/Users/gabriel/Documents/Mapinguari-development/global_grids_10_minutes",
#'     ext = FulanusDistribution,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     fixed_var = 'alt',
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     Phen_method = 'month',
#'     reorder = TRUE,
#'     separator = '_')
#'
#' perf_functions <-
#' Physiology(formula = performance ~ s(temp, bs = 'cs') + size,
#'  data = FulanusPhysiology,
#'  type = 'GAMM',
#'  random = list(id = ~ 1),
#'  separator = '_'
#' )
#'
#' Perf_rasters <-
#' EcoPhysiology(raster_source = FulanusEcoRasters,
#'  Perf_args = list(temp = 'tmax', size = 10),
#'  separator = '_',
#'  PerfFUN = perf_functions$predict$model_1)
#'
#' @export

EcoPhysiology <- function(raster_source,
  Perf_args,
  separator,
  PerfFUN) {

    if (class(raster_source[[1]]) == 'RasterStack') {

      raster_list <-
        raster_source

    }

  args_rasters <-
    Perf_args %>%
    sapply(., function(x) class(x) == "character") %>%
    `[`(Perf_args, .)

  args_constant <-
    Perf_args %>%
    sapply(., function(x) class(x) == "numeric") %>%
    `[`(Perf_args, .)

    Perf_list <-
      lapply(raster_list, function(x) {

        raster_by_arg <-
          lapply(args_rasters, function(y) {

            x %>%
              names() %>%
              grep(paste("^", y, sep = ""), .) %>%
              `[[`(x, .)

          } # close function
          ) # close alply

        raster_by_arg_by_rep <-
          lapply(raster_by_arg, function(x){

            repeated_names <-
              x %>%
              names() %>%
              strsplit(separator) %>%
              lapply(function(x) `[`(x, length(x)) ) %>%
              unlist()

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

          prediction <- do.call(PerfFUN, args = call_list)

          prediction

        } # close function
        ) # close lapply

        names(Perf_rasters) <- paste("Perf", names(Perf_rasters), sep = separator)

        raster::stack(Perf_rasters)

      } # close function
      ) # close lapply

    return(Perf_list)
  }
