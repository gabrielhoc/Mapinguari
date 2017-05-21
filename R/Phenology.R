#' Groups rasters by duration of phenological event.
#'
#' \code{Phenology} Groups rasters by duration of phenological event.
#'
#' @param raster_list list of RasterStacks. Only accept stacks with 12 layers.
#' @param Phen_args named list of strings. Correspondence between PhenFUN arguments and raster names.
#' @param PhenFUN function. Function relating phenological event to environmental conditions.
#' @param StartSeason numerical. Month of start of a phenological event.
#' @param StopSeason numerical. Month of end of a phenological event.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'FulanusEcoRasters <-
#'   Ecology(
#'     raster_source = "/Users/gabriel/Documents/Mapinguari-development/global_grids_10_minutes",
#'     ext = FulanusDistribution,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     fixed_var = 'alt',
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     phenology = 'month',
#'     reorder = TRUE,
#'     separator = "_")
#'
#'     Phenolohy(FulanusEcoRasters,
#'     StartSeason = 4,
#'     StopSeason = 11)
#'
#'    PhenFUN <- function(x) 1/(1 + exp((26 - x)/2))
#'
#'     Phenology(FulanusEcoRasters,
#'     PhenFUN,
#'     Phen_args = list(x = 'prec'))
#'
#' @export
Phenology <- function(raster_list,
  PhenFUN = NULL,
  Phen_args = NULL,
  StartSeason = 0,
  StopSeason = 12) {

  if (!is.null(PhenFUN)) {

    final_list <-
      lapply(raster_list, function(x){

        split_vars <-
          x %>%
          names() %>%
          stringr::str_split('_') %>%
          lapply(`[`, 1)

        unique_split_vars <-
          unique(split_vars)

        separate_list <-
          lapply(unique_split_vars, function(y){

            which(split_vars == y) %>%
              x[[.]] %>%
              raster::stack()

          }
          )

        names(separate_list) <- unique_split_vars

        non_fixed_logical <-
          lapply(separate_list, function(u) length(names(u)) == 12) %>%
          unlist()

        non_fixed_list <- separate_list[which(non_fixed_logical)]

        non_fixed_names <- names(separate_list)[which(non_fixed_logical)]

        names(non_fixed_list) <- non_fixed_names

        fixed_list <- separate_list[which(!non_fixed_logical)]

        fixed_names <- names(separate_list)[which(!non_fixed_logical)]

        names(fixed_list) <- fixed_names

        vars_only <-
          non_fixed_list[names(non_fixed_list) == Phen_args] %>%
          `[[`(1)

        Phen_logical <-
          raster::calc(x = vars_only, fun = PhenFUN) %>%
          raster::stack()

        Var_logical <-
          lapply(non_fixed_list, function(z) {

            plyr::alply(1:12, 1, function(i) z[[i]] * Phen_logical[[i]])

          } # close function
          ) # close alply

        # number of months able to breed

        number_months <-
          Phen_logical %>%
          raster::stack() %>%
          sum()

        Var_season <-
          lapply(Var_logical, function(w) sum(raster::stack(w))/number_months)

        Phen_logical_negative <- 1 - Phen_logical

        Var_logical_negative <-
          lapply(non_fixed_list, function(z) {

            plyr::alply(1:12, 1, function(i) z[[i]] * Phen_logical_negative[[i]])

          } # close function
          ) # close alply

        number_months_negative <-
          Phen_logical_negative %>%
          raster::stack() %>%
          sum()

        Var_non_season <-
          lapply(Var_logical_negative, function(w) sum(raster::stack(w))/number_months_negative )

        grouped_by_var <-
        lapply(1:length(Var_season), function(i){

          non_fixed_output <-
          raster::stack(Var_season[[i]], Var_non_season[[i]])

          names(non_fixed_output) <- paste(unique_split_vars[[i]], c("Season", "NonSeason"), sep = "_")

          non_fixed_output

        }
          )

        all_output <- raster::stack(append(grouped_by_var, fixed_list))

      }
      )

      } else {

        if (StartSeason < 0 | StartSeason > 12 | StopSeason < 0 | StopSeason > 12) stop("Months outside range.")

        start_month <- StartSeason %/% 1
        start_fraction <- StartSeason %% 1
        stop_month <- StopSeason %/% 1
        stop_fraction <- StopSeason %% 1

        # roll months so starting month is first
        # this avoid complications when stop month is smaller than starting month

        rolled_months <-
          c(0:11) %>%
          `+`(start_month) %>%
          `%%`(12) %>%
          `+`(1)

        season_months <-
          rolled_months %>%
          `==`(stop_month) %>%
          which() %>%
          `:`(1, .) %>%
          `[`(rolled_months, .)

        final_list <-
          lapply(raster_list, function(x,
            .season_months = season_months,
            .start_month = start_month,
            .stop_month = stop_month,
            .start_fraction = start_fraction,
            .stop_fraction = stop_fraction){

            if (length(names(x)) != 12) return(x)

            inside_months <- raster::mean(x[[.season_months]], na.rm = TRUE)

            outside_months <- raster::mean(x[[-.season_months]], na.rm = TRUE)

            fraction_first_month <-
              x[[.start_month]] %>%
              `*`(.start_fraction)

            fraction_last_month <-
              x[[.stop_month]] %>%
              `*`(.stop_fraction)

            season_average <- inside_months + fraction_first_month + fraction_last_month
            no_season_average <- outside_months - fraction_first_month - fraction_last_month

            output <- raster::stack(season_average, no_season_average)
            names(output) <- c("Season", "NonSeason")

            return(output)

          }
          )

        }

    return(final_list)

  }
