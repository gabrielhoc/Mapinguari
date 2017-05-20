#' Groups rasters by duration of phenological event.
#'
#' \code{Phenology} Groups rasters by duration of phenological event.
#'
#' @param raster_list list of RasterStacks. Only accept stacks with 12 layers.
#' @param Phen_args named list of strings. Correspondence between PhenFUN arguments and raster names.
#' @param PhenFUN function. Function relating phenological event to environmental conditions.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#' @export
Phenology <- function(raster_list, PhenFUN, Phen_args) {

  final_list <-
  lapply(raster_list, function(x){

    Phen_logical <-
      plyr::alply(1:length(names(x)), 1, function(i) {

        raster::calc(x = x[[i]], fun = PhenFUN)

      } # close function
      ) # close alply

    raster::plot(raster::stack(Phen_logical))

    Var_logical <-
      plyr::alply(1:length(names(x)), 1, function(i) {

        x[[i]] * Phen_logical[[i]]

      } # close function
      ) # close alply

    # number of months able to breed

    number_months <-
      Phen_logical %>%
      raster::stack() %>%
      sum()

    Var_season <-
      sum(raster::stack(Var_logical))/number_months

    Phen_logical_negative <-
      lapply(Phen_logical, function(x) 1 - x)

    Var_logical_negative <-
      plyr::alply(1:length(names(x)), 1, function(i) {

        x[[i]] * Phen_logical_negative[[i]]

      } # close function
      ) # close alply

    number_months_negative <-
      Phen_logical_negative %>%
      raster::stack() %>%
      sum()

    Var_non_season <-
      sum(raster::stack(Var_logical_negative))/number_months_negative

    return(raster::stack(Var_season, Var_non_season))

  }
  )

  return(final_list)

}

# Numerical phenology function
# averages accross rasters within and without season, as supplied by user (numerical)

Phenology_numerical <- function(rasterstack, StartSeason, StopSeason) {

  if (length(names(rasterstack)) != 12) return(rasterstack)

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

  inside_months <- raster::mean(rasterstack[[season_months]], na.rm = TRUE)

  outside_months <- raster::mean(rasterstack[[-season_months]], na.rm = TRUE)

  fraction_first_month <-
    rasterstack[[start_month]] %>%
    `*`(start_fraction)

  fraction_last_month <-
    rasterstack[[stop_month]] %>%
    `*`(stop_fraction)

  season_average <- inside_months + fraction_first_month + fraction_last_month
  no_season_average <- outside_months - fraction_first_month - fraction_last_month

  output <- raster::stack(season_average, no_season_average)
  names(output) <- c("Season", "NonSeason")

  return(output)

}
