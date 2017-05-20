# develop Phen_args (look at Perf_raster)
# make a decision about the grouping of rasters
# Make sure names are preserved

Phen_logistic <- function(raster_list, PhenFUN, Phen_args) {

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
