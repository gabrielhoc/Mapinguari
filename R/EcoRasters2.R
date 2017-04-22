EcoRasters2 <- function(rasters,
                        non_fixed_var,
                        fixed_var,
                        years,
                        scenarios,
                        baseline = c("present", "baseline"), # years not subject to scenarios
                        separator = "_"
) {

  # Identifies which years have different scenarios, creates combinations of those years and scenarios, and append years that don't have scenarios.

  years_scenario <-
    setdiff(years, baseline) %>%
    outer(scenarios, paste, sep = separator) %>%
    c(intersect(years, baseline))

  # Creates combinations of non-fixed variables and year/scenarios and append fixed variables.

  var_list <-
    outer(non_fixed_var, years_scenario, paste, sep = separator) %>%
    as.vector() %>%
    c(fixed_var)

  # Identifies raster delivery method and create list with stacks for each year/scenario

  raster_list <- switch(class(rasters),

                        RasterStack = {
                          lapply(1:length(var_list), function(i){

                            tryCatch({
                              names(rasters) %>%
                                stringr::str_subset(var_list[i]) %>%
                                rasters[.] %>%
                                stack()
                            },
                            error = NA)

                          }
                          )
                        },

                        list = {
                          lapply(1:length(var_list), function(i){

                            tryCatch({
                              rasters %>%
                                stringr::str_subset(var_list[i]) %>%
                                rasters[.] %>%
                                list.files(pattern = c('*.bil$','*.tif$','*.gri$'),full.names=T, ignore.case=T) %>%
                                stack()
                            },
                            error = NA)

                          }
                          )
                        },

                        character = {
                          lapply(1:length(var_list), function(i){

                            tryCatch({
                              list.dirs(rasters, pattern = var_list[i]) %>%
                                list.files(pattern = c('*.bil$','*.tif$','*.gri$'),full.names=T, ignore.case=T) %>%
                                stack()
                            },
                            error = NA)

                          }
                          )

                        }
  )

  names(raster_list) <- var_list

}
