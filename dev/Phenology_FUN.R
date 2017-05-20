# data can either be a table of reproductive status and environmental conditions,
# or a table of reproductive status, coordinates and dates

# If data is of the second kind, rasters must be supplied, and the table will be converted into the first kind, by extracting the environmental conditions in at the coordinate and month supplied

PhenFUN <- function(formula,
  data,
  rasters = NULL
  ) {

  if (is.null(rasters)) {

    regression_data <- data

  } else {

    rasters_for_vars <-
      formula %>%
      all.vars %>%
      `[`(., -1) %>%
      `[[`(rasters, .)

    plyr::alply(1:nrow(data), function(i){

      table_slice <-
        data %>%
        dplyr::slice(i)

      table_slice$month %>%
        `[[`(rasters_for_vars, .) %>%
        raster::extract(data.frame(table_slice$lon, table_slice$lat))

    } # close function
      ) # close lapply

  }

  glm(formula, family = binomial (link = 'logit'), data = regression_data) # binomial regression

}
