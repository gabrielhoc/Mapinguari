CWD_vars <- dplyr::filter(missing_vars, vars == "CWD")

CWD_rasters <-
  plyr::alply(CWD_vars, 1, function(x){

    PET_year_scenario <-
      paste('PET', x$year, x$scenario, sep = "_") %>%
      gsub("_NA", "", .)

    AET_year_scenario <-
      paste('AET', x$year, x$scenario, sep = "_") %>%
      gsub("_NA", "", .)

    CWD_year_scenario <- cropped_raster_list[[PET_year_scenario]] - cropped_raster_list[[AET_year_scenario]]

    names(CWD_year_scenario) <- paste("CWD", 1:12, sep = "")

    return(CWD_year_scenario)

  } # close function
  )  # close alply

names(CWD_rasters) <- CWD_vars$vys_names

cropped_raster_list <- append(cropped_raster_list, CWD_rasters)
