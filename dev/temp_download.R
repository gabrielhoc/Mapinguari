
  download_rasters <- function(missing_vars, resolution, ext, projection_model) {

    if (length(missing_vars) == 0) return(0)

    vars_for_download <-
      dplyr::filter(missing_vars, match( vys_vars, downloadable_vars))

    downloaded_list <- lapply(1:nrow(vars_for_download), function(i){

      wc_var <-
        vars_for_download$vys_vars[i] %>%
        grep(downloadable_vars) %>%
        min() %>%
        downloadable_vars[.]

      wc_year <- if (vars_for_download$vys_years[i] == 2050) {50
      } else if (vars_for_download$vys_years[i] == 2070) {70
      } else {vars_for_download$vys_years[i] }

      wc_rcp <- if (is.na(vars_for_download$vys_scenarios[i])) {vars_for_download$vys_scenarios[i]
      } else if (vars_for_download$vys_scenarios[i] == "rcp26") { 26
      } else if (vars_for_download$vys_scenarios[i] == "rcp45") { 45
      } else if (vars_for_download$vys_scenarios[i] == "rcp60") { 60
      } else if (vars_for_download$vys_scenarios[i] == "rcp85") { 85
      }

      if (any(grepl(wc_year, baseline))) {

        var_raster <-
          raster::getData('worldclim', var = wc_var, res = resolution) %>%
          raster::crop(ext)

      } else {

        var_raster <-
          raster::getData('CMIP5', var = wc_var, res = resolution, rcp = wc_rcp, year = wc_year,  model = projection_model) %>%
          raster::crop(ext)
      }

      return(var_raster)

    })

    return(downloaded_list)

  }

