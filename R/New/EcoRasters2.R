
# EcoRasters --------------------------------------------------------------------------------------

# EcoRasters

# To do:
# Description of EcoRasters
# Create option to always or never download or derive, with default being asking
# Ask if dependencies should be downloaded
# Change aliases on entry

EcoRasters2 <- function(raster_source,
  ext = raster::extent(-180, 180, -60, 90),
  margin = 0,
  resolution = 10,
  non_fixed_var = NA,
  fixed_var = NA,
  years = NA,
  scenarios = NA,
  baseline = c("present", "baseline"), # years not subject to scenarios
  separator = "_",
  projection_model = 'MP',
  download = FALSE,
  derive = FALSE,
  StartSeason,
  StopSeason,
  phenology = 'month'

) {

  # variable aliases

  aliases_list <- list(
    tmax_aliases = c("tmax", "tx", "Tmax"),
    tmin_aliases = c("tmin", "tn", "Tmin"),
    prec_aliases = c("prec", "precip", "precipitation"),
    bioclim_aliases = c("bio", "bioclim", "BioClim"),
    PET_aliases = c("PET", "pet"),
    AET_aliases = c("AET", "aet"),
    CWD_aliases = c("CWD", "cwd"),
    solar_radiation_aliases = c("solar_radiation", "solarradation", "solar_rad", "solarrad", "srad"),
    hours_of_sunlight_aliases = c("hours_of_sunlight", "sunlight_hours", "sunlighthours", "sun_hours", "sunhours", "day_length", "daylength"),
    slope_aliases = c("slope","Slope"),
    aspect_aliases = c("aspect", "Aspect"),
    latitude_aliases = c("lat", "Lat", "latitude", "Latitude"),
    longitude_aliases = c("long", "Long", "lon", "Lon","longitude", "Longitude")
  )

  downloadable_vars <-
    list(tmax = aliases_list$tmax_aliases,
      tmin = aliases_list$tmin_aliases,
      prec = aliases_list$prec_aliases,
      bioclim = aliases_list$bioclim_aliases)

  derivable_vars <- list(
    latitude = aliases_list$latitude_aliases,
    longitude = aliases_list$longitude_aliases,
    slope = aliases_list$slope_aliases,
    aspect = aliases_list$aspect_aliases,
    hours_of_sunlight = aliases_list$hours_of_sunlight_aliases,
    solar_radiation = aliases_list$solar_radiation_aliases,
    PET = aliases_list$PET_aliases,
    AET = aliases_list$AET_aliases,
    CWD = aliases_list$CWD_aliases
    )

  # Select extent by species distribution

  if (class(ext) != "Extent") {
    ext <- raster::extent((min(ext$Lon) - margin), (max(ext$Lon) + margin), (min(ext$Lat) - margin), (max(ext$Lat) + margin))
  }

  # Creates combinations of variables, years and scenarios.

  vys_table <- VYScomb(years,
    scenarios,
    non_fixed_var, fixed_var, baseline, separator)

  derived_logical <-
    derivable_vars %>%
    unlist() %>%
    match(vys_table$vys_vars, .) %>%
    any()

 if (derived_logical) {

   # create list of dependencies

   dependencies_list <- list(
     latitude = "altitude",
     longitude = "altitude",
     slope = "altitude",
     aspect = "altitude",
     hours_of_sunlight = c("altitude", "latitude", "longitude"),
     solar_radiation = c("altitude", "slope", "aspect"),
     PET = c("tmin", "tmax", "latitude", "altitude", "slope", "aspect"),
     AET = c("PET", "tmin", "tmax", "latitude", "altitude", "slope", "aspect"),
     CWD = c("AET", "PET", "tmin", "tmax", "latitude", "altitude", "slope", "aspect")
   )

   fixed_list <- c("altitude", "latitude", "longitude", "slope", "aspect" )
   non_fixed_list <- c("tmin", "tmax", "PET", "AET")

   to_be_derived_table <-
     derivable_vars %>%
     unlist() %>%
     `%in%`(vys_table$vys_vars, .) %>%
     `[`(vys_table, .,)

  # Creates table for variables used as dependencies for derived variables

  each_dependencies <-
    plyr::alply(1:nrow(to_be_derived_table), 1, function(i){

      table_slice <-
        to_be_derived_table %>%
        dplyr::slice(i)

      # Compare to aliases list to extract standard name

      derivable_var <-
        rapply(derivable_vars, function(x) {

          x %>%
            match(., table_slice$vys_vars) %>%
            any() %>%
            which()

        } # close function
        ) # close rapply

      derivable_var_name <- names(derivable_var)

      # get dependencies from list

      dependencies <- dependencies_list[[derivable_var_name]]

      dependencies_non_fixed <-
        dependencies %>%
        match(., non_fixed_list) %>%
        is.na() %>%
        `!`() %>%
        `[`(dependencies, .)

      dependencies_fixed <-
        dependencies %>%
        match(., fixed_list) %>%
        is.na() %>%
        `!`() %>%
        `[`(dependencies, .)

      dependencies_table_vys <- VYScomb(table_slice$vys_years,
        table_slice$vys_scenarios,
        dependencies_non_fixed,
        dependencies_fixed,
        baseline,
        separator)

    } # close function
    ) # close alply

  dependencies_table <-
  each_dependencies %>%
    do.call(rbind, .) %>%
    unique()
 }

  derived_not_inputed <- dplyr::anti_join(dependencies_table, vys_table)

  # Identifies raster delivery method and create list with stacks for each year/scenario

  stack_list <- switch(class(raster_source),

    RasterLayer = ,

    RasterStack = lapply(vys_table$vys_names, FetchStack, raster_stack = raster_source),

    character = lapply(vys_table$vys_names, FetchPath, raster_path = raster_source),

    list = FetchList(vys_table$vys_names, raster_source)

  )

  names(stack_list) <- vys_table$vys_names

  cropped_raster_list <- lapply(stack_list[!is.na(stack_list)], function(x){

    raster::crop(x, ext)

  } # close function
  ) # close lapply

  input_index <-
    cropped_raster_list %>%
    names() %>%
    `%in%`(vys_table$vys_names, .)

  # Identifies if rasters required were correclty inputed by user, so function can ask if the user wants to download or derive the ones that were not

  vys_table$source[input_index] <- sapply(cropped_raster_list, function(x) { ifelse(class(x) == 'RasterStack', "input", NA)})

  # Find variables that were not found in input
  absent_vars <-
    dplyr::filter(vys_table, is.na(source))

  if (length(absent_vars$vys_vars) > 0) {

    if (isTRUE(download)) {

      downloaded_rasters <- download_rasters(absent_vars, resolution, ext, projection_model)

      cropped_raster_list <- c(cropped_raster_list, downloaded_rasters)

    }

    if (isTRUE(derive)) {

      derived_rasters <- derive_rasters(absent_vars, resolution, ext, projection_model)

      cropped_raster_list <- c(cropped_raster_list, derived_rasters)

    }

  }

  all_rasters_list <- cropped_raster_list

  # Phenology part

  if (phenology == 'month') final_list <- all_rasters_list

  if (phenology == 'year') {

    StartSeason <- 0
    StopSeason <- 12

    raster_phenology <- lapply(all_rasters_list,
      Phenology_numerical,
      StartSeason,
      StopSeason)

    final_list <- raster_phenology
  }

  if (phenology == 'season') {
    raster_phenology <- lapply(all_rasters_list,
      Phenology_numerical,
      StartSeason,
      StopSeason)

    final_list <- raster_phenology
  }

  return(final_list)

}

# --------------------------------------------------------------------------------------
# Internal functions -------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# Fetch functions -------------------------------------------------------------------------------
# Fetch functions are internal functions that try to find the variables supplied by the user in the raster source

FetchStack <- function(vari, raster_stack){

  # Try to find variable in raster source, if not possible, return NA

  tryCatch({

    success_message <- paste("Variable", vari, "successfully loaded from input!")
    not_found_message <- paste("Unable to find raster for variable", vari)

    input_raster <-
      names(raster_stack) %>%
      stringr::str_which(vari) %>%
      raster_stack[[.]] %>%
      raster::stack()

    message(success_message)

    return(input_raster)

  }, error = function(cond){
    message(not_found_message)
    return(NA)
  })

}

FetchPath <- function(vari, raster_path){

  # Test if path is one global folder with subfolders for each variable. If it is, return a vector with subfolders as reference, if not just return vector with paths

  if (length(raster_path) == 1) {
    raster_lookup <- list.dirs(raster_path, full.names = T)[-1] # [-1] is because the first element is the parent directory
  } else {
    raster_lookup <- raster_path
  }

  success_message <- paste("Variable", vari, "successfully loaded from input!")
  not_found_message <- paste("Unable to find path for variable", vari)
  # Try to find variable in raster source, if not possible, return NA

  tryCatch({

    input_raster <-
      raster_lookup %>%
      stringr::str_which(vari) %>%
      raster_lookup[[.]] %>%
      list.files(pattern = '*.bil$|*.tif$|*.gri$', full.names = T, ignore.case = T) %>%
      raster::stack()

    message(success_message)

    return(input_raster)

  }, error = function(cond){
    message(not_found_message)
    return(NA)
  })

}

FetchList <- function(vari, raster_list) {

  switch(class(raster_list[[1]]),

    RasterLayer = ,

    RasterStack = lapply(vari, FetchStack, raster_stack = raster_list),

    character = lapply(vari, FetchPath, raster_path = unlist(raster_list))

  )
}

# VYScomb --------------------------------------------------------------------------------------

# Internal function. Creates combinations of variables, years and scenarios, preserving instances when year or scenario doesn't apply. Outputs a reference table of combinations. VYS stands for variable, year, scenario.

VYScomb <- function(years = NA,
  scenarios = NA,
  non_fixed_var = NA,
  fixed_var = NA,
  baseline = c("present", "baseline"),
  separator = "_") {

  if (is.na(non_fixed_var[1])) return(data.frame(vys_names = fixed_var))

  vys_names <-
    setdiff(years, baseline) %>%
    outer(scenarios, paste, sep = separator) %>%
    c(intersect(years, baseline)) %>%
    outer(non_fixed_var, ., paste, sep = separator) %>%
    {if (is.na(fixed_var[1])) { c(.) } else {c(., fixed_var) }} %>%
    gsub("_NA", "", .)

  len_seq <-
    strsplit(vys_names, "_") %>%
    sapply(length) %>%
    max() %>%
    seq()

  vys_table <-
    strsplit(vys_names, "_") %>%
    sapply('[', len_seq) %>%
    t() %>%
    as.data.frame()

  if (ncol(vys_table) == 1) vys_table[, c(2, 3)] <- NA
  if (ncol(vys_table) == 2) vys_table[, 3] <- NA

  column_names <-
    c("vars" ,
      "years",
      "scenarios")

  names(vys_table) <- column_names

  vys_table$vys_names <- vys_names

  return(vys_table)

}

# download_rasters-------------------------------------------------------------------------------------------------------
# This function downloads rasters from worldclim


download_rasters <- function(missing_vars, resolution, ext, projection_model) {

  downloadable_vars <- c("tmax", "tx",
    "tmin", "tn",
    "prec", "precip", "precipitation",
    "bio",
    "soil")

  vars_for_download <-
    `$`(missing_vars, "vys_vars") %>%
    match(downloadable_vars, nomatch = 0) %>%
    missing_vars[.,]

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
    } else if (vars_for_download$vys_scenarios[i] == "rcp26") {26
    } else if (vars_for_download$vys_scenarios[i] == "rcp45") {45
    } else if (vars_for_download$vys_scenarios[i] == "rcp60") {60
    } else if (vars_for_download$vys_scenarios[i] == "rcp85") {85
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

# derive_rasters-------------------------------------------------------------------------------------------------------------

derive_rasters <- function(missing_vars, resolution, ext, projection_model) {

  if (length(missing_vars) == 0) return(0)

  PET_aliases <- c("PET", "pet")
  AET_aliases <- c("AET", "aet")
  CWD_aliases <- c("CWD", "cwd")
  solar_radiation_aliases <- c("solar_radiation", "solarradation", "solar_rad", "solarrad", "srad")
  hours_of_sunlight_aliases <- c("hours_of_sunlight", "sunlight_hours", "sunlighthours", "sun_hours", "sunhours", "day_length", "daylength")
  slope_aliases <- c("slope","Slope")
  aspect_aliases <- c("aspect", "Aspect")

  derivable_vars <- c(  PET_aliases,
    AET_aliases,
    CWD_aliases,
    solar_radiation_aliases,
    hours_of_sunlight_aliases,
    slope_aliases,
    aspect_aliases)

  vars_for_derivation <-
    match(missing_vars$vys_vars, derivable_vars) %>%
    missing_vars[.,]

  derived_rasters <-
    plyr::alply(1:nrow(vars_for_derivation), function(i){

      table_slice <-
        vars_for_derivation %>%
        dplyr::slice(i)

      if (any(match(table_slice$vys_vars, PET_aliases))) {}

      if (any(match(table_slice$vys_vars, AET_aliases))) {}

      if (any(match(table_slice$vys_vars, CWD_aliases))) {}

      if (any(match(table_slice$vys_vars, solar_radiation_aliases))) {}

      if (any(match(table_slice$vys_vars, hours_of_sunlight_aliases))) {}

      if (any(match(table_slice$vys_vars, slope_aliases))) {}

      if (any(match(table_slice$vys_vars, aspect_aliases))) {}

    } # close function
    ) # close alply

  return(derived_rasters)

}


# --------------------
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

  inside_months <- raster::mean(rasterstack[[season_months]], na.rm = T)

  outside_months <- raster::mean(rasterstack[[-season_months]], na.rm = T)

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
