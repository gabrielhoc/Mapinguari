#' Retrieve and organize spatial environmental information.
#'
#' \code{Ecology} returns an organized list of environmental rasters.
#'
#' @param raster_source character or list of RasterStack. You can either input a path to a folder with the required rasters or a list of RasterStack organized by year/scenario.
#' @param ext Extent object or dataframe with coordinates. Extension to crop rasters. You can either input an extent object or a table of coordinates, in which case, the points further in each direction will determine the extent for cropping.
#' @param margin numeric. Additional distance to be added to the extent, in degrees.
#' @param resolution numeric. Spatial resolution of rasters, in degrees.
#' @param non_fixed_var character vector. Names of the time varying variables to be outputed.
#' @param fixed_var character vector. Names of the time fixed variables to be outputed.
#' @param years character vector. Names of years for time varying variables.
#' @param scenarios character vector. Names of future scenarios to for time varying variables.
#' @param baseline character vector. Names of years not subject to scenarios.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param projection_model character. Projection model for future variables. Must be any model accepted in \code{raster::getData}
#' @param download logical. Should missing variables that can be downloaded be downloaded?
#' @param derive logical. Should missing variables that can be derived be derived?
#' @param StartSeason numerical. Month of start of a phenological event.
#' @param StopSeason numerical. Month of end of a phenological event.
#' @param phenology character. Either 'month', 'year' or 'season'. 'month' will keep rasters that vary by month as they are, 'year' will average all months, 'season' will average inside and outside month range determined by 'StartSeason' and 'StopSeason'.
#' @param reorder logical. If TRUE, will use last two characters of layer names in RasterStacks with 12 layers to order them in ascending order.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#' FulanusEcoRasters_month <-
#'EcoRasters2(raster_source = "./global_grids_10_minutes/",
#'  ext = dist,
#'  non_fixed_var = c('prec', 'tmin', 'tmax'),
#'  fixed_var = 'alt',
#'  years = c("present", '2050', '2070'),
#'  scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'  phenology = 'month',
#'  reorder = T)

#'FulanusEcoRasters_season <-
#'  EcoRasters2(raster_source = "./global_grids_10_minutes/",
#'    ext = dist,
#'    non_fixed_var = c('prec', 'tmin', 'tmax', 'PET', 'AET', 'CWD'),
#'    fixed_var = 'alt',
#'    years = c("present", '2050', '2070'),
#'    scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'    phenology = 'season',
#'    StartSeason = 3,
#'    StopSeason = 8,
#'    derive = T)
#'

Environment <- function(raster_source,
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
  phenology = 'month',
  reorder = FALSE
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
    lat_aliases = c("lat", "Lat", "latitude", "Latitude"),
    lon_aliases = c("lon", "Long", "long", "Lon", "longitude", "Longitude"),
    alt_aliases = c("alt", "altitude")
  )

  downloadable_vars <-
    list(tmax = aliases_list$tmax_aliases,
      tmin = aliases_list$tmin_aliases,
      prec = aliases_list$prec_aliases,
      bioclim = aliases_list$bioclim_aliases)

  derivable_vars <- list(
    lat = aliases_list$lat_aliases,
    lon = aliases_list$lon_aliases,
    slope = aliases_list$slope_aliases,
    aspect = aliases_list$aspect_aliases,
    hours_of_sunlight = aliases_list$hours_of_sunlight_aliases,
    solar_radiation = aliases_list$solar_radiation_aliases,
    PET = aliases_list$PET_aliases,
    AET = aliases_list$AET_aliases,
    CWD = aliases_list$CWD_aliases
    )

  # substitute aliases for standard names

  sub_list <- lapply(aliases_list, function(x){

    rep(x[[1]], length(x))

  } # close function
  ) # close lapply

  fixed_var <- qdap::mgsub(unlist(aliases_list), unlist(sub_list), fixed_var)
  non_fixed_var <- qdap::mgsub(unlist(aliases_list), unlist(sub_list), non_fixed_var)

  # Select extent by species distribution

  if (class(ext) != "Extent") {
    ext <- raster::extent((min(ext$Lon) - margin), (max(ext$Lon) + margin), (min(ext$Lat) - margin), (max(ext$Lat) + margin))
  }

  # Creates combinations of variables, years and scenarios.

  vys_table <- VYScomb(years,
    scenarios,
    non_fixed_var, fixed_var, baseline, separator)

# Check for any derived variables

  derived_logical <-
    derivable_vars %>%
    unlist() %>%
    match(vys_table$vars, .) %>%
    any()

 if (isTRUE(derived_logical)) {

   # create list of dependencies

   dependencies_list <- list(
     latitude = "alt",
     longitude = "alt",
     slope = "alt",
     aspect = "alt",
     hours_of_sunlight = c("alt", "lat", "lon"),
     solar_radiation = c("alt", "slope", "aspect"),
     PET = c("tmin", "tmax", "lat", "alt", "slope", "aspect"),
     AET = c("PET", "tmin", "tmax", "lat", "alt", "slope", "aspect"),
     CWD = c("AET", "PET", "tmin", "tmax", "lat", "alt", "slope", "aspect")
   )

   fixed_list <- c("alt", "lat", "lon", "slope", "aspect" )
   non_fixed_list <- c("tmin", "tmax", "PET", "AET")

   to_be_derived_table <-
     derivable_vars %>%
     unlist() %>%
     `%in%`(vys_table$vars, .) %>%
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
            match(., table_slice$vars) %>%
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

      dependencies_table_vys <- VYScomb(table_slice$years,
        table_slice$scenarios,
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


  derived_not_inputed <- dplyr::anti_join(dependencies_table, vys_table)

  vys_table_all <- rbind(derived_not_inputed , vys_table)

 } else {

   vys_table_all <- vys_table

 }

  # Identifies raster delivery method and create list with stacks for each year/scenario

  stack_list <- switch(class(raster_source),

    RasterLayer = ,

    RasterStack = lapply(vys_table_all$vys_names, FetchStack, raster_stack = raster_source),

    character = lapply(vys_table_all$vys_names, FetchPath, raster_path = raster_source),

    list = FetchList(vys_table_all$vys_names, raster_source)

  )

  names(stack_list) <- vys_table_all$vys_names

  cropped_raster_list <- lapply(stack_list[!is.na(stack_list)], function(x){

    raster::stack(raster::crop(x, ext))

  } # close function
  ) # close lapply

  input_index <-
    cropped_raster_list %>%
    names() %>%
    `%in%`(vys_table_all$vys_names, .)

  # Identifies if rasters required were correclty inputed by user, so function can ask if the user wants to download or derive the ones that were not

  vys_table_all$source[input_index] <- sapply(cropped_raster_list, function(x) { ifelse(class(x) == 'RasterStack', "input", NA)})

  # Find variables that were not found in input
  absent_vars <-
    dplyr::filter(vys_table_all, is.na(source))

  if (length(absent_vars$vars) > 0) {

    if (isTRUE(download)) {

      download_rasters(absent_vars, resolution, ext, projection_model)

    }

    if (isTRUE(derive)) {

      derived_rasters  <- derive_rasters(absent_vars, derivable_vars, aliases_list, cropped_raster_list, resolution, ext, projection_model)

      cropped_raster_list <- append(cropped_raster_list, derived_rasters)

    }

  }

  all_rasters_list <- cropped_raster_list

  if (reorder == T) {

    all_rasters_list <-
      lapply(all_rasters_list, function(x){

        x %>%
          names() %>%
          stringr::str_sub(., -2) %>%
          qdap::mgsub(letters, 0, .) %>%
          order() %>%
          `[[`(x, .)

      } # close function
      ) # close lapply

  }

  month_names <-
  plyr::alply(names(all_rasters_list), 1, function(x){

    paste(x, 1:12, sep = separator)

  } # close function
    ) # close alply

  renamed_raster_list <-
  plyr::alply(1:length(all_rasters_list), 1, function(i){

    is_month <-
    all_rasters_list[[i]] %>%
      names() %>%
      length() %>%
      `==`(12)

    if (is_month) {

      names(all_rasters_list[[i]]) <- month_names[[i]]

      all_rasters_list[[i]]

    } else {

      all_rasters_list[[i]]

    }

  } # close function
    ) # close alply

  names(renamed_raster_list) <- names(all_rasters_list)

  # Phenology part

  if (phenology == 'month') {

    final_list <- renamed_raster_list

    }

  if (phenology == 'year') {

    StartSeason <- 0
    StopSeason <- 12

    raster_phenology <- lapply(renamed_raster_list,
      Phenology_numerical,
      StartSeason,
      StopSeason)

    final_list <- raster_phenology
  }

  if (phenology == 'season') {

    raster_phenology <- lapply(renamed_raster_list,
      Phenology_numerical,
      StartSeason,
      StopSeason)

    final_list <-
      plyr::alply(1:length(raster_phenology), 1, function(i){

        if
        (length(names(raster_phenology[[i]])) > 1) {

        names(raster_phenology[[i]]) <-
          paste(names(raster_phenology)[i], names(raster_phenology[[i]]), sep = separator)

        }

        raster_phenology[[i]]

      } # close function
        ) # close lapply

    names(final_list) <- names(raster_phenology)

  }

  years_scenarios_list <-
    paste(vys_table_all$years,
      vys_table_all$scenarios,
      sep = separator) %>%
    gsub("_NA", "", .) %>%
    split(vys_table_all, .)

  grouped_rasters <-
    lapply(years_scenarios_list, function(x){

      final_list %>%
        names() %>%
        match(x$vys_names, .) %>%
        `[`(final_list, .) %>%
        raster::stack()

    } # close function
    ) # close lapply

  names(years_scenarios_list)[names(years_scenarios_list) == 'NA'] <- "fixed"

  names(grouped_rasters) <- names(years_scenarios_list)

  final_list <-
    grouped_rasters %>%
    names() %>%
    `!=`('fixed') %>%
    `[`(grouped_rasters, .) %>%
    lapply(., function(x){

      raster::stack(x, grouped_rasters[["fixed"]])

    } # close function
    ) # close lapply

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
    strsplit(vys_names, separator) %>%
    sapply(length) %>%
    max() %>%
    seq()

  vys_table <-
    strsplit(vys_names, separator) %>%
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
    `$`(missing_vars, "vars") %>%
    match(downloadable_vars, nomatch = 0) %>%
    missing_vars[.,]

  downloaded_list <- lapply(1:nrow(vars_for_download), function(i){

    wc_var <-
      vars_for_download$vars[i] %>%
      grep(downloadable_vars) %>%
      min() %>%
      downloadable_vars[.]

    wc_year <- if (vars_for_download$years[i] == 2050) {50
    } else if (vars_for_download$years[i] == 2070) {70
    } else {vars_for_download$years[i] }

    wc_rcp <- if (is.na(vars_for_download$scenarios[i])) {vars_for_download$scenarios[i]
    } else if (vars_for_download$scenarios[i] == "rcp26") {26
    } else if (vars_for_download$scenarios[i] == "rcp45") {45
    } else if (vars_for_download$scenarios[i] == "rcp60") {60
    } else if (vars_for_download$scenarios[i] == "rcp85") {85
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


derive_rasters <- function(missing_vars, derivable_vars, aliases_list, cropped_raster_list, resolution, ext, projection_model) {

  if (length(missing_vars) == 0) return(0)

  vars_for_derivation_no_order <-
    missing_vars$vars %in% names(derivable_vars) %>%
    missing_vars[.,]

  vars_for_derivation <-
    vars_for_derivation_no_order %>%
    dplyr::mutate(vars = factor(vars, levels = names(derivable_vars))) %>%
    dplyr::arrange(vars)

  dependencies <- list()

  derived_rasters <- list()

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$lat_aliases)))) {

        if (class(cropped_raster_list[[1]]) != 'RasterStack') stop("Unable to find a raster to derive latitude") # Get better error message

        lat_raster  <- raster::init(cropped_raster_list[[1]], 'y')
        names(lat_raster) <- 'lat'

        dependencies$lat <- lat_raster
        derived_rasters$lat <- lat_raster

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$lon_aliases)))) {

        if (class(cropped_raster_list[[1]]) != 'RasterStack') stop("Unable to find a raster to derive longitude") # Get better error message

        lon_raster <- raster::init(cropped_raster_list[[1]], 'x')

        dependencies$lon <- lon_raster
        derived_rasters$lon <- lon_raster

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$hours_of_sunlight_aliases)))) {



      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$slope_aliases)))) {

        if (class(cropped_raster_list$alt) != 'RasterStack') stop("Unable to find variable alt") # Get better error message

        slope_raster <- raster::stack(raster::terrain(cropped_raster_list$alt, opt = 'slope', unit = 'radians', neighbors = 8))

        dependencies$slope <- slope_raster
        derived_rasters$slope <- slope_raster

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$aspect_aliases)))) {

        if (class(cropped_raster_list$alt) != 'RasterStack') stop("Unable to find variable alt") # Get better error message

        aspect_raster  <- raster::stack(raster::terrain(cropped_raster_list$alt, opt = 'aspect', unit = 'radians', neighbors = 8))

        dependencies$aspect <- aspect_raster
        derived_rasters$aspect <- aspect_raster

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$solar_radiation_aliases)))) {}

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$PET_aliases)))) {

        # Calculating monthly PET
        # The following code takes the function in EcoHydRology and applies
        # it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision)

        PET_vars <-
          vars_for_derivation %>%
          dplyr::filter(vars == 'PET')

        PET_rasters <-
          plyr::alply(PET_vars, 1, function(x){

            tmax_year_scenario <-
              paste('tmax', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            tmin_year_scenario <-
              paste('tmin', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            PET_year_scenario <-
              paste('PET', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            # loop over months, change to lapply

            PET_month_list <-
              plyr::alply(c(1:12), 1, function(j) {

                lat <- raster::values(dependencies$lat) * pi/180
                evap <- raster::raster(cropped_raster_list[[tmax_year_scenario]], 1)
                slope <- raster::values(dependencies$slope)
                aspect <- raster::values(dependencies$aspect)
                Tmax <- raster::values(raster::subset(cropped_raster_list[[tmax_year_scenario]], j))/10
                Tmin <- raster::values(raster::subset(cropped_raster_list[[tmin_year_scenario]], j))/10
                d <- data.frame(day = (30 * j) - 15, Tmin, Tmax, slope, aspect, lat) # day at the midpoint of each month
                d[is.na(d)] <- 0
                Es_PET <- EcoHydRology::PET_fromTemp(Jday = d$day, Tmax_C = d$Tmax, Tmin_C = d$Tmin, lat_radians = d$lat, aspect = d$aspect, slope = d$slope) * 1000
                raster::values(evap) <- Es_PET
                raster::raster(evap)
                names(evap) <- paste("PET", j, sep = "")

                return(evap)

              } # close function
              ) # close alply

            PET_year_scenario_stack <- raster::stack(PET_month_list) * 100
            names(PET_year_scenario_stack) <- paste("PET", 1:12, sep = "")

            return(PET_year_scenario_stack)

          } # close function
          )  # close alply

        names(PET_rasters) <- PET_vars$vys_names

        dependencies <- append(dependencies, PET_rasters)
        derived_rasters <- append(derived_rasters, PET_rasters)

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$AET_aliases)))) {

        # Estimating AET using a simple bucket model # Duncan Golicher code:
        # AET is Actual evapotranspiration and always lower than PET potential evapotranspiration
        # and can be much lower when the soil profile is well below field capacity.

        AET_vars <- dplyr::filter(missing_vars, vars == "AET")

        AET_rasters <-
          plyr::alply(AET_vars, 1, function(x){

            PET_year_scenario <-
              paste('PET', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            prec_year_scenario <-
              paste('prec', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            AET_year_scenario <-
              paste('PET', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            Bucket <- raster::raster(dependencies[[PET_year_scenario]]/100, 1)

            for (n in 1:2) {
              for (i in 1:359) {
                mn <- 1 + i %/% 30                                # %/% indicates integer division
                NewAET <- Bucket
                NewBucket <- raster::values(Bucket)
                rain <- raster::values(raster::subset(cropped_raster_list[[prec_year_scenario]], mn))/30
                alpha <- (NewBucket - 200)/300
                evap <- raster::values(raster::subset(dependencies[[PET_year_scenario]]/100, mn)) * alpha * 0.8   #     A fudge factor for stomatal control.
                NewBucket <- NewBucket + (rain) - evap
                NewBucket[NewBucket > 500] <- 500
                NewBucket[NewBucket < 200] <- 200
                raster::values(Bucket) <- NewBucket
                raster::values(NewAET) <- evap * (NewBucket > 200)
                if (n > 1 && (i %% 30) - 15 == 0) {     ## i%%30 will run 1 to 359 and determine position in 30 e.g., 1 is 1 and 61 is 1
                  if (mn == 1) {
                    AET <- suppressMessages(raster::brick(NewAET))
                  }
                  if (mn > 1) {
                    AET <- suppressMessages(raster::addLayer(AET, NewAET))
                  }
                }
              }
            }

            AET_year_scenario_stack <- suppressMessages(AET * 100)
            names(AET_year_scenario_stack) <- paste("AET", 1:12, sep = "")
            return(AET_year_scenario_stack)

          } # close function
          )  # close alply

        names(AET_rasters) <- AET_vars$vys_names

        dependencies <- append(dependencies, AET_rasters)
        derived_rasters <- append(derived_rasters, AET_rasters)

      }

      if (isTRUE(any(match(vars_for_derivation$vars, aliases_list$CWD_aliases)))) {

        CWD_vars <- dplyr::filter(missing_vars, vars == "CWD")

        CWD_rasters <-
          plyr::alply(CWD_vars, 1, function(x){

            PET_year_scenario <-
              paste('PET', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            AET_year_scenario <-
              paste('AET', x$year, x$scenario, sep = separator) %>%
              gsub("_NA", "", .)

            CWD_year_scenario <- dependencies[[PET_year_scenario]] - dependencies[[AET_year_scenario]]

            names(CWD_year_scenario) <- paste("CWD", 1:12, sep = "")

            return(CWD_year_scenario)

          } # close function
          )  # close alply

        names(CWD_rasters) <- CWD_vars$vys_names

        dependencies <- append(dependencies, CWD_rasters)
        derived_rasters <- append(derived_rasters, CWD_rasters)

      }

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
