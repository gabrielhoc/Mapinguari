
# EcoRasters --------------------------------------------------------------------------------------

# EcoRasters

# To do:
# Description of EcoRasters
# Handle cases when user doesn't input all arguments
# Create a table showing which variables were successfully fetched, so the function can detect which to download and which to derive
# Create option to always or never download or derive, with default being asking

EcoRasters2 <- function(raster_source,
  non_fixed_var = NA,
  fixed_var = NA,
  years = NA,
  scenarios = NA,
  baseline = c("present", "baseline"), # years not subject to scenarios
  separator = "_"
) {

  # Creates combinations of variables, years and scenarios.

  vys_table <- VYScomb(years,
    scenarios,
    non_fixed_var, fixed_var, baseline, separator)

  # Identifies raster delivery method and create list with stacks for each year/scenario

  stack_list <- switch(class(raster_source),

    RasterLayer = ,

    RasterStack = lapply(vys_table$vys_names, FetchStack, raster_stack = raster_source),

    character = lapply(vys_table$vys_names, FetchPath, raster_path = raster_source),

    list = FetchList(vys_table$vys_names, raster_source)

  )

  names(stack_list) <- vys_table$vys_names

  # Identifies if rasters required were correclty inputed by user, so function can ask if the user wants to download or derive the ones that were not

  vys_table$source <- sapply(stack_list, function(x) { ifelse(class(x) == 'RasterStack', "input", NA)})

  # Find variables that were not found in input
  absent_vars <-
    which(is.na(vys_table$source)) %>%
    vys_table[.,]

  downloadable_vars <- c("tmax", "tx",
    "tmin", "tn",
    "precipitation", "precip", "prec",
    "alt",
    "bio",
    "soil")

  derivable_vars <- c("PET", "pet",
    "AET", "aet",
    "CWD", "cwd",
    "solar_radiation", "solarradation", "solar_rad", "solarrad", "srad",
    "hours_of_sunlight", "sunlight_hours", "sunlighthours", "sun_hours", "sunhours", "day_length", "daylength",
    "slope_aspect","slopeaspect", "slope", "aspect")

  vars_for_download <-
    match(absent_vars$vys_vars, downloadable_vars) %>%
    absent_vars[.,]

  vars_for_derivation <-
    match(absent_vars$vys_vars, derivable_vars) %>%
    absent_vars[.,]

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

    input_raster<-
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

  column_names <-
    "vys_vars" %>%
    {if (is.na(years[1])) { . } else {append(., "vys_years") }} %>%
    {if (is.na(scenarios[1])) { . } else {append(., "vys_scenarios") }}

  names(vys_table) <- column_names

  vys_table$vys_names <- vys_names

  return(vys_table)

}
