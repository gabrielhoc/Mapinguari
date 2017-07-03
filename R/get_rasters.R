#' Retrieve and organize spatial environmental information.
#'
#' \code{get_rasters} returns an organized list of environmental rasters.
#'
#' @importFrom magrittr "%>%"
#'
#' @param raster_source character or list of RasterStack. You can either input a path to a folder with the required rasters or a list of RasterStack organized by year/scenario.
#' @param ext numeric vector or dataframe with coordinates. Extension to crop rasters. Input can be either a vector with limiting latitudes and longitudes or a table of coordinates. In case the input is a numeric vector, the order of values should be: western most longitude, easter most longitude, southern most latitude then northern most latitude. Longitude values west of GMT and latitude values south of equator are distinguished by a negative sign. In case the input is a table of coordinates, the points further in each direction will determine the extent for cropping.
#' @param margin numeric. Additional distance to be added to margin of extent, in degrees.
#' @param resolution numeric. Spatial resolution of rasters, in degrees. This is used for downloading only.
#' @param non_fixed_var character vector. Names of the time varying variables to be outputed. This variables will be subject to variation according to argument `years`.
#' @param fixed_var character vector. Names of the time fixed variables to be outputed. This variables will not be subject to variation according to argument `years`.
#' @param years character vector. Names of years for time varying variables.
#' @param scenarios character vector. Names of future scenarios to for time varying variables.
#' @param baseline character vector. Names of years not subject to scenarios.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param projection_model character. Projection model for future variables. Must be any model accepted in \code{raster::getData}
#' @param download_var character vector. Which of the supplied variables should be downloaded?
#' @param reorder logical. If TRUE, will use last two characters of layer names in RasterStacks with 12 layers to order them in ascending order.
#' @param seasons numerical vector or function. Months at the beggining and end of phenological events, or function relating phenological event to environmental conditions. If NULL, no summarizing is made (remains by month).
#' @param seasons_args named list of strings. Correspondence between PhenFUN arguments and raster names.
#' @param alert integer. Plays a sound alert when function is done running. See documentation of package beepr for description of sound options.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#' Ecology_download <-
#'   get_rasters(
#'     ext = FulanusDistribution,
#'     margin = 5,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     alert = 6)
#'
#' Ecology_dir <-
#'   get_rasters(
#'     raster_source = "/Users/gabriel/Documents/Mapinguari-development/global_grids_10_minutes",
#'     ext = FulanusDistribution,
#'     margin = 5,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     fixed_var = "alt",
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     reorder = TRUE,
#'     alert = 4)
#'
#' Ecology_list <-
#'   get_rasters(
#'     raster_source = Ecology_dir,
#'     ext = c(40, 20, 1, 4),
#'     margin = 5,
#'     non_fixed_var = c('prec'),
#'     fixed_var = "alt",
#'     years = c("present", '2050'),
#'     scenarios = c('rcp26', 'rcp85'),
#'     alert = 1)
#'
#' @export

get_rasters <- function(raster_source = NULL,
  ext = c(-180, 180, -60, 90),
  margin = 0,
  resolution = 10,
  non_fixed_var = NULL,
  fixed_var = NULL,
  years = 'present',
  scenarios = NULL,
  baseline = "present",
  separator = '_',
  projection_model = 'MP',
  download_var = NULL,
  seasons = NULL,
  reorder = FALSE,
  seasons_args = NULL,
  summary_args = NULL,
  summaryFUN = NULL,
  download_source = 'pkg-raster',
  alert = NULL
) {

  # Select extent by species distribution

  if (class(ext) == "numeric") {

    ext <- raster::extent(ext)

  }

  if (class(ext) == "data.frame" ) {

    ext <- raster::extent(min(ext$Lon), max(ext$Lon), min(ext$Lat), max(ext$Lat))

  }

  ext[1] <- ext[1] - margin
  ext[2] <- ext[2] + margin
  ext[3] <- ext[3] - margin
  ext[4] <- ext[4] + margin

  # Creates combinations of variables, years and scenarios, accounting for if they vary in time or if they are subject to scenarios.

  vys_table <- VYScomb(years,
    scenarios,
    non_fixed_var, fixed_var, baseline, separator)

  # Download required rasters

  if (is.null(raster_source)) {

  stack_list <-
    download_rasters(
      vys_table = vys_table,
      baseline = baseline,
      resolution = resolution,
      projection_model = projection_model,
      download_source = download_source)

  } else {

  stack_list <- FetchRasters(raster_source, vari = vys_table$vys_names, separator = separator)

  }

  # crop rasters (this is a bottleneck, so I'm parallelizing. There is a package called velox that supposedly crops faster, I haven't tried it yet.)

  ncores <- parallel::detectCores()
  raster::beginCluster(ncores, type = 'SOCK')

  cropped_raster_list <- lapply(stack_list[!is.na(stack_list)], function(x){

    raster::stack(raster::crop(x, ext))

  } # close function
  ) # close lapply

  raster::endCluster()

  # if reorder is 'TRUE', reorder layers by the last two digits

  if (reorder == TRUE) {

    cropped_raster_list <-
      lapply(cropped_raster_list, function(x){

        x %>%
          names() %>%
          stringr::str_sub(., -2) %>%
          qdap::mgsub(letters, 0, .) %>%
          order() %>%
          `[[`(x, .)

      } # close function
      ) # close lapply

  }

  # rename monthly layers, so they are in standard format

  month_names <-
    plyr::alply(names(cropped_raster_list), 1, function(x, sep_in = separator){

      strsplit(x, sep_in) %>%
        `[[`(1) %>%
        `[`(1) %>%
        paste(1:12, sep = sep_in)

    } # close function
    ) # close alply

  renamed_raster_list <-
    plyr::alply(1:length(cropped_raster_list), 1, function(i){

      is_month <-
        cropped_raster_list[[i]] %>%
        names() %>%
        length() %>%
        `==`(12)

      if (is_month) {

        names(cropped_raster_list[[i]]) <- month_names[[i]]

        cropped_raster_list[[i]]

      } else {

        cropped_raster_list[[i]]

      }

    } # close function
    ) # close alply

  names(renamed_raster_list) <- names(cropped_raster_list)

  # stack rasters by combination of year/scenario

  years_scenarios_list <-
    paste(vys_table$years,
      vys_table$scenarios,
      sep = separator) %>%
    gsub("_NA", "", .) %>%
    split(vys_table, .)

  grouped_rasters <-
    lapply(years_scenarios_list, function(x){

      renamed_raster_list %>%
        names() %>%
        match(x$vys_names, .) %>%
        `[`(renamed_raster_list, .) %>%
        unlist() %>%
        raster::stack()

    } # close function
    ) # close lapply

  # appends a copy of 'fixed' variables to every stack

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

  # Insert derive here

  if (!is.null(seasons)) {

    final_list <- lapply(final_list, summarize_rasters,
      seasons_args = seasons_args,
      summaryFUN = summaryFUN,
      summary_args = summary_args,
      separator = separator,
      seasons = seasons)
  }

  if (!is.null(alert)) {beepr::beep(alert)}

  return(final_list)

}

# --------------------------------------------------------------------------------------
# Internal functions -------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# VYScomb --------------------------------------------------------------------------------------

# Internal function. Creates combinations of variables, years and scenarios, preserving instances when year or scenario doesn't apply. Outputs a reference table of combinations. VYS stands for variable, year, scenario.

VYScomb <- function(years = NA,
  scenarios = NA,
  non_fixed_var = NA,
  fixed_var = NA,
  baseline = c("present", "baseline"),
  separator = '_') {

  if (is.null(non_fixed_var[1])) return(data.frame(vys_names = fixed_var))

  vys_names <-
    setdiff(years, baseline) %>%
    outer(scenarios, paste, sep = separator) %>%
    c(intersect(years, baseline)) %>%
    outer(non_fixed_var, ., paste, sep = separator) %>%
    {if (is.null(fixed_var[1])) { c(.) } else {c(., fixed_var) }} %>%
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
    tibble::as.tibble()

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

# Fetch functions -------------------------------------------------------------------------------
# Fetch functions are internal functions that try to find the variables supplied by the user in the raster source

FetchRasters <- function(raster_source, vari, separator){

  FetchPath <- function(raster_path, vari){

    # Test if path is one global folder with subfolders for each variable. If it is, return a vector with subfolders as reference, if not just return vector with paths

    if (length(raster_path) == 1) {
      raster_lookup <- list.dirs(raster_path, full.names = TRUE)[-1] # [-1] is because the first element is the parent directory
    } else {
      raster_lookup <- raster_path
    }

    success_message <- paste("Variable", vari, "successfully loaded from input!")
    not_found_message <- paste("Variable", vari, "not found on input!")
    # Try to find variable in raster source, if not possible, return NA

    tryCatch({

      input_raster <-
        raster_lookup %>%
        stringr::str_which(vari) %>%
        raster_lookup[[.]] %>%
        list.files(pattern = '*.bil$|*.tif$|*.gri$', full.names = T, ignore.case = TRUE) %>%
        raster::stack()

      message(success_message)

      return(input_raster)

    }, error = function(not_found_message){
      message(not_found_message)
      return(NA)
    })

  }

  FetchList <- function(raster_list, vari, separator) {

    success_message <- paste("Variable", vari, "successfully loaded from input!")
    not_found_message <- paste("Variable", vari, "not found on input!")

    split_vari <-
      strsplit(vari, separator)[[1]]

    year_scenario <- ifelse(length(split_vari) == 3, paste(split_vari[2], split_vari[3], sep = separator), split_vari[2])

    tryCatch({

    input_raster <-
    raster_list[[year_scenario]] %>%
      names() %>%
      stringr::str_which(split_vari[1]) %>%
      raster_list[[year_scenario]][[.]] %>%
      raster::stack()

    message(success_message)

    return(input_raster)

    }, error = function(not_found_message){
      message(not_found_message)
      return(NA)

    }) # close tryCatch

  }

  stack_list <- switch(class(raster_source),

    character = lapply(vari, FetchPath, raster_path = raster_source),

    list = lapply(vari, FetchList, raster_list = raster_source, separator = separator)

  )

  names(stack_list) <- vari

  return(stack_list)

}

### download

download_rasters <- function(
  vys_table,
  baseline,
  resolution = 10,
  projection_model = 'MP',
  download_source = 'pkg-raster') {

  if (download_source == 'pkg-raster') {

    vys_table$dataset <- vys_table$years

    vys_table$dataset[vys_table$dataset == '2050' | vys_table$dataset == '2070'] <- 'CMIP5'
    vys_table$dataset[vys_table$dataset == baseline] <- 'worldclim'
    vys_table$years[vys_table$years == '2050'] <- 50
    vys_table$years[vys_table$years == '2070'] <- 70
    vys_table$scenarios[vys_table$scenarios == 'rcp26'] <- 26
    vys_table$scenarios[vys_table$scenarios == 'rcp45'] <- 45
    vys_table$scenarios[vys_table$scenarios == 'rcp60'] <- 60
    vys_table$scenarios[vys_table$scenarios == 'rcp85'] <- 85

    vys_table_download <- vys_table[vys_table$vars == 'tmin' |
        vys_table$vars == 'tmax' |
        vys_table$vars == 'prec' |
        vys_table$vars == 'bio', ]

    stack_list <-
      plyr::alply(vys_table_download, 1, function(x) {

        if (x$years == baseline) {

          raster::getData(name = x$dataset, var = x$vars, res = resolution)

        } else {

          raster::getData(name = x$dataset, var = x$vars, year = x$years, rcp = x$scenarios, res = resolution, model = projection_model)

        }

      } # close function
      ) # close alply

    names(stack_list) <- vys_table_download$vys_names

  }

  return(stack_list)

}


utils::globalVariables(".")
