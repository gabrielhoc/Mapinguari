# Fetch functions --------------------------------------------------------------------------------------
# Fetch functions are internals that try to find the variables supplied by the user in the raster source

FetchStack <- function(var, raster_stack){

  tryCatch({ # Try to find variable in raster source, if not possible, return NA

    names(raster_stack) %>%
      stringr::str_which(var) %>%
      raster_stack[[.]] %>%
      raster::stack() %>%
      return()

  }, error = NA)

}

FetchPath <- function(var, raster_path){

  # Test if path is one global folder with subfolders for each variable. If it is, return a vector with subfolders as reference, if not just return vector with paths

  if(length(raster_path) == 1) {
    raster_lookup <- list.dirs(raster_path, full.names = T)[-1] # [-1] is because the first element is the parent directory
  } else {
    raster_lookup <- raster_path
  }

  tryCatch({ # Try to find variable in raster source, if not possible, return NA

    raster_lookup %>%
      stringr::str_which(var) %>%
      raster_lookup[[.]] %>%
      list.files(pattern = '*.bil$|*.tif$|*.gri$', full.names = T, ignore.case = T) %>%
      raster::stack() %>%
      return()

  }, error = NA)

}

FetchList <- function(var, raster_list) {

  switch(class(raster_list[[1]]),

    RasterLayer = ,

    RasterStack = lapply(var_list, FetchStack, raster_stack = raster_list),

    character = lapply(var_list, FetchPath, raster_path = unlist(raster_source))

  )
}

EcoRasters2 <- function(raster_source,
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

  stack_list <- switch(class(raster_source),

    RasterLayer = ,

    RasterStack = lapply(var_list, FetchStack, raster_stack = raster_source),

    character = lapply(var_list, FetchPath, raster_path = raster_source),

    list = FetchList(var_list, raster_source)

  )

  names(stack_list) <- var_list

  return(stack_list)

}
