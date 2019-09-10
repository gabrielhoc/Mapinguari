#' Retrieve and organize spatial rasters.
#'
#' \code{get_rasters} Loads rasters from directory and returns them in an organized list of specified scenarios.
#'
#' @importFrom magrittr "%>%"
#'
#' @param var character. Names of variables to be loaded.
#' @param scenario character. Names of scenarios for the variables.
#' @param raster_path character. Path to folder with raster files. See \code{\link[raster]{writeFormats}} for supported formats
#' @param ext numeric, data. frame or Extent object. Extent to crop rasters.
#' @param coord_col character. Names of columns containing Longitude and Latitude.
#' @param margin numeric. Additional distance to be added to margin of extent, in degrees.
#' @param separator character. Character that separates variable names and scenario names.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#' \dontrun{
#' Fulanus_Ecorasters_present <-
#'   get_rasters(
#'     var = c('prec', 'tmin', 'tmax'),
#'     scenario = 'present',
#'     raster_path = "C:/Users/gabri/Dropbox/Mapinguari/global_grids_10_minutes",
#'     ext = FulanusDistribution[c(2,3)],
#'     margin = 5)
#' }
#' @export

# rename load_rasters?

get_rasters <- function(var = NULL,
                        scenario = NULL,
                        raster_path = NULL,
                        ext = c(-180, 180, -60, 90),
                        coord_col = c("Lon", "Lat"),
                        margin = 0,
                        separator = '_'
) {

  # Select extent by extent object, vector or data frame with coordinates

  ext_format <- switch(class(ext),
                       Extent = ext,
                       numeric = raster::extent(ext),
                       data.frame = raster::extent(min(ext$Lon),
                                                   max(ext$Lon),
                                                   min(ext$Lat),
                                                   max(ext$Lat)))

  # add margins

  ext_margin <- ext_format
  ext_margin[1] <- ext_format[1] - margin
  ext_margin[2] <- ext_format[2] + margin
  ext_margin[3] <- ext_format[3] - margin
  ext_margin[4] <- ext_format[4] + margin

  # compile table with variable and scenario names

  var_table <-
    list.files(raster_path) %>%
    strsplit(separator) %>%
    lapply(function(x) c(x[1], paste0(x[-1], collapse = separator))) %>%
    do.call(rbind, .) %>%
    data.frame %>%
    cbind(1:nrow(.))

  #if var or scenario are NULL, select all in list

  if (is.null(var)) {
    var_checked <- unique(var_table[,1])
  } else {
    var_checked <- var
  }

  if (is.null(scenario)) {
    scenario_checked <- unique(var_table[,2])
  } else {
    scenario_checked <- unique(c("", scenario))
  }

  # narrow down to variables and scenario selected

  var_table_select <- var_table[var_table[,1] %in% var_checked & var_table[,2] %in% scenario_checked,]

  # load selected surfaces

  stack_list <-
    var_table_select[,3] %>%
    `[`(list.files(raster_path, full.names = T), .) %>%
    lapply(function(x) list.files(x,
                                  pattern = '*.grd$|*.asc$|*.sdat$|*.rst$|*.nc$|*.tif$|*.envi$|*.bil$|*.img$',
                                  full.names = T,
                                  ignore.case = TRUE) %>%
             raster::stack())

  # crop rasters

  stack_list_cropped <- lapply(stack_list, raster::crop, ext_margin, snap = "out")

  # stack rasters by scenario

  stack_list_stacked <-
    lapply(unique(var_table_select[ ,2]), function(x){

      which(var_table_select[ ,2] == x) %>%
        stack_list_cropped[.] %>%
        raster::stack()

    })

  names(stack_list_stacked) <- unique(var_table_select[ ,2])

  # stack constant rasters to other stacks

  # if there are rasters other than constant

  if (any(var_table_select[, 2] != "")) {

  #if there are constant rasters

  if ("" %in% var_table_select[, 2]) {

    constant_stack <-
      stack_list_stacked[unique(var_table_select[, 2]) == ""] %>%
      raster::stack()

    final_stack <-
      stack_list_stacked[unique(var_table_select[, 2]) != ""] %>%
      lapply(function(x) raster::stack(x, constant_stack))

  } else {

  # if there are no constant rasters

    final_stack <- stack_list_stacked[unique(var_table_select[, 2]) != ""]

  }
  } else {

  # is there are only constant rasters

    final_stack <- stack_list_stacked

      }

  if (is.null(scenario)) final_stack <- final_stack[[1]]

  return(final_stack)

}

utils::globalVariables(".")
