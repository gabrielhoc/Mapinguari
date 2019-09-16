#' Gets values from multiple rasters.
#'
#' \code{multi_extract} Extract values of multiple spatial rasters for a set of geographical coordinates.
#'
#' @param raster_path character. Path to the folder with raster folders.
#' @param coord data.frame or matrix. Longitude and Latitude from where to extract raster values.
#' @param folders character. folders from which to get rasters for extraction. If NULL, all folders are selected.
#' @param files numeric. Index for raster files to be extracted from each folder. If NULL, all files are selected.
#' @param layers numeric. Index for layers to be extracted from each raster file. If NULL, all layers are selected.
#' @param ncores integer. Number of cores to use in parallel processing.
#'
#' @return Data frame with extracted values from multiple rasters
#'
#' @examples
#'
#' \dontrun{
#' # replace rasterpath with the directory on your computer containing worldclim data
#'
#' temp_pres <-
#'   multi_extract(raster_path = "mydir/rasters/worldclim/global_rasters_10min/",
#'                 coord = TtorquatusDistribution[-1],
#'                 folders = c("tmax_present", "tmin_present"))
#'                 }
#'
#' @export

# reorder
# difference in layer names

multi_extract <- function(raster_path,
                          coord,
                          folders = NULL,
                          files = NULL,
                          layers = NULL,
                          ncores = 1) {

  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))

  #parallel::clusterExport(cl, varlist = ls(environment()), envir = environment())

  parallel::clusterEvalQ(cl, library(mgcv))
  parallel::clusterEvalQ(cl, library(magrittr))# change to Mapinguari

  folder_names <- list.files(raster_path)

  selected_folders <- if (is.null(folders)) folder_names else folders

  lapply(selected_folders, function(x) {

    message('\n\nfolder:', x)

    file_names <-
      paste(raster_path, x, sep = '/') %>%
      list.files(pattern = '*.grd$|*.asc$|*.sdat$|*.rst$|*.nc$|*.tif$|*.envi$|*.bil$|*.img$',
                 ignore.case = TRUE)

    selected_files <- if (is.null(files)) file_names else file_names[files]

    parallel::clusterExport(cl, varlist = c("x", "coord", "raster_path", "selected_files", "layers"), envir = environment())

    parallel::parLapply(cl, 1:length(selected_files), function(y){

      file_to_extract <- selected_files[y]

      selected_layers <- if (is.null(layers)) 1 else layers

      lapply(selected_layers, function(h){

        raster::raster(paste(raster_path, x, file_to_extract, sep = '/'), band = h) %>%
          raster::extract(coord) %>%
          data.frame(coord, value = ., layer = h)

      }) %>%
        do.call(rbind, .) %>%
        cbind(., file = file_to_extract, file_ind = y)

    }) %>%
      do.call(rbind, .)  %>%
      cbind(., folder = x)

  }) %>%
    do.call(rbind, .)
}
