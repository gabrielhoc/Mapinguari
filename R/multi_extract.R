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

    cat('\n\nfolder:', x)

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

# mydir <- 'C:/Users/gabri/Dropbox/Mapinguari/global_rasters_10min_reordered'
#
# temp_coord <-
#   multi_extract(raster_path = mydir,
#                 coord = TtorquatusDistribution_clean[-1],
#                 folders = c("tmax_present", "tmin_present"))
#
# mydir <- "C:/Users/gabri/Dropbox/Projetos/Doutorado/TimeofActivity_MEE"
# microclim_path = paste(mydir, "Analysis/Data/microclim_10m", sep = "/")
# coord <-
#
# temp_mat <-
#   multi_extract(raster_path = paste(mydir, "Analysis/Data/microclim_10m", sep = "/"),
#                 coord = TtorquatusDistribution_clean[-1],
#                 layers = 1:24)
#
# summary_microclim <-
#   function(temp_mat,
#            valueFUN_list,
#            colFUN_list,
#            rowFUN_list,
#            row_group,
#            col_group
#   ) {
#     temp_mat %>%
#       dplyr::mutate(!!! rlang::lang_args(dplyr::enquo(valueFUN_list))) %>%
#       dplyr::group_by(!!! rlang::lang_args(dplyr::enquo(col_group))) %>%
#       dplyr::summarise(!!! rlang::lang_args(dplyr::enquo(colFUN_list))) %>%
#       dplyr::group_by(!!! rlang::lang_args(dplyr::enquo(row_group))) %>%
#       dplyr::summarise(!!! rlang::lang_args(dplyr::enquo(rowFUN_list)))
#   }
#
# tpref <- mean(TtorquatusGradient$temp)
# vtmin <- quantile(TtorquatusGradient$temp)[2]
# vtmax <- quantile(TtorquatusGradient$temp)[4]
# ftmin <- min(TtorquatusField$temp)
# ftmax <- max(TtorquatusField$temp)
#
# htpFUN <- function(x) ifelse(x > tpref, 1, 0)
# hvtFUN <- function(x) ifelse(x > vtmin & x < vtmax, 1, 0)
# hftFUN <- function(x) ifelse(x > ftmin & x < ftmax, 1, 0)
#
# Ttorquatus_ha_mc_table <-
#   temp_mat %>%
#   dplyr::mutate(htp_h = htpFUN(value),
#                 hvt_h = hvtFUN(value),
#                 hft_h = hftFUN(value)) %>%
#   dplyr::group_by(Lon, Lat, file, layer) %>%
#   dplyr::summarise(htp_mh = mean(htp_h),
#                    hft_mh = mean(hft_h),
#                    hvt_mh = mean(hvt_h),
#                    htp_tr_mh = max(htp_h),
#                    hft_tr_mh = max(hft_h),
#                    hvt_tr_mh = max(hvt_h)) %>%
#   dplyr::group_by(Lon, Lat, file) %>%
#   dplyr::summarise(htp = sum(htp_mh),
#                    hft = sum(hft_mh),
#                    hvt = sum(hvt_mh),
#                    htp_tr = sum(htp_tr_mh),
#                    hft_tr = sum(hft_tr_mh),
#                    hvt_tr = sum(hvt_tr_mh))
#
# str(Ttorquatus_ha_mc_table)
#
# nrow(Ttorquatus_ha_mc_table)/72
