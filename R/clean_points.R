#' Clean occurrence records
#'
#' \code{clean_points} Eliminates species occurrence records that are too close to each other or at undesired locations.
#'
#' @param coord data.frame. Data frame containing longitudes (Lon) and latitudes (Lat) of occurrence records of a species.
#' @param merge_dist numeric. Maximum distance between points to be merged.
#' @param reference_layer Raster* object. Any raster for the area containing the occurrence records.
#' @param layer_filter Values for locations at reference raster which occurrence records should be eliminated.
#' @param dist_unit character. Unit for the merging distance. Either 'km' for kilometers or 'degrees' for decimal degrees.
#'
#' @return Data frame with remaining longitudes and latitudes.
#'
#' @examples
#'
#' FulanusEcoRasters_present <-
#'   get_rasters(
#'     raster_source = "/Users/gabriel/Documents/Mapinguari/global_grids_10_minutes",
#'     ext = FulanusDistribution,
#'     margin = 5,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     fixed_var = 'alt',
#'     years = c("present"),
#'     reorder = TRUE)
#'
#' alt <- FulanusEcoRasters_present$present$alt
#'
#' clean_points(coord = FulanusDistribution,
#'   merge_dist = 2,
#'   reference_layer = alt > 500 & alt < 1000,
#'   layer_filter = 0)
#'
#' @export

clean_points <- function(coord,
                         merge_dist = 2,
                         reference_layer,
                         layer_filter = NULL,
                         dist_unit = 'km') {

    if (dist_unit == 'km') merge_dist <- merge_dist/100 # 1 km ~ 0.01 degrees (also 0.05 degrees ~ 5 km)

    # subset Lon and Lat

    Lon_Lat <- subset(coord, select = c(Lon, Lat))

    # get hr_res to for resolution to less than an hour

    area_not_round <- (geosphere::areaPolygon(Lon_Lat))/1000000 #area in square Km round
    area <- round(area_not_round, 0) #area in square Km round to integer
    area_hr_res <- 10 - (round(log10(ifelse(area + 1 > 10000000, 10000000, area + 1)), 0) + 1) #this function assing an interger from 2 to 9 (for really small area 1 km2) based on the area in km2. However, it forces to 2 for extremely large distributions > 10 000 000 km2

    # process to reduce redundant localities

    sp::coordinates(coord) <- ~Lon+Lat #set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
    raster::crs(coord) <- raster::crs(reference_layer) #Get or set the coordinate reference system (CRS) of a Raster* object.
    r <- try(raster::raster(coord))
    raster::res(r) <-  merge_dist # y = 0.05 then 5 km ~0.05 degrees resolution
    r_e <- raster::extend(r, raster::extent(r) + 1)
    r_e_acsel <- dismo::gridSample(coord, r_e, n = 1)
    r_e_acsel_df <- data.frame(r_e_acsel)

    # exclude localities in the ocean

    georef <- r_e_acsel_df
    georef_layer <- cbind(georef, layer = raster::extract(reference_layer, georef, method = "bilinear"))
    species_selected_coordinates <- georef_layer[complete.cases(georef_layer$layer),]
    names(species_selected_coordinates)[3] <- names(reference_layer)

    # subset if altitude range is included

    if (!is.null(layer_filter)) species_selected_coordinates <- species_selected_coordinates[species_selected_coordinates[,3] != layer_filter,]

    n_entries_species <- nrow(Lon_Lat)
    n_entries_clean <- nrow(species_selected_coordinates)
    summary_clean_df <- data.frame(n_entries_raw = n_entries_species, n_entries_clean = n_entries_clean, km_resolution_merging = merge_dist*100, stringsAsFactors = FALSE)

  ##########################################################################################

  # bind summaries

  n_entries_clean_bind <- do.call(rbind, summary_clean_df)

  # return list

  print(n_entries_clean_bind)
  return(species_selected_coordinates)

}
