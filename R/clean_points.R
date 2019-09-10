#' Clean occurrence records
#'
#' \code{clean_points} Eliminates species occurrence records that are too close to each other or at undesired locations.
#'
#' @importFrom magrittr "%>%"
#'
#' @param coord data.frame. Data frame containing longitudes (Lon) and latitudes (Lat) of occurrence records of a species.
#' @param merge_dist numeric. Maximum distance between points to be merged, in meters.
#' @param coord_col vector of strings or integers. If x has more than two columns, indicate the name or position of longitude and latitude columns
#' @param filter_layer RasterLayer. Binary raster with 1 representing the regions where records should be kept and 0 the regions where they should be eliminated.
#' @param na.rm logical. if TRUE, remove lines with NA in any coordinate.
#'
#' @return Data frame with remaining longitudes and latitudes.
#'
#' @examples
#'\dontrun{
#' #First, we need to obtain an altitude raster to filter by altitude.
#' library(raster)
#' alt <- raster::getData("alt", country = "BRA", mask = TRUE)
#'
#' # Then, we clean the points
#'  TtorquatusDistribution_clean <-
#'   clean_points(coord = TtorquatusDistribution,
#'                merge_dist = 20000,
#'                filter_layer = !is.na(alt))
#'}
#' @export

clean_points <- function(coord,
                         merge_dist,
                         coord_col = c("Lon", "Lat"),
                         filter_layer = NULL,
                         na.rm = FALSE
) {

  # find coordinates if `coord` has more than two columns
  coord_only <-
    if (ncol(coord) > 2) coord[coord_col] else
    {if (na.rm == TRUE) coord[!is.na(coord$Lon) | !is.na(coord$Lat),] else coord}

  # Calculate matrix of point distance
  dist_mat <- raster::pointDistance(coord_only, lonlat = TRUE, allpairs = TRUE) < merge_dist

  # set diagonal to NA (it's meaningless, the distance from the point to itself)
  diag(dist_mat) <- NA

  # Detect which points have at least one case of distance inferior to merge distance
  logical_dist <- colSums(dist_mat, na.rm = TRUE) == 0

  # Detect which points are on forbidden values on the reference_layer, if reference_layer and filter_layer are supplied
  logical_raster <-
    if (!is.null(filter_layer)) {
      raster::extract(filter_layer, coord_only) == 1
      } else TRUE

  # subset original table by both logical vectors
  coord_clean <- coord[logical_dist & logical_raster,]

  # print summary
  rbind(n_entries_species = nrow(coord),
        n_entries_clean = nrow(coord_clean)) %>%
    print

  #return table
  return(coord_clean)

}

utils::globalVariables(".")

