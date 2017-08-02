#' Generate background pseudoabsence records
#'
#' \code{pseudoabsences} Creates random pseudoabsence coordinates within a radial distance from specified points.
#'
#' @param coord Coordinates of points to generate pseudoabsence points around.
#' @param diameter Diameter of circles around coordinate points.
#' @param n Number of pseudoabsences to be generated.
#'
#' @return Data frame with coordinates of pseudoabsence points
#' @export
#'
#' @examples
#'
#' pseudoabsences(FulanusDistribution[2:3], 150, 2000)
#'
pseudoabsences <- function(coord, diameter, n) {

  pseudo_abs_lon_lat <- as.data.frame(coord, stringsAsFactors = FALSE)
  sp::coordinates(pseudo_abs_lon_lat) <- ~Lon+Lat
  raster::projection(pseudo_abs_lon_lat) <- sp::CRS('+proj=longlat')

  # generate pseudoabsences: create polygons

  pseudo_abs_lon_lat_circles <- dismo::circles(pseudo_abs_lon_lat, d=diameter*1000, lonlat=TRUE)
  pseudo_abs_lon_lat_pol <- rgeos::gUnaryUnion(pseudo_abs_lon_lat_circles@polygons)

  # generate pseudoabsences: number of pseudoabsences samples

  pseudo_abs_lon_lat_samp1 <- sp::spsample(pseudo_abs_lon_lat_pol, n, type='random', iter=25)

  sample_raster <-
    raster::raster() %>%
    raster::crop(raster::extent(pseudo_abs_lon_lat_samp1))

  pseudo_abs_cells <- raster::cellFromXY(sample_raster, pseudo_abs_lon_lat_samp1)
  pseudo_abs_cells <- unique(pseudo_abs_cells)
  pseudo_abs_cells_xy <- raster::xyFromCell(sample_raster, pseudo_abs_cells)

  pseudo_abs_cells_spxy <- sp::SpatialPoints(pseudo_abs_cells_xy, proj4string = sp::CRS('+proj=longlat'))
  raster::crs(pseudo_abs_lon_lat_circles@polygons) <- raster::crs(pseudo_abs_cells_spxy)
  pseudo_abs_o <- sp::over(pseudo_abs_cells_spxy, pseudo_abs_lon_lat_circles@polygons)
  pseudo_abs_xyInside <- pseudo_abs_cells_xy[!is.na(pseudo_abs_o), ]
  psedo_backgr <- pseudo_abs_xyInside

  psedo_coordinates <- psedo_backgr
  colnames(psedo_coordinates) <- c("Lon", "Lat")

  psedo_coordinates
}
