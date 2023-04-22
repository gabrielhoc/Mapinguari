#' Sinervo (2010) hours of activity model
#'
#' \code{sin_h} Simulates daily variation in temperature and counts amount of time above a temperature threshold, as seen in Sinervo et al. 2010.
#'
#' @param tmax Raster* object. Maximum temperature raster.
#' @param tmin Raster* object. Minimum temperature raster.
#' @param thrs numeric. Temperature threshold in same unit as rasters.
#' @param res numeric. time resolution in parts of hour.
#'
#' @return numeric. Amount of time in hours above temperature threshold in simulated daily temperature variation.
#'
#' @examples
#' sin_h(28, 10, 23, 3)
#'
#' @export

sin_h <- function(tmax, tmin, thrs, res) {

  s0 <- 1:res
  h0 <- 1:24

  s <- expand.grid(s0, h0)[[1]]
  h <- expand.grid(s0, h0)[[2]]

  mapply(tmax, tmin, FUN = function(x, y){
    day_temps <-
      ((x - y)/2 * sin((pi/12) * (h + (s/res)) - 3 * (pi/4))) + (x + y)/2

    sum(ifelse(day_temps > thrs, 1/res, 0))
  })
}

