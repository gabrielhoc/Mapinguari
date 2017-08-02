#' Gets average month day lengths for an area
#'
#' \code{daylengthFUN} Generates surfaces with information on day length for each month accross an area.
#'
#' @param reference_raster RasterStack or RasterLayer. Any raster representative of the are you want daylengths to.
#'
#' @return Returns a RasterStack with 12 layers, one for each month, containing information on the duration of the day at each pixel.
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
#' daylengthFUN(FulanusEcoRasters_present$present)
#'
#' @export

daylengthFUN <- function(reference_raster) {

    reference_layer <- reference_raster[[1]]
    lat_raster  <- raster::init(reference_layer, 'y')

    day_months <- list(c(1:31),
                       c(32:59),
                       c(60:90),
                       c(91:120),
                       c(121:151),
                       c(152:181),
                       c(182:212),
                       c(213:243),
                       c(244:273),
                       c(274:304),
                       c(305:334),
                       c(336:366))

  day_length_list  <-
    lapply(day_months, function(x){

      daylengths_in_this_month <-
      lapply(x, function(y){

  geosphere::daylength(raster::values(lat_raster), doy = y)

      })

      days_dataframe <- data.frame(daylengths_in_this_month)

      day_length_raster <- lat_raster
      raster::values(day_length_raster) <- apply(days_dataframe, 1, mean)

      names(day_length_raster) <- "daylength"

      day_length_raster

    })

  return(raster::stack(day_length_list))

}

#' Relative humidity from temperature and water vapor pressure
#'
#' \code{rhFUN}  Calculates relative humidity from air temperature in Celsius and water vapor pressure in milibars
#'
#' @param temp numeric. temperature in celsius
#' @param vapor numeric. water vapor pressure in milibars
#'
#' @return a vector of relative humidity values, in decimal.
#'
#' @examples
#'
#' rhFUN(25, 20)
#'
#' rhFUN(25:40, 20:35)
#'
#' @export
#'
rhFUN <- function(temp, vapor) {

  rh <- vapor/(6.11*10^(7.5*temp/(237.7 + temp)))
  ifelse(rh < 1, rh, 1)

}

#' Wrapper around \code{PET_fromTemp} function from package \code{EcoHydrology}
#'
#' \code{PETFUN} Gets Potential EvapoTranspiration (PET) rasters from maximum temperature, minimum temperature and altitude rasters by applying function \code{PET_fromTemp} from package \code{EcoHydrology}
#'
#' @param tmax Raster* object. Maximum temperature raster.
#' @param tmin Raster* object. Minimum temperature raster.
#' @param alt Raster* object. Altitude raster.
#'
#' @return Returns a RasterLayer with estimates of Potential EvapoTranspiration in milimiters.
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
#' tmax <- FulanusEcoRasters_present$present[[25:36]]/10
#' tmin <- FulanusEcoRasters_present$present[[13:24]]/10
#'
#' PET <- PETFUN(tmax, tmin, alt)
#'
#' @export

PETFUN <- function(tmax, tmin, alt) {

  forest = 0 # set this as an argument later

  lat_rad <- alt
  raster::values(lat_rad) <- raster::values(raster::init(tmax, 'y')) * pi/180

  slope <- raster::stack(raster::terrain(alt, opt = 'slope', unit = 'radians', neighbors = 8))
  aspect  <- raster::stack(raster::terrain(alt, opt = 'aspect', unit = 'radians', neighbors = 8))

  day <- (30 * 1:12) - 15

  PET_output <-
    lapply(1:12, function(k) {

      tmax_values <- raster::values(tmax[[k]])
      tmin_values <- raster::values(tmin[[k]])
      slope_values <- raster::values(slope)
      aspect_values <- raster::values(aspect)
      lat_values <- raster::values(lat_rad)

      if (class(forest) == 'RasterStack' | class(forest) == 'RasterLayer') {

        forest_values <- raster::values(forest)

      } else {

        forest_values <- rep(forest, length(tmax_values))

      }

      tmax_values[is.na(tmax_values)] <- 0
      tmin_values[is.na(tmin_values)] <- 0
      slope_values[is.na(slope_values)] <- 0
      lat_values[is.na(lat_values)] <- 0
      aspect_values[is.na(aspect_values)] <- 0
      forest_values[is.na(forest_values)] <- 0

      PET_raster <- alt

      PET_values <-
        EcoHydRology::PET_fromTemp(Jday = rep(day[k], length(tmax_values)),
          Tmax_C = tmax_values,
          Tmin_C = tmin_values,
          lat_radians = lat_values,
          aspect = aspect_values,
          slope = slope_values,
          forest = forest_values)

      raster::values(PET_raster) <- PET_values

      PET_raster * 30000 # accumulated month plus converting to mm

    })

      names(PET_output) <- paste("PET", 1:12, sep = "_")

      raster::stack(PET_output)
}

#' Generates Actual EvapoTranspiration rasters
#'
#' \code{AETFUN} Applies Duncan Golicher's Bucket model to Potential EvapoTranspiration and precipitation rasters in order to get Actual Evapotranspiration estimates for an area.
#'
#' @param PET RasterStack with 12 layers. Total month Potential EvapoTranspiration rasters.
#' @param prec RasterStack with 12 layers. Total month precipitation rasters.
#'
#' @return Returns a RasterLayer with estimates of Actual EvapoTranspiration in milimiters.
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
#' tmax <- FulanusEcoRasters_present$present[[25:36]]/10
#' tmin <- FulanusEcoRasters_present$present[[13:24]]/10
#' prec <- FulanusEcoRasters_present$present[[1:12]]
#'
#' PET <- PETFUN(tmax, tmin, alt)
#'
#' AETFUN(PET, prec)
#'
#' AETFUN(PET, prec)
#'
#' @export
#'

AETFUN <- function(PET, prec) {

  Bucket <- PET[[1]]

  mn <- 1 + 1:359 %/% 30
  rain <- prec[[mn]]/30
  PET_seq <- PET[[mn]]/30
  AET <- PET

  for (n in 1:2) {
    for (i in 1:359) {

      NewAET <- PET_seq[[1]]
      NewBucket <- Bucket
      alpha <- (NewBucket - 60)/90
      evap <- PET_seq[[i]] * alpha * 0.8   #     A fudge factor for stomatal control.
      NewBucket <- NewBucket + rain[[i]] - evap
      NewBucket[NewBucket > 150] <- 150
      NewBucket[NewBucket < 60] <- 60
      Bucket <- NewBucket
      NewAET <- evap * (NewBucket > 60)

      if (n > 1 && (i %% 30) - 15 == 0) {     ## i%%30 will run 1 to 359 and determine position in 30 e.g., 1 is 1 and 61 is 1

        raster::values(AET[[mn[i]]]) <- raster::values(NewAET) * 30

      }
    }
  }

  names(AET) <- paste("AET", 1:12, sep = "_")

  AET

}

#' Wrapper around \code{Solar} function from package \code{EcoHydrology}
#'
#' \code{sradFUN} Gets estimates of solar radiation for an area, based on maximum and minimum temperatures and altitude rasters by applying function \code{Solar} from package \code{EcoHydrology}
#'
#' @param tmax Raster* object. Maximum temperature raster.
#' @param tmin Raster* object. Minimum temperature raster.
#' @param alt Raster* object. Altitude raster.
#'
#' @return Returns a RasterLayer with estimates of Solar in kiloJoules by square meter by day.
#'
#' @examples
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
#' tmax <- FulanusEcoRasters_present$present[[25:36]]/10
#' tmin <- FulanusEcoRasters_present$present[[13:24]]/10
#'
#' srad <- sradFUN(alt = alt, tmax = tmax, tmin = tmin)
#'
#' @export

sradFUN <- function(alt, tmax, tmin) {

  forest = 0 # set this as an argument later

  lat_rad <- alt
  raster::values(lat_rad) <- raster::values(raster::init(tmax, 'y')) * pi/180

  slope <- raster::stack(raster::terrain(alt, opt = 'slope', unit = 'radians', neighbors = 8))
  aspect  <- raster::stack(raster::terrain(alt, opt = 'aspect', unit = 'radians', neighbors = 8))

  day <- (30 * 1:12) - 15

  solar_output <- tmax

  solar_output <-
    lapply(1:12, function(k) {
      tmax_values <- raster::values(tmax[[k]])
      tmin_values <- raster::values(tmin[[k]])
      slope_values <- raster::values(slope)
      aspect_values <- raster::values(aspect)
      lat_values <- raster::values(lat_rad)

      if (class(forest) == 'RasterStack' | class(forest) == 'RasterLayer') {

        forest_values <- raster::values(forest)

      } else {

        forest_values <- rep(forest, length(tmax_values))

      }

      tmax_values[is.na(tmax_values)] <- 0
      tmin_values[is.na(tmin_values)] <- 0
      slope_values[is.na(slope_values)] <- 0
      lat_values[is.na(lat_values)] <- 0
      aspect_values[is.na(aspect_values)] <- 0
      forest_values[is.na(forest_values)] <- 0

      day_values <- rep(day[[k]], length(tmax_values))

      solar_r <- tmax[[1]]

      Es_Solar <- EcoHydRology::Solar(lat = lat_values, Jday = day_values, Tx = tmax_values, Tn = tmin_values, albedo = 0.2, forest = forest_values, aspect = aspect_values, slope = slope_values, printWarn = FALSE, latUnits = "radians")

      raster::values(solar_r) <- Es_Solar * 30

      solar_output[[k]] <- solar_r
    })

  names(solar_output) <- paste("srad", 1:12, sep = "_")

  raster::stack(solar_output)

}
