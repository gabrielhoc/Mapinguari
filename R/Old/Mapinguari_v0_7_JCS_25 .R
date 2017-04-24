###################################################################################################################
###################################################################################################################
###################################################################################################################
########################    Mapinguari0.7.1 - Juan's input    ###############################
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
#ONE FILE: paths_to_master_rasters_cut_and_stack present and future: bioclim, tmax, tmin, precipitation ###########
###################################################################################################################

paths_to_one_raster_cut_and_stack <- function (raster_path_value,
                                                   max_long_value,
                                                   min_long_value,
                                                   max_lat_value,
                                                   min_lat_value,
                                                   crop_raster_name_value,
                                                   out_dir_name_value) {

# required libraries

require('raster')
require('geosphere')
require('maptools')
require('dismo')

# from user

raster_path <- raster_path_value
max_long <- max_long_value
min_long <- min_long_value
max_lat <- max_lat_value
min_lat <- min_lat_value
crop_raster_name  <- crop_raster_name_value
out_path_common_dir <- out_dir_name_value

master_directory <- getwd()

# dir.create

dir.create(out_path_common_dir, showWarnings = F)
setwd(out_path_common_dir)

# extent

ext_species <- extent(min_long, max_long, min_lat, max_lat)

# get list of files determine

raster_file <- stack(raster_path)

#  crop raster

raster_crop <- crop (raster_file, ext_species)
raster_stack <- stack(raster_crop)


# resolution name

if(round(res(raster_stack)[1], 4) == 0.0083) {resolution_name = "30_sec" }
if(round(res(raster_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min" }
if(round(res(raster_stack)[1], 4) == 0.0833) {resolution_name = "5_min" }
if(round(res(raster_stack)[1], 4) == 0.1667) {resolution_name = "10_min" }

# write.raster

writeRaster(raster_stack , filename=paste(crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(raster_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(raster_stack), "\n")
cat("resolution: ", resolution_name, "\n")
cat("file_name_cropped_written: ", paste(crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

# set back

setwd(master_directory)

return(raster_stack)

                                       }

##########################               END OF FUNCTION               ###################


paths_to_master_rasters_cut_and_stack <- function (altitude_30sec_guide_path_value,
                                                 altitude_path_value,
                                                 wind_speed_path_value, 
                                                 solar_radiation_path_value,
                                                 water_vapor_pressure_path_value, 
                                                 sunlight_hours_path_value, 
                                                 slope_aspect_path_value,
                                                 timezone_path_value,

                                                 present_path_to_bioclim_value,
                                                 future_2050_path_to_26_bioclim_value,
                                                 future_2070_path_to_26_bioclim_value,
                                                 future_2050_path_to_45_bioclim_value,
                                                 future_2070_path_to_45_bioclim_value,
                                                 future_2050_path_to_85_bioclim_value,
                                                 future_2070_path_to_85_bioclim_value,

                                                 present_path_to_prec_value,
                                                 future_2050_path_to_26_prec_value,
                                                 future_2070_path_to_26_prec_value,
                                                 future_2050_path_to_45_prec_value,
                                                 future_2070_path_to_45_prec_value,
                                                 future_2050_path_to_85_prec_value,
                                                 future_2070_path_to_85_prec_value,

                                                 present_path_to_tmin_value,
                                                 future_2050_path_to_26_tmin_value,
                                                 future_2070_path_to_26_tmin_value,
                                                 future_2050_path_to_45_tmin_value,
                                                 future_2070_path_to_45_tmin_value,
                                                 future_2050_path_to_85_tmin_value,
                                                 future_2070_path_to_85_tmin_value,

                                                 present_path_to_tmax_value,
                                                 future_2050_path_to_26_tmax_value,
                                                 future_2070_path_to_26_tmax_value,
                                                 future_2050_path_to_45_tmax_value,
                                                 future_2070_path_to_45_tmax_value,
                                                 future_2050_path_to_85_tmax_value,
                                                 future_2070_path_to_85_tmax_value,

                                                   max_long_value,
                                                   min_long_value,
                                                   max_lat_value,
                                                   min_lat_value,
                                                   crop_raster_name_value,
                                                   out_dir_name_value) {

# required libraries

require('raster')
require('geosphere')
require('maptools')
require('dismo')

# from user

altitude_guide_path <- altitude_30sec_guide_path_value

altitude_path <- altitude_path_value
wind_speed_path <-  wind_speed_path_value 
solar_radiation_path <- solar_radiation_path_value
water_vapor_pressure_path <-  water_vapor_pressure_path_value 
sunlight_hours_path <-  sunlight_hours_path_value 
slope_aspect_path <- slope_aspect_path_value
timezone_path <- timezone_path_value

present_path_to_bioclim <- present_path_to_bioclim_value
future_2050_path_to_26_bioclim <- future_2050_path_to_26_bioclim_value
future_2070_path_to_26_bioclim <- future_2070_path_to_26_bioclim_value
future_2050_path_to_45_bioclim <- future_2050_path_to_45_bioclim_value
future_2070_path_to_45_bioclim <- future_2070_path_to_45_bioclim_value
future_2050_path_to_85_bioclim <- future_2050_path_to_85_bioclim_value
future_2070_path_to_85_bioclim <- future_2070_path_to_85_bioclim_value

present_path_to_prec <- present_path_to_prec_value
future_2050_path_to_26_prec <- future_2050_path_to_26_prec_value
future_2070_path_to_26_prec <- future_2070_path_to_26_prec_value
future_2050_path_to_45_prec <- future_2050_path_to_45_prec_value
future_2070_path_to_45_prec <- future_2070_path_to_45_prec_value
future_2050_path_to_85_prec <- future_2050_path_to_85_prec_value
future_2070_path_to_85_prec <- future_2070_path_to_85_prec_value

present_path_to_tmin <- present_path_to_tmin_value
future_2050_path_to_26_tmin <- future_2050_path_to_26_tmin_value
future_2070_path_to_26_tmin <- future_2070_path_to_26_tmin_value
future_2050_path_to_45_tmin <- future_2050_path_to_45_tmin_value
future_2070_path_to_45_tmin <- future_2070_path_to_45_tmin_value
future_2050_path_to_85_tmin <- future_2050_path_to_85_tmin_value
future_2070_path_to_85_tmin <- future_2070_path_to_85_tmin_value

present_path_to_tmax <- present_path_to_tmax_value
future_2050_path_to_26_tmax <- future_2050_path_to_26_tmax_value
future_2070_path_to_26_tmax <- future_2070_path_to_26_tmax_value
future_2050_path_to_45_tmax <- future_2050_path_to_45_tmax_value
future_2070_path_to_45_tmax <- future_2070_path_to_45_tmax_value
future_2050_path_to_85_tmax <- future_2050_path_to_85_tmax_value
future_2070_path_to_85_tmax <- future_2070_path_to_85_tmax_value

max_long <- max_long_value
min_long <- min_long_value
max_lat <- max_lat_value
min_lat <- min_lat_value
crop_raster_name  <- crop_raster_name_value
out_path_common_dir <- out_dir_name_value

master_directory <- getwd()

# dir.create

dir.create(out_path_common_dir, showWarnings = F)
setwd(out_path_common_dir)

# extent

ext_species <- extent(min_long, max_long, min_lat, max_lat)

################################     PROCESS RASTERS     #################################

################################      Altitude Guide: Alt-30sec      #################################

alt_file <- list.files(path = altitude_guide_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(alt_file) == 0) {alt_file <- list.files(path = altitude_guide_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(alt_file) == 0) {alt_file <- list.files(path = altitude_guide_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(alt_file)
cat("\n")

# get list of files determine

alt_raster <- stack(alt_file)
alt_crop <- crop (alt_raster, ext_species)
alt_stack <- stack(alt_crop)

# resolution name

if(round(res(alt_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(alt_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(alt_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(alt_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(alt_stack , filename=paste("altguide_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(alt_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(alt_stack), "\n")
cat("grid units: meters \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(alt_stack), "\n")
cat("minimum values: ", minValue(alt_stack), "\n")
cat("file_name_cropped_written: ", paste("altguide_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("------------------------------------------------\n")


################################      Altitude: Alt      #################################

alt_file <- list.files(path = altitude_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(alt_file) == 0) {alt_file <- list.files(path = altitude_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(alt_file) == 0) {alt_file <- list.files(path = altitude_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(alt_file)
cat("\n")

# get list of files determine

alt_raster <- stack(alt_file)
alt_crop <- crop (alt_raster, ext_species)
alt_stack <- stack(alt_crop)

# resolution name

if(round(res(alt_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(alt_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(alt_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(alt_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(alt_stack , filename=paste("alt_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(alt_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(alt_stack), "\n")
cat("grid units: meters \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(alt_stack), "\n")
cat("minimum values: ", minValue(alt_stack), "\n")
cat("file_name_cropped_written: ", paste("alt_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################     Monthly wind speed: m s-1      #################################

wind_file <- list.files(path = wind_speed_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(wind_file) == 0) {wind_file <- list.files(path = wind_speed_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(wind_file) == 0) {wind_file <- list.files(path = wind_speed_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(wind_file)
cat("\n")

# get list of files determine

wind_raster <- stack(wind_file)
wind_crop <- crop (wind_raster, ext_species)
wind_stack <- stack(wind_crop)
names(wind_stack) <- c("wind_01", "wind_02", "wind_03", "wind_04", "wind_05", "wind_06", "wind_07", "wind_08", "wind_09", "wind_10", "wind_11", "wind_12")
wind_stack <- wind_stack * 1000

# resolution name

if(round(res(wind_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(wind_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(wind_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(wind_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(wind_stack , filename=paste("wind_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(wind_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(wind_stack), "\n")
cat("grid units: m s-1 * 1000 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(wind_stack), "\n")
cat("minimum values: ", minValue(wind_stack), "\n")
cat("file_name_cropped_written: ", paste("wind_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################      Monthly solar radiation: kJ m-2 day-1      #################################

solar_file <- list.files(path = solar_radiation_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(solar_file) == 0) {solar_file <- list.files(path = solar_radiation_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(solar_file) == 0) {solar_file <- list.files(path = solar_radiation_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(solar_file)
cat("\n")

# get list of files determine

solar_raster <- stack(solar_file)
solar_crop <- crop (solar_raster, ext_species)
solar_stack <- stack(solar_crop)
names(solar_stack) <- c("solrad_01", "solrad_02", "solrad_03", "solrad_04", "solrad_05", "solrad_06", "solrad_07", "solrad_08", "solrad_09", "solrad_10", "solrad_11", "solrad_12")

# resolution name

if(round(res(solar_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(solar_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(solar_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(solar_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(solar_stack , filename=paste("solar_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(solar_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(solar_stack), "\n")
cat("grid units: kJ m-2 day-1 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(solar_stack), "\n")
cat("minimum values: ", minValue(solar_stack), "\n")
cat("file_name_cropped_written: ", paste("solar_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################      Monthly water vapor pressure: kPa      #################################

vapor_file <- list.files(path = water_vapor_pressure_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(vapor_file) == 0) {vapor_file <- list.files(path = water_vapor_pressure_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(vapor_file) == 0) {vapor_file <- list.files(path = water_vapor_pressure_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(vapor_file)
cat("\n")

# get list of files determine

vapor_raster <- stack(vapor_file)
vapor_crop <- crop (vapor_raster, ext_species)
vapor_stack <- stack(vapor_crop)
names(vapor_stack) <- c("vapor_01", "vapor_02", "vapor_03", "vapor_04", "vapor_05", "vapor_06", "vapor_07", "vapor_08", "vapor_09", "vapor_10", "vapor_11", "vapor_12")
vapor_stack <- vapor_stack * 1000

# resolution name

if(round(res(vapor_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(vapor_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(vapor_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(vapor_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(vapor_stack , filename=paste("vapor_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(vapor_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(vapor_stack), "\n")
cat("grid units: kPa * 1000 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(vapor_stack), "\n")
cat("minimum values: ", minValue(vapor_stack), "\n")
cat("file_name_cropped_written: ", paste("vapor_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################      Monthly sunlight hours: hours      #################################

sunlight_file <- list.files(path = sunlight_hours_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(sunlight_file) == 0) {sunlight_file <- list.files(path = sunlight_hours_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(sunlight_file) == 0) {sunlight_file <- list.files(path = sunlight_hours_path, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(sunlight_file)
cat("\n")

# get list of files determine

sunlight_raster <- stack(sunlight_file)
sunlight_crop <- crop (sunlight_raster, ext_species)
sunlight_stack_raw <- stack(sunlight_crop)
sunlight_stack <- subset(sunlight_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(sunlight_stack) <- c("sunlight_01", "sunlight_02", "sunlight_03", "sunlight_04", "sunlight_05", "sunlight_06", "sunlight_07", "sunlight_08", "sunlight_09", "sunlight_10", "sunlight_11", "sunlight_12")

# resolution name

if(round(res(sunlight_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(sunlight_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(sunlight_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(sunlight_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(sunlight_stack , filename=paste("sunlight_x_100", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(sunlight_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(sunlight_stack), "\n")
cat("grid units: hours * 100 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(sunlight_stack), "\n")
cat("minimum values: ", minValue(sunlight_stack), "\n")
cat("file_name_cropped_written: ", paste("sunlight_x_100", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################      slope_aspect_path: hours      #################################

slope_aspect_file <- list.files(path = slope_aspect_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(slope_aspect_file) == 0) {slope_aspect_file <- list.files(path = slope_aspect_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(slope_aspect_file) == 0) {slope_aspect_file <- list.files(path = slope_aspect_path, pattern="*.gri$",full.names=T, ignore.case=T)}

slope_aspect_file

cat("\n")
cat("------------- Raster file list ------------\n")
print(slope_aspect_file)
cat("\n")

# get list of files determine

slope_aspect_raster <- stack(slope_aspect_file)
slope_aspect_crop <- crop (slope_aspect_raster, ext_species)
slope_aspect_stack <- stack(slope_aspect_crop)

# resolution name

if(round(res(slope_aspect_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(slope_aspect_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(slope_aspect_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(slope_aspect_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(slope_aspect_stack , filename=paste("slope_aspect_x_1000", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(slope_aspect_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(slope_aspect_stack), "\n")
cat("grid units: radians * 1000 radians * 1000 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(slope_aspect_stack), "\n")
cat("minimum values: ", minValue(slope_aspect_stack), "\n")
cat("file_name_cropped_written: ", paste("slope_aspect_x_1000", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################      timezone:  difference hours from GTM     #################################

timezone_file <- list.files(path = timezone_path, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(timezone_file) == 0) {timezone_file <- list.files(path = timezone_path, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(timezone_file) == 0) {timezone_file <- list.files(path = timezone_path, pattern="*.gri$",full.names=T, ignore.case=T)}

timezone_file

cat("\n")
cat("------------- Raster file list ------------\n")
print(timezone_file)
cat("\n")

# get list of files determine

timezone_raster <- stack(timezone_file)
timezone_crop <- crop (timezone_raster, ext_species)
timezone_stack <- stack(timezone_crop)

# resolution name

if(round(res(timezone_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(timezone_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(timezone_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(timezone_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(timezone_stack , filename=paste("timezone_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(timezone_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(timezone_stack), "\n")
cat("grid units: hours difference GTM \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(timezone_stack), "\n")
cat("minimum values: ", minValue(timezone_stack), "\n")
cat("file_name_cropped_written: ", paste("timezone_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")


################################        bioclim_present        #################################

bioclim_present_file <- list.files(path = present_path_to_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_present_file) == 0) {bioclim_present_file <- list.files(path = present_path_to_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_present_file) == 0) {bioclim_present_file <- list.files(path = present_path_to_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_present_file)
cat("\n")

# get list of files determine

bioclim_present_raster <- stack(bioclim_present_file)
bioclim_present_crop <- crop (bioclim_present_raster, ext_species)
bioclim_present_stack_raw <- stack(bioclim_present_crop)
bioclim_present_stack <- subset(bioclim_present_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))

# resolution name

if(round(res(bioclim_present_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_present_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_present_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_present_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_present_stack , filename=paste("bioclim_present_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_present_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_present_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_present_stack), "\n")
cat("minimum values: ", minValue(bioclim_present_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_present_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2050_26_rpc        #################################

bioclim_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2050_26_rpc_file) == 0) {bioclim_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2050_26_rpc_file) == 0) {bioclim_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2050_26_rpc_file)
cat("\n")

# get list of files determine

bioclim_2050_26_rpc_raster <- stack(bioclim_2050_26_rpc_file)
bioclim_2050_26_rpc_crop <- crop (bioclim_2050_26_rpc_raster, ext_species)
bioclim_2050_26_rpc_stack_raw <- stack(bioclim_2050_26_rpc_crop)
bioclim_2050_26_rpc_stack <- subset(bioclim_2050_26_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2050_26_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2050_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2050_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2050_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2050_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2050_26_rpc_stack , filename=paste("bioclim_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2050_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2050_26_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2050_26_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2050_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2070_26_rpc        #################################

bioclim_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2070_26_rpc_file) == 0) {bioclim_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2070_26_rpc_file) == 0) {bioclim_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2070_26_rpc_file)
cat("\n")

# get list of files determine

bioclim_2070_26_rpc_raster <- stack(bioclim_2070_26_rpc_file)
bioclim_2070_26_rpc_crop <- crop (bioclim_2070_26_rpc_raster, ext_species)
bioclim_2070_26_rpc_stack_raw <- stack(bioclim_2070_26_rpc_crop)
bioclim_2070_26_rpc_stack <- subset(bioclim_2070_26_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2070_26_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2070_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2070_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2070_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2070_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2070_26_rpc_stack , filename=paste("bioclim_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2070_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2070_26_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2070_26_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2070_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2050_45_rpc        #################################

bioclim_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2050_45_rpc_file) == 0) {bioclim_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2050_45_rpc_file) == 0) {bioclim_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2050_45_rpc_file)
cat("\n")

# get list of files determine

bioclim_2050_45_rpc_raster <- stack(bioclim_2050_45_rpc_file)
bioclim_2050_45_rpc_crop <- crop (bioclim_2050_45_rpc_raster, ext_species)
bioclim_2050_45_rpc_stack_raw <- stack(bioclim_2050_45_rpc_crop)
bioclim_2050_45_rpc_stack <- subset(bioclim_2050_45_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2050_45_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2050_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2050_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2050_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2050_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2050_45_rpc_stack , filename=paste("bioclim_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2050_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2050_45_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2050_45_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2050_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2070_45_rpc        #################################

bioclim_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2070_45_rpc_file) == 0) {bioclim_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2070_45_rpc_file) == 0) {bioclim_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2070_45_rpc_file)
cat("\n")

# get list of files determine

bioclim_2070_45_rpc_raster <- stack(bioclim_2070_45_rpc_file)
bioclim_2070_45_rpc_crop <- crop (bioclim_2070_45_rpc_raster, ext_species)
bioclim_2070_45_rpc_stack_raw <- stack(bioclim_2070_45_rpc_crop)
bioclim_2070_45_rpc_stack <- subset(bioclim_2070_45_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2070_45_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2070_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2070_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2070_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2070_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2070_45_rpc_stack , filename=paste("bioclim_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2070_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2070_45_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2070_45_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2070_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2050_85_rpc        #################################

bioclim_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2050_85_rpc_file) == 0) {bioclim_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2050_85_rpc_file) == 0) {bioclim_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2050_85_rpc_file)
cat("\n")

# get list of files determine

bioclim_2050_85_rpc_raster <- stack(bioclim_2050_85_rpc_file)
bioclim_2050_85_rpc_crop <- crop (bioclim_2050_85_rpc_raster, ext_species)
bioclim_2050_85_rpc_stack_raw <- stack(bioclim_2050_85_rpc_crop)
bioclim_2050_85_rpc_stack <- subset(bioclim_2050_85_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2050_85_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2050_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2050_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2050_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2050_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2050_85_rpc_stack , filename=paste("bioclim_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2050_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2050_85_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2050_85_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2050_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        bioclim_2070_85_rpc        #################################

bioclim_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_bioclim, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(bioclim_2070_85_rpc_file) == 0) {bioclim_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_bioclim, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(bioclim_2070_85_rpc_file) == 0) {bioclim_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_bioclim, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(bioclim_2070_85_rpc_file)
cat("\n")

# get list of files determine

bioclim_2070_85_rpc_raster <- stack(bioclim_2070_85_rpc_file)
bioclim_2070_85_rpc_crop <- crop (bioclim_2070_85_rpc_raster, ext_species)
bioclim_2070_85_rpc_stack_raw <- stack(bioclim_2070_85_rpc_crop)
bioclim_2070_85_rpc_stack <- subset(bioclim_2070_85_rpc_stack_raw, c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11))
names(bioclim_2070_85_rpc_stack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

# resolution name

if(round(res(bioclim_2070_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(bioclim_2070_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(bioclim_2070_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(bioclim_2070_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(bioclim_2070_85_rpc_stack , filename=paste("bioclim_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(bioclim_2070_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(bioclim_2070_85_rpc_stack), "\n")
cat("grid units: check worldclim \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(bioclim_2070_85_rpc_stack), "\n")
cat("minimum values: ", minValue(bioclim_2070_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("bioclim_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_present        #################################

prec_present_file <- list.files(path = present_path_to_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_present_file) == 0) {prec_present_file <- list.files(path = present_path_to_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_present_file) == 0) {prec_present_file <- list.files(path = present_path_to_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_present_file)
cat("\n")

# get list of files determine

prec_present_raster <- stack(prec_present_file)
prec_present_crop <- crop (prec_present_raster, ext_species)
prec_present_stack_raw <- stack(prec_present_crop)
prec_present_stack <- subset(prec_present_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))

# resolution name

if(round(res(prec_present_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_present_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_present_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_present_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_present_stack , filename=paste("prec_present_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_present_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_present_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_present_stack), "\n")
cat("minimum values: ", minValue(prec_present_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_present_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2050_26_rpc        #################################

prec_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2050_26_rpc_file) == 0) {prec_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2050_26_rpc_file) == 0) {prec_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2050_26_rpc_file)
cat("\n")

# get list of files determine

prec_2050_26_rpc_raster <- stack(prec_2050_26_rpc_file)
prec_2050_26_rpc_crop <- crop (prec_2050_26_rpc_raster, ext_species)
prec_2050_26_rpc_stack_raw <- stack(prec_2050_26_rpc_crop)
prec_2050_26_rpc_stack <- subset(prec_2050_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2050_26_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2050_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2050_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2050_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2050_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2050_26_rpc_stack , filename=paste("prec_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2050_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2050_26_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2050_26_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2050_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2050_45_rpc        #################################

prec_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2050_45_rpc_file) == 0) {prec_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2050_45_rpc_file) == 0) {prec_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2050_45_rpc_file)
cat("\n")

# get list of files determine

prec_2050_45_rpc_raster <- stack(prec_2050_45_rpc_file)
prec_2050_45_rpc_crop <- crop (prec_2050_45_rpc_raster, ext_species)
prec_2050_45_rpc_stack_raw <- stack(prec_2050_45_rpc_crop)
prec_2050_45_rpc_stack <- subset(prec_2050_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2050_45_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2050_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2050_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2050_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2050_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2050_45_rpc_stack , filename=paste("prec_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2050_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2050_45_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2050_45_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2050_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2050_85_rpc        #################################

prec_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2050_85_rpc_file) == 0) {prec_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2050_85_rpc_file) == 0) {prec_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2050_85_rpc_file)
cat("\n")

# get list of files determine

prec_2050_85_rpc_raster <- stack(prec_2050_85_rpc_file)
prec_2050_85_rpc_crop <- crop (prec_2050_85_rpc_raster, ext_species)
prec_2050_85_rpc_stack_raw <- stack(prec_2050_85_rpc_crop)
prec_2050_85_rpc_stack <- subset(prec_2050_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2050_85_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2050_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2050_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2050_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2050_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2050_85_rpc_stack , filename=paste("prec_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2050_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2050_85_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2050_85_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2050_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2070_26_rpc        #################################

prec_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2070_26_rpc_file) == 0) {prec_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2070_26_rpc_file) == 0) {prec_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2070_26_rpc_file)
cat("\n")

# get list of files determine

prec_2070_26_rpc_raster <- stack(prec_2070_26_rpc_file)
prec_2070_26_rpc_crop <- crop (prec_2070_26_rpc_raster, ext_species)
prec_2070_26_rpc_stack_raw <- stack(prec_2070_26_rpc_crop)
prec_2070_26_rpc_stack <- subset(prec_2070_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2070_26_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2070_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2070_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2070_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2070_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2070_26_rpc_stack , filename=paste("prec_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2070_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2070_26_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2070_26_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2070_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2070_45_rpc        #################################

prec_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2070_45_rpc_file) == 0) {prec_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2070_45_rpc_file) == 0) {prec_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2070_45_rpc_file)
cat("\n")

# get list of files determine

prec_2070_45_rpc_raster <- stack(prec_2070_45_rpc_file)
prec_2070_45_rpc_crop <- crop (prec_2070_45_rpc_raster, ext_species)
prec_2070_45_rpc_stack_raw <- stack(prec_2070_45_rpc_crop)
prec_2070_45_rpc_stack <- subset(prec_2070_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2070_45_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2070_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2070_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2070_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2070_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2070_45_rpc_stack , filename=paste("prec_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2070_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2070_45_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2070_45_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2070_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        prec_2070_85_rpc        #################################

prec_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_prec, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(prec_2070_85_rpc_file) == 0) {prec_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_prec, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(prec_2070_85_rpc_file) == 0) {prec_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_prec, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(prec_2070_85_rpc_file)
cat("\n")

# get list of files determine

prec_2070_85_rpc_raster <- stack(prec_2070_85_rpc_file)
prec_2070_85_rpc_crop <- crop (prec_2070_85_rpc_raster, ext_species)
prec_2070_85_rpc_stack_raw <- stack(prec_2070_85_rpc_crop)
prec_2070_85_rpc_stack <- subset(prec_2070_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(prec_2070_85_rpc_stack) <- c("prec1", "prec2", "prec3", "prec4", "prec5", "prec6", "prec7", "prec8", "prec9", "prec10", "prec11", "prec12")

# resolution name

if(round(res(prec_2070_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(prec_2070_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(prec_2070_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(prec_2070_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(prec_2070_85_rpc_stack , filename=paste("prec_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(prec_2070_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_2070_85_rpc_stack), "\n")
cat("grid units: mm \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_2070_85_rpc_stack), "\n")
cat("minimum values: ", minValue(prec_2070_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("prec_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_present        #################################

tmin_present_file <- list.files(path = present_path_to_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_present_file) == 0) {tmin_present_file <- list.files(path = present_path_to_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_present_file) == 0) {tmin_present_file <- list.files(path = present_path_to_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_present_file)
cat("\n")

# get list of files determine

tmin_present_raster <- stack(tmin_present_file)
tmin_present_crop <- crop (tmin_present_raster, ext_species)
tmin_present_stack_raw <- stack(tmin_present_crop)
tmin_present_stack <- subset(tmin_present_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))

# resolution name

if(round(res(tmin_present_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_present_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_present_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_present_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_present_stack , filename=paste("tmin_present_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_present_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_present_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_present_stack), "\n")
cat("minimum values: ", minValue(tmin_present_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_present_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2050_26_rpc        #################################

tmin_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2050_26_rpc_file) == 0) {tmin_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2050_26_rpc_file) == 0) {tmin_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2050_26_rpc_file)
cat("\n")

# get list of files determine

tmin_2050_26_rpc_raster <- stack(tmin_2050_26_rpc_file)
tmin_2050_26_rpc_crop <- crop (tmin_2050_26_rpc_raster, ext_species)
tmin_2050_26_rpc_stack_raw <- stack(tmin_2050_26_rpc_crop)
tmin_2050_26_rpc_stack <- subset(tmin_2050_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2050_26_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2050_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2050_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2050_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2050_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2050_26_rpc_stack , filename=paste("tmin_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2050_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2050_26_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2050_26_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2050_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2050_45_rpc        #################################

tmin_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2050_45_rpc_file) == 0) {tmin_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2050_45_rpc_file) == 0) {tmin_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2050_45_rpc_file)
cat("\n")

# get list of files determine

tmin_2050_45_rpc_raster <- stack(tmin_2050_45_rpc_file)
tmin_2050_45_rpc_crop <- crop (tmin_2050_45_rpc_raster, ext_species)
tmin_2050_45_rpc_stack_raw <- stack(tmin_2050_45_rpc_crop)
tmin_2050_45_rpc_stack <- subset(tmin_2050_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2050_45_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2050_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2050_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2050_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2050_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2050_45_rpc_stack , filename=paste("tmin_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2050_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2050_45_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2050_45_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2050_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2050_85_rpc        #################################

tmin_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2050_85_rpc_file) == 0) {tmin_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2050_85_rpc_file) == 0) {tmin_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2050_85_rpc_file)
cat("\n")

# get list of files determine

tmin_2050_85_rpc_raster <- stack(tmin_2050_85_rpc_file)
tmin_2050_85_rpc_crop <- crop (tmin_2050_85_rpc_raster, ext_species)
tmin_2050_85_rpc_stack_raw <- stack(tmin_2050_85_rpc_crop)
tmin_2050_85_rpc_stack <- subset(tmin_2050_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2050_85_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2050_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2050_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2050_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2050_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2050_85_rpc_stack , filename=paste("tmin_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2050_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2050_85_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2050_85_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2050_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2070_26_rpc        #################################

tmin_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2070_26_rpc_file) == 0) {tmin_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2070_26_rpc_file) == 0) {tmin_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2070_26_rpc_file)
cat("\n")

# get list of files determine

tmin_2070_26_rpc_raster <- stack(tmin_2070_26_rpc_file)
tmin_2070_26_rpc_crop <- crop (tmin_2070_26_rpc_raster, ext_species)
tmin_2070_26_rpc_stack_raw <- stack(tmin_2070_26_rpc_crop)
tmin_2070_26_rpc_stack <- subset(tmin_2070_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2070_26_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2070_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2070_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2070_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2070_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2070_26_rpc_stack , filename=paste("tmin_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2070_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2070_26_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2070_26_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2070_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2070_45_rpc        #################################

tmin_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2070_45_rpc_file) == 0) {tmin_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2070_45_rpc_file) == 0) {tmin_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2070_45_rpc_file)
cat("\n")

# get list of files determine

tmin_2070_45_rpc_raster <- stack(tmin_2070_45_rpc_file)
tmin_2070_45_rpc_crop <- crop (tmin_2070_45_rpc_raster, ext_species)
tmin_2070_45_rpc_stack_raw <- stack(tmin_2070_45_rpc_crop)
tmin_2070_45_rpc_stack <- subset(tmin_2070_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2070_45_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2070_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2070_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2070_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2070_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2070_45_rpc_stack , filename=paste("tmin_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2070_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2070_45_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2070_45_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2070_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmin_2070_85_rpc        #################################

tmin_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmin, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmin_2070_85_rpc_file) == 0) {tmin_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmin, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmin_2070_85_rpc_file) == 0) {tmin_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmin, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmin_2070_85_rpc_file)
cat("\n")


# get list of files determine

tmin_2070_85_rpc_raster <- stack(tmin_2070_85_rpc_file)
tmin_2070_85_rpc_crop <- crop (tmin_2070_85_rpc_raster, ext_species)
tmin_2070_85_rpc_stack_raw <- stack(tmin_2070_85_rpc_crop)
tmin_2070_85_rpc_stack <- subset(tmin_2070_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmin_2070_85_rpc_stack) <- c("tmin1", "tmin2", "tmin3", "tmin4", "tmin5", "tmin6", "tmin7", "tmin8", "tmin9", "tmin10", "tmin11", "tmin12")

# resolution name

if(round(res(tmin_2070_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmin_2070_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmin_2070_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmin_2070_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmin_2070_85_rpc_stack , filename=paste("tmin_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmin_2070_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmin_2070_85_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmin_2070_85_rpc_stack), "\n")
cat("minimum values: ", minValue(tmin_2070_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmin_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_present        #################################

tmax_present_file <- list.files(path = present_path_to_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_present_file) == 0) {tmax_present_file <- list.files(path = present_path_to_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_present_file) == 0) {tmax_present_file <- list.files(path = present_path_to_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_present_file)
cat("\n")

# get list of files determine

tmax_present_raster <- stack(tmax_present_file)
tmax_present_crop <- crop (tmax_present_raster, ext_species)
tmax_present_stack_raw <- stack(tmax_present_crop)
tmax_present_stack <- subset(tmax_present_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))

# resolution name

if(round(res(tmax_present_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_present_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_present_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_present_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_present_stack , filename=paste("tmax_present_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_present_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_present_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_present_stack), "\n")
cat("minimum values: ", minValue(tmax_present_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_present_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2050_26_rpc        #################################

tmax_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2050_26_rpc_file) == 0) {tmax_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2050_26_rpc_file) == 0) {tmax_2050_26_rpc_file <- list.files(path = future_2050_path_to_26_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2050_26_rpc_file)
cat("\n")

# get list of files determine

tmax_2050_26_rpc_raster <- stack(tmax_2050_26_rpc_file)
tmax_2050_26_rpc_crop <- crop (tmax_2050_26_rpc_raster, ext_species)
tmax_2050_26_rpc_stack_raw <- stack(tmax_2050_26_rpc_crop)
tmax_2050_26_rpc_stack <- subset(tmax_2050_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2050_26_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2050_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2050_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2050_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2050_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2050_26_rpc_stack , filename=paste("tmax_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2050_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2050_26_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2050_26_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2050_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2050_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2050_45_rpc        #################################

tmax_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2050_45_rpc_file) == 0) {tmax_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2050_45_rpc_file) == 0) {tmax_2050_45_rpc_file <- list.files(path = future_2050_path_to_45_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2050_45_rpc_file)
cat("\n")

# get list of files determine

tmax_2050_45_rpc_raster <- stack(tmax_2050_45_rpc_file)
tmax_2050_45_rpc_crop <- crop (tmax_2050_45_rpc_raster, ext_species)
tmax_2050_45_rpc_stack_raw <- stack(tmax_2050_45_rpc_crop)
tmax_2050_45_rpc_stack <- subset(tmax_2050_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2050_45_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2050_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2050_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2050_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2050_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2050_45_rpc_stack , filename=paste("tmax_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2050_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2050_45_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2050_45_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2050_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2050_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2050_85_rpc        #################################

tmax_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2050_85_rpc_file) == 0) {tmax_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2050_85_rpc_file) == 0) {tmax_2050_85_rpc_file <- list.files(path = future_2050_path_to_85_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2050_85_rpc_file)
cat("\n")

# get list of files determine

tmax_2050_85_rpc_raster <- stack(tmax_2050_85_rpc_file)
tmax_2050_85_rpc_crop <- crop (tmax_2050_85_rpc_raster, ext_species)
tmax_2050_85_rpc_stack_raw <- stack(tmax_2050_85_rpc_crop)
tmax_2050_85_rpc_stack <- subset(tmax_2050_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2050_85_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2050_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2050_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2050_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2050_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2050_85_rpc_stack , filename=paste("tmax_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2050_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2050_85_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2050_85_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2050_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2050_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2070_26_rpc        #################################

tmax_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2070_26_rpc_file) == 0) {tmax_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2070_26_rpc_file) == 0) {tmax_2070_26_rpc_file <- list.files(path = future_2070_path_to_26_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2070_26_rpc_file)
cat("\n")

# get list of files determine

tmax_2070_26_rpc_raster <- stack(tmax_2070_26_rpc_file)
tmax_2070_26_rpc_crop <- crop (tmax_2070_26_rpc_raster, ext_species)
tmax_2070_26_rpc_stack_raw <- stack(tmax_2070_26_rpc_crop)
tmax_2070_26_rpc_stack <- subset(tmax_2070_26_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2070_26_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2070_26_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2070_26_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2070_26_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2070_26_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2070_26_rpc_stack , filename=paste("tmax_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2070_26_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2070_26_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2070_26_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2070_26_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2070_26_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2070_45_rpc        #################################

tmax_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2070_45_rpc_file) == 0) {tmax_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2070_45_rpc_file) == 0) {tmax_2070_45_rpc_file <- list.files(path = future_2070_path_to_45_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2070_45_rpc_file)
cat("\n")

# get list of files determine

tmax_2070_45_rpc_raster <- stack(tmax_2070_45_rpc_file)
tmax_2070_45_rpc_crop <- crop (tmax_2070_45_rpc_raster, ext_species)
tmax_2070_45_rpc_stack_raw <- stack(tmax_2070_45_rpc_crop)
tmax_2070_45_rpc_stack <- subset(tmax_2070_45_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2070_45_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2070_45_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2070_45_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2070_45_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2070_45_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2070_45_rpc_stack , filename=paste("tmax_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2070_45_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2070_45_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2070_45_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2070_45_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2070_45_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

################################        tmax_2070_85_rpc        #################################

tmax_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmax, pattern="*.bil$",full.names=T, ignore.case=T)
if(length(tmax_2070_85_rpc_file) == 0) {tmax_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmax, pattern="*.tif$",full.names=T, ignore.case=T)}
if(length(tmax_2070_85_rpc_file) == 0) {tmax_2070_85_rpc_file <- list.files(path = future_2070_path_to_85_tmax, pattern="*.gri$",full.names=T, ignore.case=T)}

cat("\n")
cat("------------- Raster file list ------------\n")
print(tmax_2070_85_rpc_file)
cat("\n")

# get list of files determine

tmax_2070_85_rpc_raster <- stack(tmax_2070_85_rpc_file)
tmax_2070_85_rpc_crop <- crop (tmax_2070_85_rpc_raster, ext_species)
tmax_2070_85_rpc_stack_raw <- stack(tmax_2070_85_rpc_crop)
tmax_2070_85_rpc_stack <- subset(tmax_2070_85_rpc_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))
names(tmax_2070_85_rpc_stack) <- c("tmax1", "tmax2", "tmax3", "tmax4", "tmax5", "tmax6", "tmax7", "tmax8", "tmax9", "tmax10", "tmax11", "tmax12")

# resolution name

if(round(res(tmax_2070_85_rpc_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(tmax_2070_85_rpc_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(tmax_2070_85_rpc_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(tmax_2070_85_rpc_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(tmax_2070_85_rpc_stack , filename=paste("tmax_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(tmax_2070_85_rpc_stack))

cat("\n")
cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(tmax_2070_85_rpc_stack), "\n")
cat("grid units: C * 10 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(tmax_2070_85_rpc_stack), "\n")
cat("minimum values: ", minValue(tmax_2070_85_rpc_stack), "\n")
cat("file_name_cropped_written: ", paste("tmax_2070_85_rpc_", crop_raster_name, "_res_", resolution_name, sep=""), "\n")
cat("max_longitude: ", max_long, "\n","min_longitude: ", min_long, "\n", "max_latitude: ", max_lat, "\n","min_latitude: ",  min_lat, "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

##########################################################################################

setwd(master_directory)

                                       }

##########################               END OF FUNCTION               ###################


###################################################################################################################
#create logical for winter by sunlight and precipitation ##########################################################
###################################################################################################################

create_logical_winter_summer_raster_stacks <- function (master_stack_dir_value,
                                                   precipitation_threshold_value = c('mean', 'quantile_3'),
                                                   model_value,
                                                   out_dir_name_value) {

# required libraries

require('raster')

# from user

rasterpath <- master_stack_dir_value
model <- model_value
threshold <- precipitation_threshold_value
out_path_common_dir <- out_dir_name_value

master_directory <- getwd()

# get list of files

sunlight_file <- list.files(path = rasterpath, pattern="^sunlight_.*.gri$",full.names=T, ignore.case=T)

if(model == 'present') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_present_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_26_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_45_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_85_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_26_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_45_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_85_rpc') {
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }


# dir.create

dir.create(out_path_common_dir, showWarnings = F)
setwd(out_path_common_dir)

# read rasters

sunlight_stack <- stack(sunlight_file)/100 # needs to be divided by 100
monthly_prec <- stack(prec_file) 

# raster extent

raster_extend <- extent(sunlight_stack)

# resolution name

if(round(res(sunlight_stack)[1], 4) == 0.0083) {resolution_name = "30_sec" }
if(round(res(sunlight_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min" }
if(round(res(sunlight_stack)[1], 4) == 0.0833) {resolution_name = "5_min" }
if(round(res(sunlight_stack)[1], 4) == 0.1667) {resolution_name = "10_min" }

#################             calculate by sunlight

#### for stack 12 bands for sunlight

sunlight_seq_year_list <-list()

for (j in 1:12) {
  sun_bin<-as.matrix(sunlight_stack[[j]])
  (sun_bin[sun_bin < - 998] <- NA) #assign -999 to missing
  sunlight_seq_year_list[[j]] <- sun_bin
}
rm(j)

####for stack 12 bands for sunlight -- logical

sunlight_matrix_logical <- list() #create an empty list

for(j in 1:12){    # 1st to 12th month 
                            temp <- sunlight_seq_year_list[[j]] >= 12 #more or equal to 12 hours of sunlight is consider summer
                            sunlight_matrix_logical[[j]] <- temp
                            rm(temp)
                  }
rm(j)

# loop per month to create stack of logicals

list_of_logicals_months <- list()

for (jj in 1:12) {
sunlight_logical_raster <-stack(raster(sunlight_matrix_logical[[jj]], 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(sunlight_logical_raster) <- paste0("WinterSolarLogic_", jj)
list_of_logicals_months[[jj]] <- sunlight_logical_raster
                }
rm(jj)

sunlight_logical_winter_stack <- stack(list_of_logicals_months)

#################             calculate by precipitation
######     divide months by highest to lowest precipitation to set winter or summer   ####

if(threshold == 'mean') {prec_raster_thres <- calc(monthly_prec, mean) }

if(threshold == 'quantile_3') {
                               prec_raster_mean <- calc(monthly_prec, mean)
                               prec_raster_sd <- calc(monthly_prec, sd)
                               prec_raster_3Q <- prec_raster_mean + (.675)*prec_raster_sd
                               prec_raster_thres <- prec_raster_3Q
                               }

prec_mean_matrix <-as.matrix(prec_raster_thres)

# to create logical PART 1

prec_raster_matrix_1_list <-list()

for (j in 1:12) {
  prec_bin<-as.matrix(monthly_prec[[j]])
  (prec_bin[prec_bin < - 998] <- NA)
  prec_raster_matrix_1_list[[j]] <- prec_bin
}
rm(j)

####for stack 12 bands for winter -- logical PART 2

prec_matrix_winter_logical <- list() #create an empty list

for(j in 1:12){    # 1st to 12th month 
                            temp <- prec_raster_matrix_1_list[[j]] >= prec_mean_matrix #more or equal to 12 hours of sunlight is consider summer
                            prec_matrix_winter_logical[[j]] <- temp
                            rm(temp)
                  }
rm(j)

# loop per month to create stack of logicals

list_of_logicals_prec_months <- list()

for (jj in 1:12) {
prec_logical_raster <-stack(raster(prec_matrix_winter_logical[[jj]], 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(prec_logical_raster) <- paste0("WinterPrecLogic_", jj)
list_of_logicals_prec_months[[jj]] <- prec_logical_raster
                }
rm(jj)

prec_logical_winter_stack <- stack(list_of_logicals_prec_months)

# names to stack relative 

names(sunlight_logical_winter_stack) <- c("WinterSolarLogic_1", "WinterSolarLogic_2", "WinterSolarLogic_3", "WinterSolarLogic_4", "WinterSolarLogic_5", "WinterSolarLogic_6",
                                                       "WinterSolarLogic_7", "WinterSolarLogic_8", "WinterSolarLogic_9", "WinterSolarLogic_10", "WinterSolarLogic_11", "WinterSolarLogic_12")

names(prec_logical_winter_stack) <- c("WinterPrecLogic_1", "WinterPrecLogic_2", "WinterPrecLogic_3", "WinterPrecLogic_4", "WinterPrecLogic_5", "WinterPrecLogic_6",
                                                       "WinterPrecLogic_7", "WinterPrecLogic_8", "WinterPrecLogic_9", "WinterPrecLogic_10", "WinterPrecLogic_11", "WinterPrecLogic_12")

# write.raster

writeRaster(sunlight_logical_winter_stack , filename=paste("WinterSolarLogical_", model, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)
writeRaster(prec_logical_winter_stack , filename=paste("WinterPrecLogical_", model, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(sunlight_logical_winter_stack))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(sunlight_logical_winter_stack), "\n")
cat("grid units: logical 1-cell is winter or 0-cell is summer \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(sunlight_logical_winter_stack), "\n")
cat("minimum values: ", minValue(sunlight_logical_winter_stack), "\n")
cat("file_name_cropped_written: ", paste("WinterSolarLogical_", model, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(prec_logical_winter_stack), "\n")
cat("grid units: logical 1-cell is winter or 0-cell is summer \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(prec_logical_winter_stack), "\n")
cat("minimum values: ", minValue(prec_logical_winter_stack), "\n")
cat("file_name_cropped_written: ", paste("WinterPrecLogical_", model, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

##
setwd(master_directory)

return(list(sunlight_logical_winter_stack, prec_logical_winter_stack))

                                       }

##########################               END OF FUNCTION               ###################


###################################################################################################################
#create relative humidity #########################################################################################
###################################################################################################################

create_relative_humidity_raster_stack <- function (master_stack_dir_value,
                                                   model_value,
                                                   out_dir_name_value) {

# required libraries

require('raster')

# from user

rasterpath <- master_stack_dir_value
model <- model_value
out_path_common_dir <- out_dir_name_value

master_directory <- getwd()

# get list of files

vapor_file <- list.files(path = rasterpath, pattern="^vapor_.*.gri$",full.names=T, ignore.case=T)

if(model == 'present') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_present_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_26_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_45_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_85_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_26_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_45_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_85_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }


# dir.create

dir.create(out_path_common_dir, showWarnings = F)
setwd(out_path_common_dir)

# read rasters

actual_vapor_pressure_rasters_in_KPa_x_1000 <- stack(vapor_file)
maximum_temperature_rasters_in_celsius_x_10 <- stack(tmax_file)

# resolution name

if(round(res(actual_vapor_pressure_rasters_in_KPa_x_1000)[1], 4) == 0.0083) {resolution_name = "30_sec" }
if(round(res(actual_vapor_pressure_rasters_in_KPa_x_1000)[1], 4) == 0.0417) {resolution_name = "2_5_min" }
if(round(res(actual_vapor_pressure_rasters_in_KPa_x_1000)[1], 4) == 0.0833) {resolution_name = "5_min" }
if(round(res(actual_vapor_pressure_rasters_in_KPa_x_1000)[1], 4) == 0.1667) {resolution_name = "10_min" }

# calculate

actual_vapor_pressure_rasters_in_milibars<-list()
maximum_temperature_rasters_in_celsius<-list()
saturation_vapor_pressure_rasters_in_milibars<-list()
relative_humidity_rasters_in_decimal<-list()

for(i in 1:12){

	actual_vapor_pressure_rasters_in_milibars[[i]]<-actual_vapor_pressure_rasters_in_KPa_x_1000[[i]]/100
	
	maximum_temperature_rasters_in_celsius[[i]]<-maximum_temperature_rasters_in_celsius_x_10[[i]]/10
	
	saturation_vapor_pressure_rasters_in_milibars[[i]]<-6.11*10^(7.5*maximum_temperature_rasters_in_celsius[[i]]/(237.7+maximum_temperature_rasters_in_celsius[[i]]))

	relative_humidity_rasters_in_decimal[[i]]<-actual_vapor_pressure_rasters_in_milibars[[i]]/saturation_vapor_pressure_rasters_in_milibars[[i]]
	
	relative_humidity_rasters_in_decimal[[i]][relative_humidity_rasters_in_decimal[[i]] > 1] <- 1

}

# stack relative 

relative_humidity_rasters_in_decimal_stack <- stack(relative_humidity_rasters_in_decimal)
names(relative_humidity_rasters_in_decimal_stack) <- c("rh_1", "rh_2", "rh_3", "rh_4", "rh_5", "rh_6",
                                                       "rh_7", "rh_8", "rh_9", "rh_10", "rh_11", "rh_12")
relative_humidity_rasters_in_decimal_stack_x_10000 <- relative_humidity_rasters_in_decimal_stack * 10000

# write.raster

writeRaster(relative_humidity_rasters_in_decimal_stack_x_10000 , filename=paste("rel_hum_x_10000_", raster_model_name, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(relative_humidity_rasters_in_decimal_stack_x_10000))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(relative_humidity_rasters_in_decimal_stack_x_10000), "\n")
cat("grid units: relative humidity in_decimal * 10000 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(relative_humidity_rasters_in_decimal_stack_x_10000), "\n")
cat("minimum values: ", minValue(relative_humidity_rasters_in_decimal_stack_x_10000), "\n")
cat("file_name_cropped_written: ", paste("rel_hum_x_10000_", raster_model_name, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

##
setwd(master_directory)

return(relative_humidity_rasters_in_decimal_stack_x_10000)

                                       }

##########################               END OF FUNCTION               ###################



###################################################################################################################
#create PET AET and Solar #########################################################################################
###################################################################################################################

create_PET_AET_SolarRad_stack_list <- function (master_stack_dir_value,
                                                   model_value,
                                                   out_dir_name_value) {

# required libraries

require('raster')
require('EcoHydRology')

# from user

rasterpath <- master_stack_dir_value
model <- model_value
out_path_common_dir <- out_dir_name_value

master_directory <- getwd()

# get list of files

slope_aspect_file <- list.files(path = rasterpath, pattern="^slope_aspect_.*.gri$",full.names=T, ignore.case=T)

if(model == 'present') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_present_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_present_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_present_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_26_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_26_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2050_26_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_45_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_45_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2050_45_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2050_85_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2050_85_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2050_85_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2050_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_26_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_26_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2070_26_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_26_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_45_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_45_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2070_45_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_45_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

if(model == '2070_85_rpc') {
            tmax_file <- list.files(path = rasterpath, pattern="^tmax_2070_85_rpc_.*.gri$",full.names=T, ignore.case=T)
            tmin_file <- list.files(path = rasterpath, pattern="^tmin_2070_85_rpc_.*.gri$",full.names=T, ignore.case=T)
            prec_file <- list.files(path = rasterpath, pattern="^prec_2070_85_rpc_.*.gri$",full.names=T, ignore.case=T)
                       }

# stacks files

slope_aspect_stack <- stack(slope_aspect_file)/1000 # the global slope and aspect were multiplied by 1000
tmax_stack <- stack(tmax_file)
tmin_stack <- stack(tmin_file)
prec_stack <- stack(prec_file)


# dir.create

dir.create(out_path_common_dir, showWarnings = F)
setwd(out_path_common_dir)

# resolution name

if(round(res(tmax_stack)[1], 4) == 0.0083) {resolution_name = "30_sec" }
if(round(res(tmax_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min" }
if(round(res(tmax_stack)[1], 4) == 0.0833) {resolution_name = "5_min" }
if(round(res(tmax_stack)[1], 4) == 0.1667) {resolution_name = "10_min" }

# calculate

#### from degrees of latitude of each cell to radians

lat_rad <- coordinates(tmax_stack)[, 2] * pi/180

##########################################################################################
#######                 PET (Potential Evapotranspiration)              ##################
##########################################################################################

# Calculating monthly PET
# The following code takes the function in EcoHydRology and applies 
# it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision) .

for (i in 1:12) {
    evap <- raster(tmax_stack, 1)
    slope <- values(subset(slope_aspect_stack, 1))
    aspect <- values(subset(slope_aspect_stack, 2))
    Tmax <- values(subset(tmax_stack, i))/10
    Tmin <- values(subset(tmin_stack, i))/10
    d <- data.frame(day = (30 * i) - 15, Tmin, Tmax, slope, aspect, lat_rad) # day at the midpoint of each month
    d[is.na(d)] <- 0
    Es_PET <- PET_fromTemp(Jday = d$day, Tmax_C = d$Tmax, Tmin_C = d$Tmin, lat_radians = d$lat_rad, aspect = d$aspect, slope = d$slope) * 1000
    values(evap) <- Es_PET
    if (i == 1) {
        PET <<- brick(evap)
    }
    if (i > 1) {
        PET <<- addLayer(PET, evap)
    }
}

# name PET layers

PET_months <- c("PET_1", "PET_2", "PET_3", "PET_4", "PET_5", "PET_6", "PET_7", "PET_8", "PET_9", "PET_10", "PET_11", "PET_12")
PET_x_100 <- PET * 100
names(PET) <- PET_months
names(PET_x_100) <- PET_months

##########################################################################################
########                 AET (Actual evapotranspiration)             #####################
##########################################################################################

# Estimating AET using a simple bucket model # Duncan Golicher code: 
# AET is Actual evapotranspiration and always lower than PET potential evapotranspiration 
# and can be much lower when the soil profile is well below field capacity.

Bucket <- raster(PET, 1)

for (n in 1:2) {
    for (i in 1:359) {
        mn <- 1 + i%/%30                                # %/% indicates integer division
        NewAET <- raster(PET, 1)
        NewBucket <- values(Bucket)
        rain <- values(subset(prec_stack, mn))/30
        alpha <- (NewBucket - 200)/300
        evap <- values(subset(PET, mn)) * alpha * 0.8   #     A fudge factor for stomatal control.
        NewBucket <- NewBucket + (rain) - evap
        NewBucket[NewBucket > 500] <- 500
        NewBucket[NewBucket < 200] <- 200
        values(Bucket) <- NewBucket
        values(NewAET) <- evap * (NewBucket > 200)
        if (n > 1 && (i%%30) - 15 == 0) {     ## i%%30 will run 1 to 359 and determine position in 30 e.g., 1 is 1 and 61 is 1
            if (mn == 1) {
                AET <<- brick(NewAET)
            }
            if (mn > 1) {
                AET <<- addLayer(AET, NewAET)
            }
        }
    }
}

# name AET layers

AET_months <- c("AET_1", "AET_2", "AET_3", "AET_4", "AET_5", "AET_6", "AET_7", "AET_8", "AET_9", "AET_10", "AET_11", "AET_12")
AET_x_100 <- AET * 100
names(AET) <- AET_months
names(AET_x_100) <- AET_months

##########################################################################################
###########################         Solar Radiation          #############################
##########################################################################################

# Calculating monthly Solar Radiation
# The following code takes the function in EcoHydRology and applies 
# it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision) .

for (k in 1:12) {
    solar_r <- raster(tmax_stack, 1)
    slope <- values(subset(slope_aspect_stack, 1))
    aspect <- values(subset(slope_aspect_stack, 2))
    Tmax <- values(subset(tmax_stack, k))/10
    Tmin <- values(subset(tmin_stack, k))/10
    d <- data.frame(day = (30 * k) - 15, Tmin, Tmax, lat_rad, slope, aspect) # day at the midpoint of each month
    d[is.na(d)] <- 0
    Es_Solar <- Solar(lat = d$lat_rad, Jday = d$day, Tx = d$Tmax, Tn = d$Tmin, albedo=0.2, forest=0, aspect = d$aspect, slope = d$slope, printWarn=F) / 10
    values(solar_r) <- Es_Solar
    if (k == 1) {
        solar_out <<- brick(solar_r)
    }
    if (k > 1) {
        solar_out <<- addLayer(solar_out, solar_r)
    }
}


# name solar layers

solar_months <- c("solarad_1", "solarad_2", "solarad_3", "solarad_4", "solarad_5", "solarad_6", "solarad_7", "solarad_8", "solarad_9", "solarad_10", "solarad_11", "solarad_12")
names(solar_out) <- solar_months


# write.raster

writeRaster(PET_x_100 , filename=paste("PET_x_100_", model, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)
writeRaster(AET_x_100 , filename=paste("AET_x_100_", model, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)
writeRaster(solar_out , filename=paste("SolarRadHydro_", model, "_res_", resolution_name, sep=""), datatype='INT2S', overwrite=TRUE)


# output grid

projection_raster_stack <- capture.output(crs(PET_x_100))

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(PET_x_100), "\n")
cat("grid units: potential Evapotranspiration (in meters) based on the Priestley-Taylor equation (1972) * 100 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(PET_x_100), "\n")
cat("minimum values: ", minValue(PET_x_100), "\n")
cat("file_name_cropped_written: ", paste("PET_x_100_", model, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(AET_x_100), "\n")
cat("grid units: Actual evapotranspiration derived from PET and precipitation using the -simple bucket model- * 100 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(AET_x_100), "\n")
cat("minimum values: ", minValue(AET_x_100), "\n")
cat("file_name_cropped_written: ", paste("AET_x_100_", model, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

cat("------------- Raster file properties ------------\n")
cat("grid names: ", names(solar_out), "\n")
cat("grid units: Solar radiation at the ground surface [kJ m-2 d-1] \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(solar_out), "\n")
cat("minimum values: ", minValue(solar_out), "\n")
cat("file_name_cropped_written: ", paste("SolarRadHydro_", model, "_res_", resolution_name, sep=""), "\n")
cat(projection_raster_stack, "\n")
cat("----------------------------------------\n")

##
setwd(master_directory)

return(list(PET_x_100, AET_x_100, solar_out))

                                       }

##########################               END OF FUNCTION               ###################

###################################################################################################################
#cleanpoints#######################################################################################################
###################################################################################################################

cleanpoints_expanded <- function(spcoord,
                        altpath,
                        km_merge = 2,
                        minimum_number_entries_value = 5,
                        alt_range_in_meters_lower_value = NULL,
                        alt_range_in_meters_higher_value = NULL,
                        plot_clean_data_value = TRUE,
                        out_dir_value = NULL)   {      

require('raster')
require('geosphere')
require('maptools')
require('dismo')
require('rworldmap')
data(wrld_simpl)
data(countriesCoarse)

# input from user

species_data_raw<-spcoord
alt_data_raster <- altpath
y <- (km_merge)/100 # 1 km ~ 0.01 degrees (also 0.05 degrees ~ 5 km)
minimum_number_entries <- minimum_number_entries_value
alt_range_in_meters_lower <- alt_range_in_meters_lower_value
alt_range_in_meters_higher <- alt_range_in_meters_higher_value
plot_clean_data <- plot_clean_data_value
out_dir <- out_dir_value

master_directory <- getwd()

# dir.create

if(is.null(out_dir) == FALSE) {
                              dir.create(out_dir, showWarnings = F)
                              setwd(out_dir)
                                    }


###########    preprocessing split file of species distributions to vectors    ###########

# read alt raster

alt_data <- raster(alt_data_raster)

# read coordinates file if not data frame

if(class(spcoord) == "data.frame") {
                     spcoord_table <- spcoord
                                   } else {
if(grepl(pattern = "*.csv$", spcoord) == TRUE) {spcoord_table <- read.table(file = spcoord, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", spcoord) == TRUE) {spcoord_table <- read.table(file = spcoord, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

# split file by species

species_split <- split(spcoord_table, spcoord_table$species)

# summary of entries in the  original dataset

names_species_split <- as.data.frame(rownames(summary(species_split)))

n_entries <- list()
for (i in 1:nrow(names_species_split)) {
                                       n_entries[[i]] <- nrow(species_split[[i]])
                                       }

n_entries_df <-as.data.frame(unlist(n_entries))
names_species_split_df <- cbind(names_species_split,n_entries_df)
names(names_species_split_df) <- c("species", "n_entries_raw")

# from list to vectors

Y <- lapply(seq_along(species_split), function(x) as.data.frame(species_split[[x]]))#as dataframes
Z_all <- lapply(seq_along(Y),  function(x) unique (Y[[x]]) )#unique data values per dataframe

###########################################################################################

Z_one <- list()
counter_1 <- 0

for(i in 1:length(Z_all)) {

if (nrow(Z_all[[i]]) > minimum_number_entries) {
                       counter_1 <- counter_1 + 1
                   Z_one[[counter_1]] <- Z_all[[i]]
                                            }
                        }

## exclude if all the same

Z_two <- list()
counter_2 <- 0

# Z_one[[3]]

for(i in 1:length(Z_one)) {

                    are_unique_rows <- unique(Z_one[[i]][ , c("Lon", "Lat") ] )
                    are_unique_lat <- unique(Z_one[[i]][ , c("Lat") ] )
                    are_unique_lon <- unique(Z_one[[i]][ , c("Lon") ] )



if (!nrow(are_unique_rows) <= 1 && !length(are_unique_lat) <= 1 && !length(are_unique_lon) <= 1) {
                       counter_2 <- counter_2 + 1
                   Z_two[[counter_2]] <- Z_one[[i]]
                                            }

                        }

Z <- Z_two


###########        Start: processing loop for all species in the species file         ###########

summary_clean_list <- list()

for (ww in 1:length(Z)) {

# processing

Z_2 <- Z[[ww]]

# Z_2 <- Z[[2]]

# get species name

Z_2_species <- unique(Z_2$species)

# subset Lon and Lat

Z_2_Lon_Lat <- subset(Z_2, select = c(Lon, Lat)) 

# get hr_res to for resolution to less than an hour 

Z_2_area_not_round <- (areaPolygon(Z_2_Lon_Lat))/1000000 #area in square Km round 
Z_2_area <- round(Z_2_area_not_round, 0) #area in square Km round to integer
Z_2_area_hr_res <- 10-(round(log10(ifelse (Z_2_area+1>10000000,10000000, Z_2_area+1)),0)+1) #this function assing an interger from 2 to 9 (for really small area 1 km2) based on the area in km2. However, it forces to 2 for extremely large distributions > 10 000 000 km2

# process to reduce redundant localities

coordinates(Z_2) <- ~Lon+Lat #set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
crs(Z_2) <- crs(wrld_simpl) #Get or set the coordinate reference system (CRS) of a Raster* object.
Z_2_r <- try(raster(Z_2))
res(Z_2_r) <- y # y = 0.05 then 5 km ~0.05 degrees resolution
Z_2_r_e <- extend(Z_2_r, extent(Z_2_r)+1)
Z_2_r_e_acsel <- gridSample(Z_2, Z_2_r_e, n=1)
Z_2_r_e_acsel_df <- data.frame(Z_2_r_e_acsel)

# exclude localities in the ocean

georef<- Z_2_r_e_acsel_df
georef_alt<- cbind(georef, alt = extract(alt_data, georef, method = "bilinear"))
species_selected_coordinates <- georef_alt[complete.cases(georef_alt$alt),]

# subset if altitude range is included

if(is.null(alt_range_in_meters_lower) == FALSE) {species_selected_coordinates <- subset(species_selected_coordinates, alt > alt_range_in_meters_lower)}
if(is.null(alt_range_in_meters_higher) == FALSE) {species_selected_coordinates <- subset(species_selected_coordinates, alt < alt_range_in_meters_higher)}

# add other species info

species_selected_coordinates$species <- Z_2_species

## for summary

n_entries_species <- nrow(Z_2_Lon_Lat)
n_entries_clean <- nrow(species_selected_coordinates)
summary_cleaning_df <- data.frame(species = Z_2_species, n_entries_raw = n_entries_species, n_entries_clean = n_entries_clean, km_resolution_merging = y*100, stringsAsFactors = FALSE)

## for summary

summary_clean_list[[ww]] <- summary_cleaning_df

# if plot is requested

if (plot_clean_data == TRUE) {
                                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch=1, cex=0.75, ylab='latitude', xlab='longitude')
                                plot(countriesCoarse, add=T, border='blue', lwd=1)
                                title(main= Z_2_species) 

                                pdf(paste0(Z_2_species, "_total_", n_entries_clean, "_clean_points_res_", y*100,"_km2.pdf"))
                                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch=1, cex=0.75, ylab='latitude', xlab='longitude')
                                plot(countriesCoarse, add=T, border='blue', lwd=1)
                                title(main= Z_2_species) 
                                dev.off()

                                }

# write clean points

write.table(species_selected_coordinates, file = paste0(Z_2_species, "_total_", n_entries_clean, "_clean_points_res_", y*100,"_km2.txt"), sep = "\t", row.names = FALSE)

                                          }

##########################################################################################

# bind summaries

n_entries_clean_bind <- do.call(rbind, summary_clean_list)

# return list

print(n_entries_clean_bind)
setwd(master_directory)
return(species_selected_coordinates)

                                       }

##########################               END OF FUNCTION               ###################



###################################################################################################################
#species_monthly_sunset_sunrise_daylength_distribution ############################################################
###################################################################################################################

monthly_sunset_sunrise <- function(spcoord_value,
                                   GTM_off_raster_value,
                                   requested_summary_value,
                                   out_dir_name_value) {

require('insol')
require('plyr')

# user input

spcoord <- spcoord_value
GTM_off_raster_file <- GTM_off_raster_value
requested_summary <- requested_summary_value
out_dir <- out_dir_name_value

master_directory <- getwd()

# dir.create

if(is.null(out_dir) == FALSE) {
                              dir.create(out_dir, showWarnings = F)
                              setwd(out_dir)
                                    }


###########    preprocessing split file of species distributions to vectors    ###########

# stack rasters

if(class(GTM_off_raster_file)[1] == "RasterStack") {
                                   GTM_off_raster <-GTM_off_raster_file
                                              } else {
                                   GTM_off_raster <- stack(GTM_off_raster_file)}

# read coordinates file if not data frame

if(class(spcoord) == "data.frame") {
                     spcoord_table <- spcoord
                                   } else {
if(grepl(pattern = "*.csv$", spcoord) == TRUE) {spcoord_table <- read.table(file = spcoord, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", spcoord) == TRUE) {spcoord_table <- read.table(file = spcoord, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

############ extract tmz hours form GTM_off_raster 

coord_df <- subset(spcoord_table, select = c("Lon", "Lat"))
Lon_Lat_matrix <- data.matrix(coord_df)

# extract time zone

GTM_df <- as.data.frame(extract(GTM_off_raster, Lon_Lat_matrix), stringsAsFactors = FALSE)
names(GTM_df) <- "GTM"

# fromr raster to matrix of coordinates

#######     jan

jan_days_list <- list()
counter <- 0

for (i in 1:31) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 jan_days_list[[counter]] <- one_day
                }

jan_mean <- aaply(laply(jan_days_list, as.matrix), c(2, 3), mean)

#######     feb_

feb_days_list <- list()
counter <- 0

for (i in 32:59) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 feb_days_list[[counter]] <- one_day
                }

feb_mean <- aaply(laply(feb_days_list, as.matrix), c(2, 3), mean)

#######     mar_

mar_days_list <- list()
counter <- 0

for (i in 60:90) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 mar_days_list[[counter]] <- one_day
                }

mar_mean <- aaply(laply(mar_days_list, as.matrix), c(2, 3), mean)

#######     apr_

apr_days_list <- list()
counter <- 0

for (i in 91:120) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 apr_days_list[[counter]] <- one_day
                }

apr_mean <- aaply(laply(apr_days_list, as.matrix), c(2, 3), mean)

#######     may_

may_days_list <- list()
counter <- 0

for (i in 121:151) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 may_days_list[[counter]] <- one_day
                }

may_mean <- aaply(laply(may_days_list, as.matrix), c(2, 3), mean)

#######     jun_

jun_days_list <- list()
counter <- 0

for (i in 152:181) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 jun_days_list[[counter]] <- one_day
                }

jun_mean <- aaply(laply(jun_days_list, as.matrix), c(2, 3), mean)

#######     jul_

jul_days_list <- list()
counter <- 0

for (i in 182:212) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 jul_days_list[[counter]] <- one_day
                }

jul_mean <- aaply(laply(jul_days_list, as.matrix), c(2, 3), mean)

#######     aug_

aug_days_list <- list()
counter <- 0

for (i in 213:243) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 aug_days_list[[counter]] <- one_day
                }

aug_mean <- aaply(laply(aug_days_list, as.matrix), c(2, 3), mean)

#######     sep_

sep_days_list <- list()
counter <- 0

for (i in 244:273) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 sep_days_list[[counter]] <- one_day
                }

sep_mean <- aaply(laply(sep_days_list, as.matrix), c(2, 3), mean)

#######     oct_

oct_days_list <- list()
counter <- 0

for (i in 274:304) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 oct_days_list[[counter]] <- one_day
                }

oct_mean <- aaply(laply(oct_days_list, as.matrix), c(2, 3), mean)

#######     nov_

nov_days_list <- list()
counter <- 0

for (i in 305:334) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 nov_days_list[[counter]] <- one_day
                }

nov_mean <- aaply(laply(nov_days_list, as.matrix), c(2, 3), mean)

#######     dec_

dec_days_list <- list()
counter <- 0

for (i in 335:365) {
                 counter <- counter + 1
                 one_day <- as.data.frame(t(mapply(insol::daylength, coord_df$Lat, coord_df$Lon, jd=i, GTM_df$GTM)), stringsAsFactors = FALSE)
                 names(one_day) <- c("sunrise", "sunset", "daylength")
                 dec_days_list[[counter]] <- one_day
                }

dec_mean <- aaply(laply(dec_days_list, as.matrix), c(2, 3), mean)

#############################  stats per month 

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

if(requested_summary == 'mean') {

jan_sum_means <- colMeans(jan_mean)
feb_sum_means <- colMeans(feb_mean)
mar_sum_means <- colMeans(mar_mean)
apr_sum_means <- colMeans(apr_mean)
may_sum_means <- colMeans(may_mean)
jun_sum_means <- colMeans(jun_mean)
jul_sum_means <- colMeans(jul_mean)
aug_sum_means <- colMeans(aug_mean)
sep_sum_means <- colMeans(sep_mean)
oct_sum_means <- colMeans(oct_mean)
nov_sum_means <- colMeans(nov_mean)
dec_sum_means <- colMeans(dec_mean)

reference_month_auto <- data.frame(month = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                                   sunrise = c(jan_sum_means[1], feb_sum_means[1], mar_sum_means[1], apr_sum_means[1], may_sum_means[1], jun_sum_means[1], jul_sum_means[1], aug_sum_means[1], sep_sum_means[1], oct_sum_means[1], nov_sum_means[1], dec_sum_means[1]),
                                   sunset = c(jan_sum_means[2], feb_sum_means[2], mar_sum_means[2], apr_sum_means[2], may_sum_means[2], jun_sum_means[2], jul_sum_means[2], aug_sum_means[2], sep_sum_means[2], oct_sum_means[2], nov_sum_means[2], dec_sum_means[2]),
                                   daylength = c(jan_sum_means[3], feb_sum_means[3], mar_sum_means[3], apr_sum_means[3], may_sum_means[3], jun_sum_means[3], jul_sum_means[3], aug_sum_means[3], sep_sum_means[3], oct_sum_means[3], nov_sum_means[3], dec_sum_means[3]), stringsAsFactors = F)

name_species <- ifelse(is.null(unique(spcoord_table$species)), NA, unique(spcoord_table$species))
if(!is.na(name_species)) {reference_month_auto$species <- name_species}

                                 }

if(requested_summary == 'max') {

jan_sum_means <- colMax(as.data.frame(jan_mean))
feb_sum_means <- colMax(as.data.frame(feb_mean))
mar_sum_means <- colMax(as.data.frame(mar_mean))
apr_sum_means <- colMax(as.data.frame(apr_mean))
may_sum_means <- colMax(as.data.frame(may_mean))
jun_sum_means <- colMax(as.data.frame(jun_mean))
jul_sum_means <- colMax(as.data.frame(jul_mean))
aug_sum_means <- colMax(as.data.frame(aug_mean))
sep_sum_means <- colMax(as.data.frame(sep_mean))
oct_sum_means <- colMax(as.data.frame(oct_mean))
nov_sum_means <- colMax(as.data.frame(nov_mean))
dec_sum_means <- colMax(as.data.frame(dec_mean))

reference_month_auto <- data.frame(month = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                                   sunrise = c(jan_sum_means[1], feb_sum_means[1], mar_sum_means[1], apr_sum_means[1], may_sum_means[1], jun_sum_means[1], jul_sum_means[1], aug_sum_means[1], sep_sum_means[1], oct_sum_means[1], nov_sum_means[1], dec_sum_means[1]),
                                   sunset = c(jan_sum_means[2], feb_sum_means[2], mar_sum_means[2], apr_sum_means[2], may_sum_means[2], jun_sum_means[2], jul_sum_means[2], aug_sum_means[2], sep_sum_means[2], oct_sum_means[2], nov_sum_means[2], dec_sum_means[2]),
                                   daylength = c(jan_sum_means[3], feb_sum_means[3], mar_sum_means[3], apr_sum_means[3], may_sum_means[3], jun_sum_means[3], jul_sum_means[3], aug_sum_means[3], sep_sum_means[3], oct_sum_means[3], nov_sum_means[3], dec_sum_means[3]), stringsAsFactors = F)

name_species <- ifelse(is.null(unique(spcoord_table$species)), NA, unique(spcoord_table$species))
if(!is.na(name_species)) {reference_month_auto$species <- name_species}

                                 }

if(requested_summary == 'min') {

jan_sum_means <- colMin(as.data.frame(jan_mean))
feb_sum_means <- colMin(as.data.frame(feb_mean))
mar_sum_means <- colMin(as.data.frame(mar_mean))
apr_sum_means <- colMin(as.data.frame(apr_mean))
may_sum_means <- colMin(as.data.frame(may_mean))
jun_sum_means <- colMin(as.data.frame(jun_mean))
jul_sum_means <- colMin(as.data.frame(jul_mean))
aug_sum_means <- colMin(as.data.frame(aug_mean))
sep_sum_means <- colMin(as.data.frame(sep_mean))
oct_sum_means <- colMin(as.data.frame(oct_mean))
nov_sum_means <- colMin(as.data.frame(nov_mean))
dec_sum_means <- colMin(as.data.frame(dec_mean))

reference_month_auto <- data.frame(month = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                                   sunrise = c(jan_sum_means[1], feb_sum_means[1], mar_sum_means[1], apr_sum_means[1], may_sum_means[1], jun_sum_means[1], jul_sum_means[1], aug_sum_means[1], sep_sum_means[1], oct_sum_means[1], nov_sum_means[1], dec_sum_means[1]),
                                   sunset = c(jan_sum_means[2], feb_sum_means[2], mar_sum_means[2], apr_sum_means[2], may_sum_means[2], jun_sum_means[2], jul_sum_means[2], aug_sum_means[2], sep_sum_means[2], oct_sum_means[2], nov_sum_means[2], dec_sum_means[2]),
                                   daylength = c(jan_sum_means[3], feb_sum_means[3], mar_sum_means[3], apr_sum_means[3], may_sum_means[3], jun_sum_means[3], jul_sum_means[3], aug_sum_means[3], sep_sum_means[3], oct_sum_means[3], nov_sum_means[3], dec_sum_means[3]), stringsAsFactors = F)

name_species <- ifelse(is.null(unique(spcoord_table$species)), NA, unique(spcoord_table$species))
if(!is.na(name_species)) {reference_month_auto$species <- name_species}

                                 }

# write clean points

write.table(reference_month_auto, file = paste0(name_species, "_", requested_summary, "_sunrise_sunset_daylength_per_month.txt"), sep = "\t", row.names = FALSE)

setwd(master_directory)

detach("package:insol", unload=TRUE)

return(reference_month_auto)

                                         }

##########################               END OF FUNCTION               ###################

##########################################################################################
##########                        Mapinguari_sdm_map_plot                   ##############
##########################################################################################

Mapinguari_sdm_map_plot <- function ( raster_value,
                    species_coordinates_file_value,
                              extent_lat_max_value = NULL,
                              extent_lat_min_value = NULL,
                              extent_lon_max_value = NULL,
                              extent_lon_min_value = NULL,
                           variables_to_plot_value = "all",
                        present_palette_name_value = NULL,
                        color_min_middle_max_value = c("blue", "white", "red"),
                      number_of_color_breaks_value = 100,
                                  add_points_value = FALSE,
                          add_coutry_borders_value = FALSE,
          load_rds_file_country_borders_name_value = NULL,
                      maps_folder_dir_output_value = NULL,
          save_rds_file_country_borders_name_value = NULL,
                             path_output_dir_value = NULL) {

# require
require("ggplot2")
require("Rmisc")
require("dismo")
require("countrycode")
require("maps")
require("mapdata")
require("maptools")

# palettes requested by user

palettes_list <- list (Cavalcanti = c("#D8B70A","#972D15"),
                            Royal1 = c("#899DA4", "#DC863B"),
                            Royal2 = c("#9A8822", "#74A089"),
                            Chevalier = c("#446455", "#C7B19C"),
                            Zissou = c("#3B9AB2", "#F21A00"),
                            Darjeeling = c("#00A08A", "#F98400"),
                            daynight_3 = c("#063852", "#E6DF44", "#F0810F"),
                            tropical_3 = c("4897D8", "#FFDB5C", "#FA6E59"),
                            muted_3 = c("#A4CABC", "#EAB364", "#B2473E"),
                            retro_3 = c("#97B8C2", "#D6C6B9", "#D35C37"))

# input from user

raster <- raster_value
sp_coord_file <- species_coordinates_file_value
lat_max <- extent_lat_max_value
lat_min <- extent_lat_min_value
lon_max <- extent_lon_max_value
lon_min <- extent_lon_min_value
var_to_plot <- variables_to_plot_value

palette_name <- present_palette_name_value
if(!is.null(palette_name)) { palette_name <- palettes_list[[palette_name]] }

color_min_middle_max <- color_min_middle_max_value
color_breaks <- number_of_color_breaks_value
add_points <- add_points_value
add_coutry <- add_coutry_borders_value

load_rds_file <- load_rds_file_country_borders_name_value
maps_folder_dir_output <- maps_folder_dir_output_value
save_rds_file <- save_rds_file_country_borders_name_value

output_dir <- path_output_dir_value

master_directory <- getwd()

###################            create dir             #########################

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                                    }

###################            read files or are data_frames             ################

if(is.null(sp_coord_file)) { species_name <- NA
                             species_coordinates <- NA 
            } else if(class(sp_coord_file) == "data.frame") {
                             sp_coord <- sp_coord_file
                             species_name <- unique(sp_coord$species)
                             species_coordinates <- sp_coord
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) 
                                                      species_name <- unique(sp_coord$species)
                                                      species_coordinates <- sp_coord}
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) 
                                                      species_name <- unique(sp_coord$species)
                                                      species_coordinates <- sp_coord}
                                          }

###################            read rasters             ################

if(class(raster) == "character") {
                                raster_stack_master <- stack(raster)
                                 }

if(class(raster) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                raster_stack_master <- raster
                                 }

###################            subset rasters             ################

if(var_to_plot == "all") {
                         raster_stack <- raster_stack_master
                         } else {
                         raster_stack <- subset(raster_stack_master, var_to_plot)
                                }

########################               crop rasters              #########################

if(!is.null(lat_max) | !is.null(lon_max) | !is.null(lat_min) | !is.null(lon_min)) {
                       common_extent <- extent(lon_min, lon_max, lat_min, lat_max)
                       raster_stack <-  crop(raster_stack, common_extent)
                       }

########################           get borders polygons          #########################

if(add_coutry == TRUE) {

        if(is.null(load_rds_file)) {

# input from user

raster_man <- raster_stack[[1]]
number_random_coordinates <- 100000

# random sample of coordinates in map

raster_cells <- raster_man@nrows * raster_man@ncols
random_cells <- sample(1:raster_cells, number_random_coordinates, replace = TRUE)
coordinates_map <- as.data.frame(xyFromCell(raster_man, random_cells, spatial=FALSE))

# get names same names as species presence coordinates

colnames(coordinates_map) <- c("Lon", "Lat")

# prepare a SpatialPolygons object with one poly per country

countries <- map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
names <- sapply(strsplit(countries$names, ":"), function(x) x[1])

# clean up polygons that are out of bounds

filter <- countries$x < -180 & !is.na(countries$x)
countries$x[filter] <- -180
filter <- countries$x > 180 & !is.na(countries$x)
countries$x[filter] <- 180

countriesSP <- map2SpatialPolygons(countries, IDs=countries$names, proj4string=CRS('+proj=longlat'))

# convert our list of points to a SpatialPoints object

pointsSP <- SpatialPoints(coordinates_map, proj4string=CRS('+proj=longlat'))

# use 'over' to get indices of the Polygons object containing each point 

indices <- over(pointsSP, countriesSP)

# Return the state names of the Polygons object containing each point

myNames <- sapply(countriesSP@polygons, function(x) x@ID)
country_names_raw <- myNames[indices]
country_names_no_na <- unique(country_names_raw[!is.na(country_names_raw)])
country_final <- unique(gsub("\\:.*","",country_names_no_na))

# get iso3 names for GADM

iso3_character_country <- countrycode(country_final, "country.name", "iso3c")
iso3_character_country_no_na <- iso3_character_country[!is.na(iso3_character_country)]
iso3_character_country_no_na_unique <- unique (iso3_character_country_no_na)

# return final list

country_final_list <- list()
country_final_list[[paste("country_ISO3")]] <- iso3_character_country_no_na_unique
country_final_list[[paste("country_names")]] <- country_final

# print to console

cat("\n--------------------------    Borders of countries requested    --------------------------\n")
print(country_final_list)
cat("\n------------------------------------------------------------------------------------------\n")

# input to download borders

maps_folder_dir_output <- maps_folder_dir_output
species_country_iso3_codes <- country_final_list$country_ISO3
level_resolution <- 0
species_extent <- extent(raster_man)

# get file list of countries if they exist in master database

contry_maps_GADM_list <- list ()
counter <- 0

for (i in 1:length(species_country_iso3_codes)) {
                                                   counter <- counter + 1
                                                   country_GADM <- try(raster::getData('GADM', country=species_country_iso3_codes[i], path=maps_folder_dir_output, level=level_resolution), silent = TRUE)
                                                   if(!class(country_GADM) == "try-error") {
                                                                                            contry_maps_GADM_list [[counter]] <- crop (country_GADM, species_extent)
                                                                                           }
                                                }
rm(i)

list_country_border_polygons <- contry_maps_GADM_list

if(is.null(save_rds_file)) {
                           saveRDS(list_country_border_polygons, file = "country_borders_cropped.rds")
                          } else {
                           saveRDS(list_country_border_polygons, file = paste0(save_rds_file, "_country_borders_cropped.rds"))
                                 }

                                          } # end loop if(is.null(load_rds_file))

##########################################################################################

if(!is.null(load_rds_file)) {

list_country_border_polygons_raw <- readRDS(file = load_rds_file)
raster_man <- raster_stack[[1]]
species_extent <- extent(raster_man)

contry_maps_GADM_list_1 <- list()

for (i in 1:length(list_country_border_polygons_raw)) {
                                                      contry_maps_GADM_list_1[[i]] <- crop (list_country_border_polygons_raw[[i]], species_extent)
                                                }
rm(i)

# remove null country borders that are not in the area

contry_maps_GADM_list <- contry_maps_GADM_list_1[!sapply(contry_maps_GADM_list_1, is.null)] 

list_country_border_polygons <- contry_maps_GADM_list

                            } # end of loop if(!is.null(load_rds_file))

            }  # end of loop if(add_coutry == TRUE)

##########################################################################################

# plot a map a time

for(i in 1:nlayers(raster_stack)) {

                 one_raster_plot <- raster_stack[[i]]
#                 one_raster_plot <- raster_stack[[1]]

                  one_raster_max <- maxValue(one_raster_plot)
                  one_raster_min <- minValue(one_raster_plot)

                  one_raster_brakes <- seq(one_raster_min, one_raster_max, length.out = color_breaks) 
                  one_raster_nb <- length(one_raster_brakes)

              if(is.null(palette_name)) {
                    if(is.null(color_min_middle_max)) {
                      one_raster_color <-colorRampPalette(c("#899DA4", "#DC863B"))(one_raster_nb)
                                                      } else {
                      one_raster_color <-colorRampPalette(color_min_middle_max)(one_raster_nb) } }

              if(!is.null(palette_name)) { one_raster_color <-colorRampPalette(palette_name)(one_raster_nb) } 

              one_raster_plot_extent <- extent(one_raster_plot)

    if(add_points == TRUE) { species_coordinates$Lat[species_coordinates$Lat > one_raster_plot_extent[4]] <- NA
                             species_coordinates$Lat[species_coordinates$Lat < one_raster_plot_extent[3]] <- NA
                             species_coordinates$Lon[species_coordinates$Lon > one_raster_plot_extent[2]] <- NA
                             species_coordinates$Lon[species_coordinates$Lon < one_raster_plot_extent[1]] <- NA
                             species_coordinates <- na.omit(species_coordinates)
                             coordinates_matrix <- data.matrix(subset(species_coordinates, select = c(Lon, Lat)))
                             }

# plot

if(exists("species_name")) { main_title = paste0(species_name, "_", names(one_raster_plot))} else {main_title = names(one_raster_plot)}
lab_breaks_arg <- list(at=c(min(one_raster_brakes),  (max(one_raster_brakes) + min(one_raster_brakes))/2, max(one_raster_brakes)), labels=c(min(one_raster_brakes),  (max(one_raster_brakes) + min(one_raster_brakes))/2, max(one_raster_brakes)))

plot(one_raster_plot, breaks=one_raster_brakes, col=one_raster_color, lab.breaks=one_raster_brakes, main= main_title, axis.args=lab_breaks_arg) 
         if(add_points == TRUE) {points(coordinates_matrix, pch=20, cex=0.7, col="black")}
           if(add_coutry == TRUE){ 
                for (j in 1:length(list_country_border_polygons)) {plot (list_country_border_polygons[[j]],add=T, border="black",lwd=0.7)}
                                 }
rm(j)

pdf(paste0(main_title, "_raster_plot.pdf", sep=""), width = 12, height = 6)

    plot(one_raster_plot, breaks=one_raster_brakes, col=one_raster_color, lab.breaks=one_raster_brakes, main=main_title, axis.args=lab_breaks_arg) 
         if(add_points == TRUE) {points(coordinates_matrix, pch=20, cex=0.7, col="black")}
           if(add_coutry == TRUE){ 
                for (j in 1:length(list_country_border_polygons)) {plot (list_country_border_polygons[[j]],add=T, border="black",lwd=0.7)}
                                 }
rm(j)
dev.off()

                                  } # end loop for(i in 1:nlayers(raster_stack))
rm(i)

# return to home directory

setwd(master_directory)

                                                     }

##########################           End of function         ##############################

# home

#############################################################################################################################
#Process HOBO Data    ####################################################################################################
#############################################################################################################################

Process_HOBO_data <-function(HOBOdata_value,
				   method_value,
				   Tlwr_value,
				   Tupr_value,
				   hcap_value=NULL,
				   diel_value='diurnal',
				   out_dir_value = NULL){

require(geosphere)

# user input

HOBOdata <- HOBOdata_value
method <- method_value
Tlwr <- Tlwr_value
Tupr <- Tupr_value
hcap <- hcap_value
diel <- diel_value
variables <- c('ha','hr')

out_dir <- out_dir_value

master_directory <- getwd()

# dir.create

if(is.null(out_dir) == FALSE) {
                              dir.create(out_dir, showWarnings = F)
                              setwd(out_dir)
                                    }

# read HOBO file if not data frame

if(class(HOBOdata) == "data.frame") {
                             HOBOdf_raw <- HOBOdata
                                   } else {
if(grepl(pattern = "*.csv$", HOBOdata) == TRUE) {HOBOdf_raw <- read.table(file = HOBOdata, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", HOBOdata) == TRUE) {HOBOdf_raw <- read.table(file = HOBOdata, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

# data frame

HOBOdf <- HOBOdf_raw

# merge day_month_year

HOBOdf$day_month_year <- paste(HOBOdf$day, "_", HOBOdf$month, "_", HOBOdf$year, sep ="")

if(method=='max'){
HOBOdf$max_HOBOs  <- apply(HOBOdf[,grep("^temp_", colnames(HOBOdf))], 1, function(x) max(na.omit(x)))

if(is.element('hr',variables) ){
binaryhr <- list()

                 if (!all(is.na(HOBOdf$max_HOBOs))) {                 	
binaryhr <-sapply(HOBOdf$max_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tupr, 0, 1))))
                                                                  
                      } 
                      HOBOdf <- cbind(HOBOdf, binaryhr)
                      }  

if(is.element('ha',variables) ){
binaryha <- list()

                 if (!all(is.na(HOBOdf$max_HOBOs))) {                 	
binaryha <-sapply(HOBOdf$max_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tlwr, 0, 1))))
                                                                
                      }
                      HOBOdf <- cbind(HOBOdf, binaryha)                        
                      }
                        
}


if(method=='mean'){
HOBOdf$mean_HOBOs  <- apply(HOBOdf[,grep("^temp_", colnames(HOBOdf))], 1, function(x) mean(na.omit(x)))

if(is.element('hr',variables) ){
binaryhr <- list()

                 if (!all(is.na(HOBOdf$mean_HOBOs))) {                 	
binaryhr <-sapply(HOBOdf$mean_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tupr, 0, 1))))
                                                                 
                      }
                      HOBOdf <- cbind(HOBOdf, binaryhr)
                      }         
                            
if(is.element('ha',variables) ){
binaryha <- list()

                 if (!all(is.na(HOBOdf$mean_HOBOs))) {                 	
binaryha <-sapply(HOBOdf$mean_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tlwr, 0, 1))))
                                                             
                      }
					  HOBOdf <- cbind(HOBOdf, binaryha)
                      }   
                        
}

if(method=='min'){
HOBOdf$min_HOBOs  <- apply(HOBOdf[,grep("^temp_", colnames(HOBOdf))], 1, function(x) min(na.omit(x)))

if(is.element('hr',variables) ){

binaryhr <- list()

                 if (!all(is.na(HOBOdf$min_HOBOs))) {                 	
binaryhr <-sapply(HOBOdf$min_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tupr, 0, 1))))
                                                                 
                      }
                      HOBOdf <- cbind(HOBOdf, binaryhr)
                      }               

if(is.element('ha',variables) ){
binaryha <- list()

                 if (!all(is.na(HOBOdf$min_HOBOs))) {                 	
binaryha <-sapply(HOBOdf$min_HOBOs, function(x) (ifelse (is.na(x), NA,ifelse (x < Tlwr, 0, 1))))
                                                                
                      }
                      HOBOdf <- cbind(HOBOdf, binaryha)
                      }   
                        
}

##########################################################################################
##########################################################################################


####   split by locality

locality_HOBO_split_by_day <- split(HOBOdf, HOBOdf$locality)

### proces per locality

locality_HOBO_list <- list ()

for(z in 1:length(locality_HOBO_split_by_day)) {

## one locality

HOBOdf_one_locality <-  locality_HOBO_split_by_day[[z]]

# HOBOdf_one_locality <-  locality_HOBO_split_by_day[[1]]

####   split by day and 

raw_HOBO_split_by_day <- split(HOBOdf_one_locality, HOBOdf_one_locality$day_month_year)

#### from list to data frames

raw_HOBO_split_by_day_df_list <- lapply(seq_along(raw_HOBO_split_by_day), function(x) as.data.frame(raw_HOBO_split_by_day[[x]]))

########################           loop for each day                ######################

richards_data_point_day_list <- list()

for ( i in 1:length(raw_HOBO_split_by_day_df_list)) {

temp_all_values_complete <- raw_HOBO_split_by_day_df_list[[i]]

# temp_all_values_complete <- raw_HOBO_split_by_day_df_list[[1]]

#######################          time interval in minutes

total_secs_in_minutes <- (temp_all_values_complete$second[2] - temp_all_values_complete$second[1])/60
total_min_in_minutes <- (temp_all_values_complete$minute[2] - temp_all_values_complete$minute[1])

total_seconds_and_minutes_interval <- total_secs_in_minutes+total_min_in_minutes

if(total_seconds_and_minutes_interval == 0 ) {
                                  frac_in_minutes_interval <- (temp_all_values_complete$hour[2] - temp_all_values_complete$hour[1]) * 60
                                             } else {
                                  frac_in_minutes_interval <- total_seconds_and_minutes_interval
                                             }

##########################            determine day of the year

dateTime_station_name <- paste(unique(temp_all_values_complete$month), "/", unique(temp_all_values_complete$day), "/", unique(temp_all_values_complete$year), sep ="")

# convert to day of year (Julian date)

day_of_year_calculated <- strptime(dateTime_station_name, "%m/%d/%Y")$yday+1

##########################            determine hours of sunrise and sunset to exclude night

## sunrise and sunset hour calculator

suncalc<-function(day_of_year ,Lat,Long){

# http://www.r-bloggers.com/approximate-sunrise-and-sunset-times/
d <- day_of_year

## Function to convert degrees to radians
rad<-function(x)pi*x/180

##Radius of the earth (km)
R=6378

##Radians between the xy-plane and the ecliptic plane
epsilon=rad(23.45)

##Convert observer's latitude to radians
L=rad(Lat)

## Calculate offset of sunrise based on longitude (min)
timezone = -4*(abs(Long)%%15)*sign(Long)

## The earth's mean distance from the sun (km)
r = 149598000
theta = 2*pi/365.25*(d-80)
z.s = r*sin(theta)*sin(epsilon)
r.p = sqrt(r^2-z.s^2)
t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))

##a kludge adjustment for the radius of the sun
that = t0+5 

## Adjust "noon" for the fact that the earth's orbit is not circular:
n = 720-10*sin(4*pi*(d-80)/365.25)+8*sin(2*pi*d/365.25)

## now sunrise and sunset are:

sunrise = (n-that+timezone)/60
sunset = (n+that+timezone)/60

return(list("sunrise" = sunrise,"sunset" = sunset))
}


sunrise_sunset_hour_station  <- suncalc (day_of_year =day_of_year_calculated, Lat=temp_all_values_complete$Lat[1],Long=temp_all_values_complete$Lon[1])

sunrise_round <- round(sunrise_sunset_hour_station[[1]],0)
sunset_round <- round(sunrise_sunset_hour_station[[2]],0)

#######################          time brackets for diel behavior: diurnal or nocturnal

      if( diel == 'diurnal') {
                         temp_all_values  <- subset (temp_all_values_complete, hour >= sunrise_round & hour <= sunset_round)
                                }
      if( diel == 'nocturnal') {
                         temp_all_values  <- subset (temp_all_values_complete, hour <= sunrise_round | hour >= sunset_round)
                                  }

#######################          -- hr day summary

hr_day <- (sum(temp_all_values$binaryhr)/60) *frac_in_minutes_interval 

#######################          -- ha day summary

if(is.null(hcap)) {
          ha_day <- (sum(temp_all_values$binaryha)/60) *frac_in_minutes_interval
                  } else {
          ha_day_temp <- (sum(temp_all_values$binaryha)/60) *frac_in_minutes_interval
               ha_day <- ifelse(ha_day_temp > hcap, hcap, ha_day_temp)
                         }

#######################          -- tair day summary

t_air_max_day  <- max(temp_all_values$t_air)
t_air_mean_day <- mean(temp_all_values$t_air)
t_air_min_day <- min(temp_all_values$t_air)

#######################          -- lat and lon

lat_on_day <- temp_all_values$Lat[1]
lon_on_day <- temp_all_values$Lon[1]

#######################          -- day

day_on_day <- temp_all_values$day[1]
month_on_day <- temp_all_values$month[1]
year_on_day <- temp_all_values$year[1]

#######################          -- locality

one_locality_name <- temp_all_values$locality[1]

#######################          row with infor per day

all_bind <- data.frame(species = temp_all_values$species[1], diel = diel, locality = one_locality_name, Lat = lat_on_day, Lon = lon_on_day, day = day_on_day,
                       month = month_on_day, year = year_on_day, day_month_year = temp_all_values$day_month_year[1],
                       ha = ha_day, hr = hr_day, t_air_mean = t_air_mean_day, t_air_max = t_air_max_day, t_air_min = t_air_min_day, stringsAsFactors = FALSE)

richards_data_point_day_list[[i]] <-all_bind

}

##################### end of loop for each day

# from list to data frame

hahrdf_raw<-do.call(rbind.data.frame, richards_data_point_day_list)
hahrdf <- hahrdf_raw[with(hahrdf_raw, order(year, month, day)), ]

# place on list

locality_HOBO_list[[z]] <- hahrdf

                                      }

##################### end of loop for locality

hahrdf_combined_raw <-do.call(rbind, locality_HOBO_list)
hahrdf_combined <- hahrdf_combined_raw[complete.cases(hahrdf_combined_raw),]

######## write data frame

write.table(hahrdf_combined, file=paste0(unique(hahrdf_combined$species), "_HOBO_derived_ha_hr_for_Richards.txt"), sep="\t",  row.names = FALSE)

# return list

setwd(master_directory)
return(hahrdf_combined)

                                       }

##########################               END OF FUNCTION               ###################


#############################################################################################################################
#RichHOBO_FlexParamCurve ####################################################################################################
#############################################################################################################################

# installing a working version of FlexParamCurve should be "1.5-2"
# make sure that you use the FlexParamCurve_1.5-2 which is not the latest and the new version is broken

# if installed "FlexParamCurve"; uninstall the package
# packageVersion("FlexParamCurve")
# remove.packages(pkgs = "FlexParamCurve")

# require(devtools)
# install_version("FlexParamCurve", version = "1.5-2", repos = "http://cran.us.r-project.org")
# packageVersion("FlexParamCurve")

# curve from another, producing:

# ha_emp ~ (a / ( 1 + b*exp(-c* tmax_tpref_selected))^(1/d))
#      y ~  A / ([1 + m exp(-k* (     t-i)         )]^(1/m))                    + A' / ([1+ m' exp(-k' (t-i' ))]^(1/m') ),

# Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M))


# where A=Asym , k  =K, i=Infl, m =M,
#       A'=RAsym, k'=Rk, i'=Ri , m'=RM; are argument of the negative Richards should be ignored
#                                       if force4par

#                     a = A = Asym   ---- asymptote patameter
#                     c = k = K      ---- rate parameter
#    no-present in Barrys i = Infl   ---- inflection point parameter
#                 b = d = m = M      ---- shape parameter


## modpar

# http://www.inside-r.org/packages/cran/FlexParamCurve/docs/modpar

# x -- tmax_tupperpref
# y -- ha 
# first.y -- the value of y at minimum x when it is required to be constrained
# x.at.first.y -- the final value of x - 0 value is used if not specified when last.y is not NA
# last.y -- the value of y at maximum x when it is required to be constrained
# x.at.last.y -- the final value of x - must be specified if last.y is not NA
# force4par -- logical specifying whether parameters of the negative Richards should be ignored - effectively
#              using simple Richards curve
# taper.ends -- numeric representing the proportion of the range of the x variable for which data
#               are extended at the two ends of the data set. This is used in initial estimation (prior to optim and
#               nls optimizations) and can speed up subsequent optimizations by imposing a more pronounced S-shape to
#               both first and second curves. Defaults to 0.45.
# width.bounds -- a numeric indicating the proportion of the usual width of parameter bounds to
#                 be imposed during optimizations.

##########################################################################################
##############     Estimate Richards coefficients FlexParamCurve      ####################
##########################################################################################


RichHOBO_FlexParamCurve <- function (HOBOdf_ha_hr_value,
                                            t_air_var_selected_value,
                                            Tupr_value,
                                            remove_excess_of_0_logical_value = FALSE,
                                            round_tolerance_for_excess_0_value = 0,
                                            locality_value = 'all',
                                            months_selected_value = 1:12,
                                            upper_quantile_value = 1,
                                            lower_quantile_value = 0,
                                            size_sample_value = 'all',
                                            species_name_value,
                                            path_output_dir_value) {

# check version of  FlexParamCurve
require('geosphere')
require('FlexParamCurve')
require('utils')
FlexParamCurve_version <- as.character(packageVersion("FlexParamCurve"))

if(FlexParamCurve_version == "1.5.2") {
                                       cat("----- you have the correct FlexParamCurve version: ", FlexParamCurve_version, " -----\n\n")
                                      } else {
                                       cat("----- you have the incorrect FlexParamCurve version: ", FlexParamCurve_version, " it should be 1.5.2 -----\n")
                                       cat("Please follow these instructions to install the correct version: \n")
                                       cat("require(devtools)\n")
                                       cat("remove.packages(pkgs = \"FlexParamCurve\")\n")
                                       cat("install_version(\"FlexParamCurve\", version = \"1.5-2\", repos = \"http://cran.us.r-project.org\n")
                                       cat("add and extra parenthesis to install_version above\n")
                                       stop("-----------------------------\n\n")}

### user input 

HOBOdf_ha_hr <- HOBOdf_ha_hr_value
t_air_var_selected <- t_air_var_selected_value
Tupr <- Tupr_value
remove_excess_of_0_logical <- remove_excess_of_0_logical_value
local <- locality_value
months_selected <- months_selected_value
upper_quantile <- upper_quantile_value
lower_quantile <- lower_quantile_value
round_tolerance <- round_tolerance_for_excess_0_value
size_sample <- size_sample_value
name_species <- species_name_value
path_output_dir <- path_output_dir_value

master_directory <- getwd()

# dir.create

if(is.null(path_output_dir) == FALSE) {
                              dir.create(path_output_dir, showWarnings = F)
                              setwd(path_output_dir)
                                    }

# read HOBO file if not data frame

if(class(HOBOdf_ha_hr) == "data.frame") {
                             HOBO_ha_hr_raw <- HOBOdf_ha_hr
                                   } else {
if(grepl(pattern = "*.csv$", HOBOdf_ha_hr) == TRUE) {HOBO_ha_hr_raw <- read.table(file = HOBOdf_ha_hr, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", HOBOdf_ha_hr) == TRUE) {HOBO_ha_hr_raw <- read.table(file = HOBOdf_ha_hr, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

## read raw data with lon-lat, ha, hr, tmax_tupperpref if indivual files in single_file

raw_data_dataloggers_species <- HOBO_ha_hr_raw

# remove row with NA in ha and hr

## hr

completeVec <- complete.cases(raw_data_dataloggers_species[, "hr"])
raw_data_dataloggers_species <- raw_data_dataloggers_species[completeVec, ]

## ha

completeVec <- complete.cases(raw_data_dataloggers_species[, "ha"])
raw_data_dataloggers_species <- raw_data_dataloggers_species[completeVec, ]

## if excess of 0's remove 

if(remove_excess_of_0_logical == TRUE) {

tt1 <- raw_data_dataloggers_species[!(raw_data_dataloggers_species$hr == 0),]
zz1 <- raw_data_dataloggers_species[(raw_data_dataloggers_species$hr == 0 & raw_data_dataloggers_species$ha > 0),]
oo3 <- raw_data_dataloggers_species[(raw_data_dataloggers_species$hr == 0 & raw_data_dataloggers_species$ha == 0),]
oo3$round <- round(oo3$t_air_max, digits = 1)
oo4 <- !duplicated(oo3$round)
oo5 <- oo3[oo4,]
max_cutoff <- (-1)* 1.2* max(raw_data_dataloggers_species$t_air_max)
oo6 <- oo5[oo5$t_air_max > max_cutoff,]
oo7 <- subset(oo6, select = -c(round) )

raw_data_dataloggers_species <- rbind(tt1,zz1,oo7)

                                       }

# subset locality of interest

if (!any(local == 'all')) {
raw_data_dataloggers_all <- subset (raw_data_dataloggers_species, locality %in% local)
                           } else { raw_data_dataloggers_all <- raw_data_dataloggers_species}

#############################    select months

raw_data_dataloggers <- subset (raw_data_dataloggers_all, month %in% months_selected)

####        get upper and lower bound for a -- theta 1 -- hours of ligth        ##########

# get mean light hours for months collected

lat_seq <- unique(raw_data_dataloggers$Lat) 

mean_lenght_months_lat_loc <- list ()

for (i in 1: length(lat_seq)) {

jan <- lapply(lat_seq[i], daylength, 1:31)
feb <- lapply(lat_seq[i], daylength, 32:59)
mar <- lapply(lat_seq[i], daylength, 60:90)
apr <- lapply(lat_seq[i], daylength, 91:120)
may <- lapply(lat_seq[i], daylength, 121:151)
jun <- lapply(lat_seq[i], daylength, 152:181)
jul <- lapply(lat_seq[i], daylength, 182:212)
aug <- lapply(lat_seq[i], daylength, 213:243)
sep <- lapply(lat_seq[i], daylength, 244:273)
oct <- lapply(lat_seq[i], daylength, 274:304)
nov <- lapply(lat_seq[i], daylength, 305:334)
dec <- lapply(lat_seq[i], daylength, 335:365)

jan_mean <- unique(round(unlist(lapply(lapply(jan,mean), '[[', 1)),3))
feb_mean <- unique(round(unlist(lapply(lapply(feb,mean), '[[', 1)),3))
mar_mean <- unique(round(unlist(lapply(lapply(mar,mean), '[[', 1)),3))
apr_mean <- unique(round(unlist(lapply(lapply(apr,mean), '[[', 1)),3))
may_mean <- unique(round(unlist(lapply(lapply(may,mean), '[[', 1)),3))
jun_mean <- unique(round(unlist(lapply(lapply(jun,mean), '[[', 1)),3))
jul_mean <- unique(round(unlist(lapply(lapply(jul,mean), '[[', 1)),3))
aug_mean <- unique(round(unlist(lapply(lapply(aug,mean), '[[', 1)),3))
sep_mean <- unique(round(unlist(lapply(lapply(sep,mean), '[[', 1)),3))
oct_mean <- unique(round(unlist(lapply(lapply(oct,mean), '[[', 1)),3))
nov_mean <- unique(round(unlist(lapply(lapply(nov,mean), '[[', 1)),3))
dec_mean <- unique(round(unlist(lapply(lapply(dec,mean), '[[', 1)),3))

all_months_mean <- c(jan_mean, feb_mean, mar_mean, apr_mean, may_mean, jun_mean, 
                     jul_mean, aug_mean, sep_mean, oct_mean, nov_mean, dec_mean)

mean_lenght_months_lat_loc [[i]] <- all_months_mean

                              }

mean_lenght_months_lat_loc_df <- data.frame(matrix(unlist(mean_lenght_months_lat_loc), nrow=length(lat_seq), byrow=T),stringsAsFactors=FALSE)
mean_per_column_months_lat_loc_df <- colMeans(mean_lenght_months_lat_loc_df)
names(mean_per_column_months_lat_loc_df) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# select months to to get upper and lower a-value

months_collected <- as.character (unique(raw_data_dataloggers$month))

# get matching months values 

mean_hours_ligth <- list()

if(any(months_collected == 1)) {
                           mean_hours_ligth [[1]] <- mean_per_column_months_lat_loc_df[1] } else { mean_hours_ligth [[1]] <- 0 }
if(any(months_collected == 2)) {
                           mean_hours_ligth [[2]] <- mean_per_column_months_lat_loc_df[2] } else { mean_hours_ligth [[2]] <- 0 }
if(any(months_collected == 3)) {
                           mean_hours_ligth [[3]] <- mean_per_column_months_lat_loc_df[3] } else { mean_hours_ligth [[3]] <- 0 }
if(any(months_collected == 4)) {
                           mean_hours_ligth [[4]] <- mean_per_column_months_lat_loc_df[4] } else { mean_hours_ligth [[4]] <- 0 }
if(any(months_collected == 5)) {
                           mean_hours_ligth [[5]] <- mean_per_column_months_lat_loc_df[5] } else { mean_hours_ligth [[5]] <- 0 }
if(any(months_collected == 6)) {
                           mean_hours_ligth [[6]] <- mean_per_column_months_lat_loc_df[6] } else { mean_hours_ligth [[6]] <- 0 }
if(any(months_collected == 7)) {
                           mean_hours_ligth [[7]] <- mean_per_column_months_lat_loc_df[7] } else { mean_hours_ligth [[7]] <- 0 }
if(any(months_collected == 8)) {
                           mean_hours_ligth [[8]] <- mean_per_column_months_lat_loc_df[8] } else { mean_hours_ligth [[8]] <- 0 }
if(any(months_collected == 9)) {
                           mean_hours_ligth [[9]] <- mean_per_column_months_lat_loc_df[9] } else { mean_hours_ligth [[9]] <- 0 }
if(any(months_collected == 10)) {
                           mean_hours_ligth [[10]] <- mean_per_column_months_lat_loc_df[10] } else { mean_hours_ligth [[10]] <- 0 }
if(any(months_collected == 11)) {
                           mean_hours_ligth [[11]] <- mean_per_column_months_lat_loc_df[11] } else { mean_hours_ligth [[11]] <- 0 }
if(any(months_collected == 12)) {
                           mean_hours_ligth [[12]] <- mean_per_column_months_lat_loc_df[12] } else { mean_hours_ligth [[12]] <- 0 }


mean_hours_ligth_df <- data.frame(matrix(unlist(mean_hours_ligth), nrow=12, byrow=T),stringsAsFactors=FALSE)
names(mean_hours_ligth_df) <- "mean_month_hours_light"

mean_hours_ligth_df_non_zero <- subset (mean_hours_ligth_df, mean_month_hours_light > 0)

cat("\n------- Hours of light for all localities included ----------------\n")
print(mean_hours_ligth_df_non_zero)
cat("\n------------------------------------------------------------------------\n")

###############   Before_remove repeated common names for variables to vector for cleaning        ###############

if (t_air_var_selected %in% names(raw_data_dataloggers)) {
                                                    tmax_tpref_var_pre <- as.vector(t(subset (raw_data_dataloggers, select = t_air_var_selected)))
                                                            } else {
                                                        stop(print("t_air_var_selected not present in Richards input"))
                                                                   }

if (all(is.na(tmax_tpref_var_pre))) {
                                                        stop(print("t_air_var_selected not present in Richards input"))
                                                                   }

#################################      remove repeated entries      ###############################

raw_data_dataloggers$selected_tmax_tpref_var <- round(tmax_tpref_var_pre, digits = 4)

remove_repeated_entries <- paste(raw_data_dataloggers$selected_tmax_tpref_var, "_", raw_data_dataloggers$ha, "_", raw_data_dataloggers$hr, sep="")
raw_data_dataloggers$repeated_entries <- remove_repeated_entries

raw_data_dataloggers_non_duplicated <- raw_data_dataloggers[!duplicated(raw_data_dataloggers[,"repeated_entries"]),]

###############   After_remove repeated common names for variables to vector for cleaning        ###############

if (t_air_var_selected %in% names(raw_data_dataloggers_non_duplicated)) {
                                                    tmax_tpref_var <- as.vector(t(subset (raw_data_dataloggers_non_duplicated, select = t_air_var_selected)))
                                                            } else {
                                                        stop(print("tmax_tpref variable not present in Richards input"))
                                                                   }

if (all(is.na(t_air_var_selected))) {
                                                        stop(print("tmax_tpref variable not present in Richards input"))
                                                                   }

#################################      remove outliers      ###############################

remove_outliers <- function(x, upper=upper_quantile, lower=lower_quantile,  na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(lower, upper), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# decide  

tmax_tupperpref_clean <- remove_outliers(tmax_tpref_var)
raw_data_dataloggers_non_duplicated$tmax_tupperpref_clean <- tmax_tupperpref_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_non_duplicated[complete.cases(raw_data_dataloggers_non_duplicated$tmax_tupperpref_clean),]

# clean ha

ha_clean <- remove_outliers(raw_data_dataloggers_clean$ha)
raw_data_dataloggers_clean$ha_clean <- ha_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_clean[complete.cases(raw_data_dataloggers_clean$ha_clean),]

# clean hr

hr_clean <- remove_outliers(raw_data_dataloggers_clean$hr)
raw_data_dataloggers_clean$hr_clean <- hr_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_clean[complete.cases(raw_data_dataloggers_clean$hr_clean),]

#######################         resample function            #############################

if (!size_sample == 'all') {
               raw_data_dataloggers_clean <- raw_data_dataloggers_clean[sample(nrow(raw_data_dataloggers_clean), size_sample), ]
                            } 

#######################      rename select variable          #############################

raw_data_dataloggers_clean$tmax_tpref_selected <- raw_data_dataloggers_clean$tmax_tupperpref_clean

##########################################################################################

 par(mfrow=c(2,2))

 plot(y = raw_data_dataloggers_non_duplicated$ha, x = tmax_tpref_var)
 plot(y = raw_data_dataloggers_non_duplicated$hr, x = tmax_tpref_var)
 plot(ha~tmax_tpref_selected, data=raw_data_dataloggers_clean)
 plot(hr~tmax_tpref_selected, data=raw_data_dataloggers_clean)

plot_dots <- raw_data_dataloggers_clean

cat("\n----------------------ha -- Richards FlexParamCurve --------------------------------\n")

##########################################################################################
#####################      ha -- Richards -- FlexParamCurve     ##########################

## a -- A -- Asym -- theta 1 bounds

a_upper <- max(mean_hours_ligth_df_non_zero)
a_lower <- min(mean_hours_ligth_df_non_zero)

a_mean <- mean(mean_hours_ligth_df_non_zero$mean_month_hours_light)

######################         ha -- FlexParamCurve --          ##########################

modpar(x = raw_data_dataloggers_clean$tmax_tpref_selected, 
       y = raw_data_dataloggers_clean$ha, 
       first.y = min(raw_data_dataloggers_clean$ha), 
       x.at.first.y = min(raw_data_dataloggers_clean$tmax_tpref_selected),
       last.y = a_upper,
       x.at.last.y = max(raw_data_dataloggers_clean$tmax_tpref_selected),
       force4par = TRUE, 
       taper.ends = 0.45,
       width.bounds = 1,
       pn.options = "ha_options")


############################### ha nls estimation ########################################
# http://www.inside-r.org/packages/cran/FlexParamCurve/docs/SSposnegRichards


ha_nls_mod20 <- try(nls(ha ~ SSposnegRichards(tmax_tpref_selected,
                                                               Asym = Asym,
                                                                  K = K,
                                                               Infl = Infl,
                                                              modno = 20, 
                                                         pn.options = "ha_options"), data = raw_data_dataloggers_clean))

out_fitModel_nls_ha <-capture.output(ha_nls_mod20)

if ("[1] \"try-error\"" %in% out_fitModel_nls_ha)     { 

richards_ha_nls_coefficients <- as.data.frame(c("--", "--", "--", "--"))
row.names(richards_ha_nls_coefficients) <- c("a_Asym", "b_d_M", "c_K", "i_Infl")
names(richards_ha_nls_coefficients) <- "ha_Richards_FlexParamCurve_nls"

                                        } else {

richards_ha_nls_coefficients_Asym_K_Infl <- coef(ha_nls_mod20)
richards_ha_nls_coefficients_M <- ha_options$M
richards_ha_nls_coefficients <- as.data.frame(c(richards_ha_nls_coefficients_Asym_K_Infl, richards_ha_nls_coefficients_M))
row.names(richards_ha_nls_coefficients) <- c("a_Asym", "c_K", "i_Infl", "b_d_M")
names(richards_ha_nls_coefficients) <- "ha_Richards_FlexParamCurve_nls"

print(richards_ha_nls_coefficients)

summary_fitModel_nls_ha <-capture.output(summary(ha_nls_mod20))

}

cat("\n------------------------------------------------------------------------------------\n")


##########################################################################################
#################################      hr -- Richards      ###############################

cat("\n----------------------hr -- Richards FlexParamCurve --------------------------------\n")

##################       remove extra 0's for hr-determination       #####################

if(remove_excess_of_0_logical == TRUE) {

hr_1 <- raw_data_dataloggers_clean[!(raw_data_dataloggers_clean$hr == 0),]

hr_2 <- raw_data_dataloggers_clean[(raw_data_dataloggers_clean$hr == 0 & raw_data_dataloggers_clean$ha == 0),]

hr_3 <- raw_data_dataloggers_clean[(raw_data_dataloggers_clean$hr == 0 & raw_data_dataloggers_clean$ha > 0),]
hr_3$round <- round(hr_3$tmax_tpref_selected, digits = round_tolerance)
hr_4 <- !duplicated(hr_3$round)
hr_5 <- hr_3[hr_4,]
hr_6 <- subset(hr_5, select = -c(round) )

raw_data_dataloggers_clean <- rbind(hr_1,hr_2,hr_6)

                                       }


## a -- theta 1 bounds

a_upper <- max(mean_hours_ligth_df_non_zero)
a_lower <- min(mean_hours_ligth_df_non_zero)

a_mean <- mean(mean_hours_ligth_df_non_zero$mean_month_hours_light)

######################         hr -- FlexParamCurve --          ##########################

modpar(x = raw_data_dataloggers_clean$tmax_tpref_selected, 
       y = raw_data_dataloggers_clean$hr, 
       first.y = min(raw_data_dataloggers_clean$hr), 
       x.at.first.y = min(raw_data_dataloggers_clean$tmax_tpref_selected),
       last.y = a_upper,
       x.at.last.y = max(raw_data_dataloggers_clean$tmax_tpref_selected),
       force4par = TRUE, 
       taper.ends = 0.45,
       width.bounds = 2,
       pn.options = "hr_options")


############################### hr nls estimation ########################################
# http://www.inside-r.org/packages/cran/FlexParamCurve/docs/SSposnegRichards


hr_nls_mod20 <- try(nls(hr ~ SSposnegRichards(tmax_tpref_selected,
                                                               Asym = Asym,
                                                                  K = K,
                                                               Infl = Infl,
                                                              modno = 20, 
                                                         pn.options = "hr_options"), data = raw_data_dataloggers_clean))

out_fitModel_nls_hr <-capture.output(hr_nls_mod20)

if ("[1] \"try-error\"" %in% out_fitModel_nls_hr)     { 

richards_hr_nls_coefficients <- as.data.frame(c("--", "--", "--", "--"))
row.names(richards_hr_nls_coefficients) <- c("a_Asym", "b_d_M", "c_K", "i_Infl")
names(richards_hr_nls_coefficients) <- "hr_Richards_FlexParamCurve_nls"

                                        } else {

richards_hr_nls_coefficients_Asym_K_Infl <- coef(hr_nls_mod20)
richards_hr_nls_coefficients_M <- hr_options$M
richards_hr_nls_coefficients <- as.data.frame(c(richards_hr_nls_coefficients_Asym_K_Infl, richards_hr_nls_coefficients_M))
row.names(richards_hr_nls_coefficients) <- c("a_Asym", "c_K", "i_Infl", "b_d_M")
names(richards_hr_nls_coefficients) <- "hr_Richards_FlexParamCurve_nls"

print(richards_hr_nls_coefficients)

summary_fitModel_nls_hr <-capture.output(summary(hr_nls_mod20))

}

cat("\n------------------------------------------------------------------------------------\n")

#################################    summary of coefficients      ########################

ha_hr_FlexParamCurve_df <- cbind(richards_ha_nls_coefficients, 
                                 richards_hr_nls_coefficients)

print(ha_hr_FlexParamCurve_df)

cat("\n------------------------------------------------------------------------------------\n")

#########################     write summary of coefficients      #########################

local_names <- paste(local, collapse = '_')

write.table(ha_hr_FlexParamCurve_df, file = paste(name_species,"_", local_names, "_","sample_", size_sample, "_", t_air_var_selected, "_Richards_FlexParamCurve_coefficients.txt", sep = ""), sep="\t")

if (!"[1] \"try-error\"" %in% out_fitModel_nls_ha)     {
writeLines(summary_fitModel_nls_ha,con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_ha_Richards_FlexParamCurve_summary.txt", sep=""))
}

if (!"[1] \"try-error\"" %in% out_fitModel_nls_hr)     {
writeLines(summary_fitModel_nls_hr,con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_hr_Richards_FlexParamCurve_summary.txt", sep=""))
}


#################################    ha hr -- plots --      ##############################

# visualize plot

tmax_tupperpref_simulated <- data.frame(tmax_tupperpref = seq(min(raw_data_dataloggers_clean$tmax_tpref_selected),
                                                              max(raw_data_dataloggers_clean$tmax_tpref_selected),
                                                       len=length(raw_data_dataloggers_clean$tmax_tpref_selected)))
names(tmax_tupperpref_simulated) <- "tmax_tpref_selected" 

# plot

par(mfrow=c(1,2))
plot(ha~tmax_tpref_selected, data=plot_dots)
if (!"[1] \"try-error\"" %in% out_fitModel_nls_ha)     {
lines(y=predict(ha_nls_mod20,newdata=tmax_tupperpref_simulated), x=tmax_tupperpref_simulated$tmax_tpref_selected, col="red", lwd=2)
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,2.5, "ha_curve_FlexParamCurve", cex=1, pos=4, col = "red")
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,1.5, "temperature C", cex=1, pos=4, col = "red")
                                                        }
mtext(paste(name_species, "_ha_Richards_FlexParamCurve_curve", sep = ""), outer = F, cex = 1)

plot(hr~tmax_tpref_selected, data=plot_dots)
if (!"[1] \"try-error\"" %in% out_fitModel_nls_hr)     {
lines(y=predict(hr_nls_mod20,newdata=tmax_tupperpref_simulated), x=tmax_tupperpref_simulated$tmax_tpref_selected, col="blue", lwd=2)
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,2, "hr_curve_FlexParamCurve", cex=1, pos=4, col = "blue")
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,1.5, "temperature C", cex=1, pos=4, col = "blue")
                                                        }
mtext(paste(name_species, "_hr_Richards_FlexParamCurve_curve", sep = ""), outer = F, cex = 1)


pdf( paste(name_species,"_", local_names, "_","sample_", size_sample,"_", t_air_var_selected,"_ha_hr_Richards_FlexParamCurve_curve.pdf", sep =""), width = 11, height = 6 )
par(mfrow=c(1,2))
plot(ha~tmax_tpref_selected, data=plot_dots)
if (!"[1] \"try-error\"" %in% out_fitModel_nls_ha)     {
lines(y=predict(ha_nls_mod20,newdata=tmax_tupperpref_simulated), x=tmax_tupperpref_simulated$tmax_tpref_selected, col="red", lwd=2)
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,2.5, "ha_curve_FlexParamCurve", cex=1, pos=4, col = "red")
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,1.5, "temperature C", cex=1, pos=4, col = "red")
                                                        }
mtext(paste(name_species, "_ha_Richards_FlexParamCurve_curve", sep = ""), outer = F, cex = 1)

plot(hr~tmax_tpref_selected, data=plot_dots)
if (!"[1] \"try-error\"" %in% out_fitModel_nls_hr)     {
lines(y=predict(hr_nls_mod20,newdata=tmax_tupperpref_simulated), x=tmax_tupperpref_simulated$tmax_tpref_selected, col="blue", lwd=2)
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,2, "hr_curve_FlexParamCurve", cex=1, pos=4, col = "blue")
text(mean(tmax_tupperpref_simulated$tmax_tpref_selected) ,1.5, "temperature C", cex=1, pos=4, col = "blue")
                                                        }
mtext(paste(name_species, "_hr_Richards_FlexParamCurve_curve", sep = ""), outer = F, cex = 1)
dev.off()

# return to master directory

setwd(master_directory)

# make list that is compatible with RichHOBO_bbmle

hr_Asym <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[1]) == FALSE, NA, ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[1])
hr_K <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[2]) == FALSE, NA, ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[2])
hr_M <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[4]) == FALSE, NA, ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[4])
hr_Infl <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[3]) == FALSE, NA, ha_hr_FlexParamCurve_df$hr_Richards_FlexParamCurve_nls[3])

ha_Asym <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[1]) == FALSE, NA, ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[1])
ha_K <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[2]) == FALSE, NA, ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[2])
ha_M <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[4]) == FALSE, NA, ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[4])
ha_Infl <- ifelse(is.numeric(ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[3]) == FALSE, NA, ha_hr_FlexParamCurve_df$ha_Richards_FlexParamCurve_nls[3])

# construct list

coeflist <- list(hr_Asym=hr_Asym,hr_K=hr_K,hr_M=hr_M,hr_Infl=hr_Infl,ha_Asym=ha_Asym,ha_K=ha_K,ha_M=ha_M,ha_Infl=ha_Infl,Tupr=Tupr, estimator = "FlexParamCurve")

# return selected layers

return(coeflist)

}

##############                          END of function                       ############

#############################################################################################################################
#RichHOBO_bbmle_JCS  ########################################################################################################
#############################################################################################################################

RichHOBO_bbmle_JCS <- function (HOBOdf_ha_hr_value,
                                            t_air_var_selected_value,
                                            hcap_value = NULL,
                                            Tupr_value,
                                            remove_excess_of_0_logical_value = FALSE,
                                            round_tolerance_for_excess_0_value = 0,
                                            locality_value = 'all',
                                            months_selected_value = 1:12,
                                            upper_quantile_value = 1,
                                            lower_quantile_value = 0,
                                            size_sample_value = 'all',
                                            species_name_value,
                                            variables_value =c('ha','hr'),
                                            path_output_dir_value) {

# requires 

require('bbmle')
require('geosphere')

### user input 

HOBOdf_ha_hr <- HOBOdf_ha_hr_value
t_air_var_selected <- t_air_var_selected_value
hcap <- hcap_value
Tupr <- Tupr_value
remove_excess_of_0_logical <- remove_excess_of_0_logical_value
local <- locality_value
months_selected <- months_selected_value
upper_quantile <- upper_quantile_value
lower_quantile <- lower_quantile_value
round_tolerance <- round_tolerance_for_excess_0_value
size_sample <- size_sample_value
name_species <- species_name_value
variables <- variables_value
path_output_dir <- path_output_dir_value

master_directory <- getwd()

# dir.create

if(is.null(path_output_dir) == FALSE) {
                              dir.create(path_output_dir, showWarnings = F)
                              setwd(path_output_dir)
                                    }

# read HOBO file if not data frame

if(class(HOBOdf_ha_hr) == "data.frame") {
                             HOBO_ha_hr_raw <- HOBOdf_ha_hr
                                   } else {
if(grepl(pattern = "*.csv$", HOBOdf_ha_hr) == TRUE) {HOBO_ha_hr_raw <- read.table(file = HOBOdf_ha_hr, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", HOBOdf_ha_hr) == TRUE) {HOBO_ha_hr_raw <- read.table(file = HOBOdf_ha_hr, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

## read raw data with lon-lat, ha, hr, tmax_tupperpref if indivual files in single_file

raw_data_dataloggers_species <- HOBO_ha_hr_raw

# remove row with NA in ha and hr

## hr

completeVec <- complete.cases(raw_data_dataloggers_species[, "hr"])
raw_data_dataloggers_species <- raw_data_dataloggers_species[completeVec, ]

## ha

completeVec <- complete.cases(raw_data_dataloggers_species[, "ha"])
raw_data_dataloggers_species <- raw_data_dataloggers_species[completeVec, ]

## if excess of 0's remove 

if(remove_excess_of_0_logical == TRUE) {

tt1 <- raw_data_dataloggers_species[!(raw_data_dataloggers_species$hr == 0),]
zz1 <- raw_data_dataloggers_species[(raw_data_dataloggers_species$hr == 0 & raw_data_dataloggers_species$ha > 0),]
oo3 <- raw_data_dataloggers_species[(raw_data_dataloggers_species$hr == 0 & raw_data_dataloggers_species$ha == 0),]
oo3$round <- round(oo3$t_air_max, digits = 1)
oo4 <- !duplicated(oo3$round)
oo5 <- oo3[oo4,]
max_cutoff <- (-1)* 1.2* max(raw_data_dataloggers_species$t_air_max)
oo6 <- oo5[oo5$t_air_max > max_cutoff,]
oo7 <- subset(oo6, select = -c(round) )

raw_data_dataloggers_species <- rbind(tt1,zz1,oo7)

                                       }

# subset locality of interest

if (!any(local == 'all')) {
raw_data_dataloggers_all <- subset (raw_data_dataloggers_species, locality %in% local)
                           } else { raw_data_dataloggers_all <- raw_data_dataloggers_species}

#############################    select months

raw_data_dataloggers <- subset (raw_data_dataloggers_all, month %in% months_selected)

####        get upper and lower bound for a -- theta 1 -- hours of ligth        ##########

# get mean light hours for months collected

lat_seq <- unique(raw_data_dataloggers$Lat) 

mean_lenght_months_lat_loc <- list ()

for (i in 1: length(lat_seq)) {

jan <- lapply(lat_seq[i], daylength, 1:31)
feb <- lapply(lat_seq[i], daylength, 32:59)
mar <- lapply(lat_seq[i], daylength, 60:90)
apr <- lapply(lat_seq[i], daylength, 91:120)
may <- lapply(lat_seq[i], daylength, 121:151)
jun <- lapply(lat_seq[i], daylength, 152:181)
jul <- lapply(lat_seq[i], daylength, 182:212)
aug <- lapply(lat_seq[i], daylength, 213:243)
sep <- lapply(lat_seq[i], daylength, 244:273)
oct <- lapply(lat_seq[i], daylength, 274:304)
nov <- lapply(lat_seq[i], daylength, 305:334)
dec <- lapply(lat_seq[i], daylength, 335:365)

jan_mean <- unique(round(unlist(lapply(lapply(jan,mean), '[[', 1)),3))
feb_mean <- unique(round(unlist(lapply(lapply(feb,mean), '[[', 1)),3))
mar_mean <- unique(round(unlist(lapply(lapply(mar,mean), '[[', 1)),3))
apr_mean <- unique(round(unlist(lapply(lapply(apr,mean), '[[', 1)),3))
may_mean <- unique(round(unlist(lapply(lapply(may,mean), '[[', 1)),3))
jun_mean <- unique(round(unlist(lapply(lapply(jun,mean), '[[', 1)),3))
jul_mean <- unique(round(unlist(lapply(lapply(jul,mean), '[[', 1)),3))
aug_mean <- unique(round(unlist(lapply(lapply(aug,mean), '[[', 1)),3))
sep_mean <- unique(round(unlist(lapply(lapply(sep,mean), '[[', 1)),3))
oct_mean <- unique(round(unlist(lapply(lapply(oct,mean), '[[', 1)),3))
nov_mean <- unique(round(unlist(lapply(lapply(nov,mean), '[[', 1)),3))
dec_mean <- unique(round(unlist(lapply(lapply(dec,mean), '[[', 1)),3))

all_months_mean <- c(jan_mean, feb_mean, mar_mean, apr_mean, may_mean, jun_mean, 
                     jul_mean, aug_mean, sep_mean, oct_mean, nov_mean, dec_mean)

mean_lenght_months_lat_loc [[i]] <- all_months_mean

                              }

mean_lenght_months_lat_loc_df <- data.frame(matrix(unlist(mean_lenght_months_lat_loc), nrow=length(lat_seq), byrow=T),stringsAsFactors=FALSE)
mean_per_column_months_lat_loc_df <- colMeans(mean_lenght_months_lat_loc_df)
names(mean_per_column_months_lat_loc_df) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# select months to to get upper and lower a-value

months_collected <- as.character (unique(raw_data_dataloggers$month))

# get matching months values 

mean_hours_ligth <- list()

if(any(months_collected == 1)) {
                           mean_hours_ligth [[1]] <- mean_per_column_months_lat_loc_df[1] } else { mean_hours_ligth [[1]] <- 0 }
if(any(months_collected == 2)) {
                           mean_hours_ligth [[2]] <- mean_per_column_months_lat_loc_df[2] } else { mean_hours_ligth [[2]] <- 0 }
if(any(months_collected == 3)) {
                           mean_hours_ligth [[3]] <- mean_per_column_months_lat_loc_df[3] } else { mean_hours_ligth [[3]] <- 0 }
if(any(months_collected == 4)) {
                           mean_hours_ligth [[4]] <- mean_per_column_months_lat_loc_df[4] } else { mean_hours_ligth [[4]] <- 0 }
if(any(months_collected == 5)) {
                           mean_hours_ligth [[5]] <- mean_per_column_months_lat_loc_df[5] } else { mean_hours_ligth [[5]] <- 0 }
if(any(months_collected == 6)) {
                           mean_hours_ligth [[6]] <- mean_per_column_months_lat_loc_df[6] } else { mean_hours_ligth [[6]] <- 0 }
if(any(months_collected == 7)) {
                           mean_hours_ligth [[7]] <- mean_per_column_months_lat_loc_df[7] } else { mean_hours_ligth [[7]] <- 0 }
if(any(months_collected == 8)) {
                           mean_hours_ligth [[8]] <- mean_per_column_months_lat_loc_df[8] } else { mean_hours_ligth [[8]] <- 0 }
if(any(months_collected == 9)) {
                           mean_hours_ligth [[9]] <- mean_per_column_months_lat_loc_df[9] } else { mean_hours_ligth [[9]] <- 0 }
if(any(months_collected == 10)) {
                           mean_hours_ligth [[10]] <- mean_per_column_months_lat_loc_df[10] } else { mean_hours_ligth [[10]] <- 0 }
if(any(months_collected == 11)) {
                           mean_hours_ligth [[11]] <- mean_per_column_months_lat_loc_df[11] } else { mean_hours_ligth [[11]] <- 0 }
if(any(months_collected == 12)) {
                           mean_hours_ligth [[12]] <- mean_per_column_months_lat_loc_df[12] } else { mean_hours_ligth [[12]] <- 0 }


mean_hours_ligth_df <- data.frame(matrix(unlist(mean_hours_ligth), nrow=12, byrow=T),stringsAsFactors=FALSE)
names(mean_hours_ligth_df) <- "mean_month_hours_light"

mean_hours_ligth_df_non_zero <- subset (mean_hours_ligth_df, mean_month_hours_light > 0)

# cat("\n------- Hours of light for all localities included ----------------\n")
# print(mean_hours_ligth_df_non_zero)
# cat("\n------------------------------------------------------------------------\n")

###############   Before_remove repeated common names for variables to vector for cleaning        ###############

if (t_air_var_selected %in% names(raw_data_dataloggers)) {
                                                    tmax_tpref_var_pre <- as.vector(t(subset (raw_data_dataloggers, select = t_air_var_selected)))
                                                            } else {
                                                        stop(print("t_air_var_selected not present in Richards input"))
                                                                   }

if (all(is.na(tmax_tpref_var_pre))) {
                                                        stop(print("t_air_var_selected not present in Richards input"))
                                                                   }

#################################      remove repeated entries      ###############################

raw_data_dataloggers$selected_tmax_tpref_var <- round(tmax_tpref_var_pre, digits = 4)

remove_repeated_entries <- paste(raw_data_dataloggers$selected_tmax_tpref_var, "_", raw_data_dataloggers$ha, "_", raw_data_dataloggers$hr, sep="")
raw_data_dataloggers$repeated_entries <- remove_repeated_entries

raw_data_dataloggers_non_duplicated <- raw_data_dataloggers[!duplicated(raw_data_dataloggers[,"repeated_entries"]),]

###############   After_remove repeated common names for variables to vector for cleaning        ###############

if (t_air_var_selected %in% names(raw_data_dataloggers_non_duplicated)) {
                                                    tmax_tpref_var <- as.vector(t(subset (raw_data_dataloggers_non_duplicated, select = t_air_var_selected)))
                                                            } else {
                                                        stop(print("tmax_tpref variable not present in Richards input"))
                                                                   }

if (all(is.na(t_air_var_selected))) {
                                                        stop(print("tmax_tpref variable not present in Richards input"))
                                                                   }

#################################      remove outliers      ###############################

remove_outliers <- function(x, upper=upper_quantile, lower=lower_quantile,  na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(lower, upper), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# decide  

tmax_tupperpref_clean <- remove_outliers(tmax_tpref_var)
raw_data_dataloggers_non_duplicated$tmax_tupperpref_clean <- tmax_tupperpref_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_non_duplicated[complete.cases(raw_data_dataloggers_non_duplicated$tmax_tupperpref_clean),]

# clean ha

ha_clean <- remove_outliers(raw_data_dataloggers_clean$ha)
raw_data_dataloggers_clean$ha_clean <- ha_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_clean[complete.cases(raw_data_dataloggers_clean$ha_clean),]

# clean hr

hr_clean <- remove_outliers(raw_data_dataloggers_clean$hr)
raw_data_dataloggers_clean$hr_clean <- hr_clean
raw_data_dataloggers_clean <-raw_data_dataloggers_clean[complete.cases(raw_data_dataloggers_clean$hr_clean),]

#######################         resample function            #############################

if (!size_sample == 'all') {
               raw_data_dataloggers_clean <- raw_data_dataloggers_clean[sample(nrow(raw_data_dataloggers_clean), size_sample), ]
                            } 

#######################      rename select variable          #############################

raw_data_dataloggers_clean$tmax_tpref_selected <- raw_data_dataloggers_clean$tmax_tupperpref_clean

# from list to data frame

hahrdf_pre <- subset(raw_data_dataloggers_clean, select = c(locality, ha, hr))
hahrdf_pre$t_air <- raw_data_dataloggers_clean$tmax_tpref_selected
hahrdf <-hahrdf_pre

##########################################################################################

#Richard's regression

#hr
printatend<-list()
success<-c()

if(is.element('hr',variables) ){
	
#Starting values
Asym<-max(hahrdf$hr)
lmstart<-lm(hahrdf$hr~hahrdf$t_air)
K=lmstart$coefficients[2]
M=1
Infl=min(hahrdf$t_air)+(max(hahrdf$t_air)-min(hahrdf$t_air))/2


# Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M))

if(is.null(hcap)){
mhrtry<-try(suppressWarnings(mle2(hr~dexp(rate=1/(Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M)))),start=list(Asym=Asym,K=K,M=M,Infl=Infl,Tupr=Tupr),data=hahrdf,fixed=list(Tupr=Tupr),lower=list(K=0.0000001),upper=list(Asym=24),method='Nelder-Mead')),silent=TRUE)
if ('try-error' %in% class(mhrtry)) { printatend[length(printatend)+1]<-'Could not fit a positive curve to hr data'
                                                                    
                            } else { mhr  <- mhrtry
hrcoef<-mhr@fullcoef
hrformula<-paste('hr=(',mhr@fullcoef[1],')/((1+(',mhr@fullcoef[2],')*exp(-(',mhr@fullcoef[4],')*(t_air-',Tupr,')))^(1/(',mhr@fullcoef[3],')))',sep='')

success[length(success)+1]<-'hr'

}

} else {

mhrtry<-try(suppressWarnings(mle2(hr~dexp(rate=1/(Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M)))),start=list(Asym=Asym,K=K,M=M,Infl=Infl,Tupr=Tupr),data=hahrdf,fixed=list(Asym=hcap,Tupr=Tupr),lower=list(K=0.0000001),upper=list(Asym=24),method='Nelder-Mead')),silent=TRUE)
if ('try-error' %in% class(mhrtry)) { printatend[length(printatend)+1]<-'Could not fit a positive curve to hr data'
                                                                    
                            } else { mhr  <- mhrtry

hrcoef<-mhr@fullcoef
hrformula<-paste('hr=(',mhr@fullcoef[1],')/((1+(',mhr@fullcoef[2],')*exp(-(',mhr@fullcoef[4],')*(t_air-',Tupr,')))^(1/(',mhr@fullcoef[3],')))',sep='')

success[length(success)+1]<-'hr'

}
}
}

#ha

if(is.element('ha',variables) ){
Asym<-max(hahrdf$ha)
lmstart<-lm(hahrdf$ha~hahrdf$t_air)
K=lmstart$coefficients[2]
M=1
Infl=min(hahrdf$t_air)+(max(hahrdf$t_air)-min(hahrdf$t_air))/2

if(is.null(hcap)){
mhatry<-try(suppressWarnings(mle2(ha~dexp(rate=1/(Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M)))),start=list(Asym=Asym,K=K,M=M,Infl=Infl,Tupr=Tupr),data=hahrdf,fixed=list(Tupr=Tupr),lower=list(K=0.0000001),upper=list(Asym=24),method='Nelder-Mead')),silent=TRUE)
if ('try-error' %in% class(mhatry)) { printatend[length(printatend)+1]<-'Could not fit a positive curve to ha data'
                                                                    
                            } else {mha  <- mhatry
hacoef<-mha@fullcoef
haformula<-paste('ha=(',mha@fullcoef[1],')/((1+(',mha@fullcoef[2],')*exp(-(',mha@fullcoef[4],')*(t_air-',Tupr,')))^(1/(',mha@fullcoef[3],')))',sep='')

success[length(success)+1]<-'ha'

}

} else {
mhatry<-try(suppressWarnings(mle2(ha~dexp(rate=1/(Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M)))),start=list(Asym=Asym,K=K,M=M,Infl=Infl,Tupr=Tupr),data=hahrdf,fixed=list(Asym=hcap,Tupr=Tupr),lower=list(K=0.0000001),upper=list(Asym=24),method='Nelder-Mead')),silent=TRUE)
if ('try-error' %in% class(mhatry)) { printatend[length(printatend)+1]<-'Could not fit a positive curve to ha data'
                                                                    
                            } else {mha  <- mhatry

hacoef<-mha@fullcoef
haformula<-paste('ha=(',mha@fullcoef[1],')/((1+(',mha@fullcoef[2],')*exp(-(',mha@fullcoef[4],')*(t_air-',Tupr,')))^(1/(',mha@fullcoef[3],')))',sep='')

success[length(success)+1]<-'ha'

}

}
}

if(length(success)==0){
	print(printatend)
} else {

if(is.element('hr',success) & is.element('ha',success) ){
summary<-list(ha_hr_by_day=hahrdf,hr_coefficients=hrcoef,hr_formula=hrformula,ha_coefficients=hacoef,ha_formula=haformula)
} else if(is.element('hr',success) ){
summary<-list(ha_hr_by_day=hahrdf,hr_coefficients=hrcoef,hr_formula=hrformula)
} else if(is.element('ha',success) ){
summary<-list(ha_hr_by_day=hahrdf,ha_coefficients=hacoef,ha_formula=haformula)
}


# plot

if(is.element('hr',success) & is.element('ha',success) ){
par(mfrow=c(1,2))
}

if(is.element('hr',success) ){
title<-paste(name_species, "_hr_Richards_curve", sep = "")

Tdelta<-hahrdf$t_air-Tupr

plot(hahrdf$hr~Tdelta, data=hahrdf, main=title,xlab='T_air-Tupr')

hrfunction<-function(t_air,Tupr){
	(hrcoef[1])/((1+hrcoef[2]*exp(-hrcoef[4]*(t_air-Tupr)))^(1/hrcoef[3]))
	}

range_t_air<-c(min(hahrdf$t_air):max(hahrdf$t_air))	

lines(y=hrfunction(range_t_air,Tupr), x=range_t_air-Tupr, col="red", lwd=2)                                                        
}

if(is.element('ha',success) ){
title<-paste(name_species, "_ha_Richards_curve", sep = "")

Tdelta<-hahrdf$t_air-Tupr

plot(hahrdf$ha~Tdelta, data=hahrdf, main=title,xlab='T_air-Tupr')

hafunction<-function(t_air,Tupr){
	(hacoef[1])/((1+hacoef[2]*exp(-hacoef[4]*(t_air-Tupr)))^(1/hacoef[3]))	}

range_t_air<-c(min(hahrdf$t_air):max(hahrdf$t_air))	
	
lines(y=hafunction(range_t_air,Tupr), x=range_t_air-Tupr, col="red", lwd=2)
}
#

if(is.element('hr',success) & is.element('ha',success) ){
coeflist<-list(hr_Asym=hrcoef[1],hr_K=hrcoef[2],hr_M=hrcoef[3],hr_Infl=hrcoef[4],ha_Asym=hacoef[1],ha_K=hacoef[2],ha_M=hacoef[3],ha_Infl=hacoef[4],Tupr=hacoef[5], estimator = "bbmle")
} else if(is.element('hr',success) ){
coeflist<-list(hr_Asym=hrcoef[1],hr_K=hrcoef[2],hr_M=hrcoef[3],hr_Infl=hrcoef[4],ha_Asym=NA,ha_K=NA,ha_M=NA,ha_Infl=NA,Tupr=hrcoef[5], estimator = "bbmle")
} else if(is.element('ha',success) ){
coeflist<-list(hr_Asym=NA,hr_K=NA,hr_M=NA,hr_Infl=NA,  ha_Asym=hacoef[1],ha_K=hacoef[2],ha_M=hacoef[3],ha_Infl=hacoef[4],Tupr=hacoef[5], estimator = "bbmle")
}

print(summary)

}

local_names <- paste(local, collapse = '_')

writeLines(capture.output(summary),con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_hr_Richards_bbmle_summary.txt", sep=""))

# return to master directory

setwd(master_directory)

# return selected coefficients

return(coeflist)

}

##############                          END of function                       ############

#############################################################################################################################
#RichWater###################################################################################################################
#############################################################################################################################

RichWater<-function(EWLdf,
				   ewlcap=NULL){

require('bbmle')
require('lattice')
require('gridExtra')
	# dependencies: stats4

EWLdf$day_month_year <- paste(EWLdf$day, "_", EWLdf$month, "_", EWLdf$year, sep ="")
EWLdf$ewl<-((EWLdf$initial_mass-EWLdf$final_mass)/EWLdf$hydrated_mass)/EWLdf$interval
EWLdf$t_delta<-EWLdf$t_air-EWLdf$t_hobo

#Richard's regression

#hr

#Starting values
Asym<-max(EWLdf$ewl)
lmT<-lm(EWLdf$ewl~EWLdf$t_delta)
lmRH<-lm(EWLdf$ewl~EWLdf$rh)
K1=lmT$coefficients[2]
K2=lmRH$coefficients[2]
M=1
Infl1=min(EWLdf$t_delta)+(max(EWLdf$t_delta)-min(EWLdf$t_delta))/2
Infl2=min(EWLdf$rh)+(max(EWLdf$rh)-min(EWLdf$rh))/2
Kdry=1
Kshd=1

if(is.null(ewlcap)){
mewl<-suppressWarnings(mle2(ewl~dexp(rate=1/(Asym/((1+M*exp(-K1*(t_delta-Infl1)-K2*(rh-Infl2)+Kdry*dry+Kshd*shade))^(1/M)))),start=list(Asym=Asym,K1=K1,M=M,Infl1=Infl1,K2=K2,Infl2=Infl2,Kdry=Kdry,Kshd=Kshd),data=EWLdf,method='Nelder-Mead')) 

} else {
	
mewl<-suppressWarnings(mle2(ewl~dexp(rate=1/(Asym/((1+M*exp(-K1*(t_delta-Infl1)-K2*(rh-Infl2)+Kdry*dry+Kshd*shade))^(1/M)))),start=list(Asym=Asym,K1=K1,M=M,Infl1=Infl1,K2=K2,Infl2=Infl2,Kdry=Kdry,Kshd=Kshd),data=EWLdf,fixed=list(Asym=ewlcap),method='Nelder-Mead'))
}

ewlcoef<-mewl@fullcoef
ewlformula<-paste('ewl=(',mewl@fullcoef[1],')/((1+(',mewl@fullcoef[3],')*exp(-(',mewl@fullcoef[2],')*(t_delta-(',mewl@fullcoef[4],'))-(',mewl@fullcoef[5],')*(rh-(',mewl@fullcoef[6],'))+(',mewl@fullcoef[7],')*dry+(',mewl@fullcoef[8],')*shade))^(1/(',mewl@fullcoef[3],')))',sep='')

summary<-list(ewl_coefficients=ewlcoef,ewl_formula=ewlformula)

# plot

#plots in dry/wet sun/shade 3d

ewlfunction<-function(t_delta,rh,dry,shade){
	(ewlcoef[1])/((1+ewlcoef[3]*exp(-ewlcoef[2]*(t_delta-ewlcoef[4])-ewlcoef[5]*(rh-ewlcoef[6])+ewlcoef[7]*dry+ewlcoef[8]*shade))^(1/ewlcoef[3]))
	}

range_t_delta<-rep(seq(min(EWLdf$t_delta),max(EWLdf$t_delta),length.out=25),each=25)	
range_rh<-rep(seq(min(EWLdf$rh),max(EWLdf$rh),length.out=25),25)
ewl_shade_dry<-ewlfunction(range_t_delta,range_rh,1,1)
ewl_sun_dry<-ewlfunction(range_t_delta,range_rh,1,0)
ewl_shade_wet<-ewlfunction(range_t_delta,range_rh,0,1)
ewl_sun_wet<-ewlfunction(range_t_delta,range_rh,0,0)

grid.arrange(
wireframe(ewl_shade_dry~range_t_delta*range_rh,main="Shade Dry",drape=T,screen=list(z=220,x=-60,y=0),zlab='EWL',ylab='RH',xlab='TDelta'),
wireframe(ewl_sun_dry~range_t_delta*range_rh,main="Sun Dry",drape=T,screen=list(z=220,x=-60,y=0),zlab='EWL',ylab='RH',xlab='TDelta'),
wireframe(ewl_shade_wet~range_t_delta*range_rh,main="Shade Wet",drape=T,screen=list(z=220,x=-60,y=0),zlab='EWL',ylab='RH',xlab='TDelta'),
wireframe(ewl_sun_wet~range_t_delta*range_rh,main="Sun Wet",drape=T,screen=list(z=220,x=-60,y=0),zlab='EWL',ylab='RH',xlab='TDelta')
)

#Regression between HOBO and air temperature

t_regression<-lm(EWLdf$t_hobo~EWLdf$t_air)

coeflist<-list(ewl_Asym=ewlcoef[1],ewl_K1=ewlcoef[2],ewl_M=ewlcoef[3],ewl_Infl1=ewlcoef[4],ewl_K2=ewlcoef[5],ewl_Infl2=ewlcoef[6],Kdry=ewlcoef[7],Kshd=ewlcoef[8],T_intercept=t_regression$coefficients[1],T_slope=t_regression$coefficients[2])

print(summary)
return(coeflist)

}


#############################################################################################################################
#PerfGAMM####################################################################################################################
#############################################################################################################################

PerfGAMM<- function (performance_input,                                    
                                   knots_vector=NA, 
                                   size = TRUE,
                                   acc = FALSE,
                                   hydration = FALSE
                                   ) {

# load required packages

require('mgcv')
	# dependencies: nlme
require("ggplot2")
	#dependencies: grid

# use knots

uni_temp <- if (is.na(knots_vector)) {
                                        uni_temp <- unique(performance_input$round_temp)
                                } else {uni_temp <- knots_vector}

k <- length(uni_temp)

#### plot points measured visual inspection

require(grid)
plot_dispersion_species <- ggplot(performance_input,aes(x= temp,y= performance, color = id)) + 
                                             geom_point(size = 6, alpha = 0.7) + 
                                             theme_bw() +
                                             theme(legend.text = element_text(size = 7),
                                             legend.key.size = unit(0.2,"cm"),
                                             legend.margin = unit(0,"cm"),
                                             panel.grid.major = element_line(colour = "gray"),
                                             panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
                                             geom_rug(col="darkred",alpha=.7)

############################## Correlation schemes by ID #################################

## corAR1 : autoregressive process
cor_1 <- corAR1(form=~1|id)   # no autocorrelation
cor_2 <- corAR1(0.1,form=~1|id)   # autocorrelation set to 0.1

## corCAR1 : continuous autoregressive process
cor_3 <- corCAR1(0.1,form=~1|id)  # correlation between two observations one unit of time apart set to 0.1

## corGaus : Gaussian spatial correlation
cor_5 <- corGaus(form=~1|id)
cor_6 <- corGaus(form=~1|id, nugget = TRUE) # account for nugget effect

## corLin : representing a linear spatial correlation structure
# cor_7 <- corLin(form=~1|id)
# cor_8 <- corLin(form=~1|id, nugget = TRUE)
# cor_7, cor_8, -- those do not behave well -- Then excluded

## corExp : exponential spatial correlation structure
cor_9 <- corExp(form=~1|id)
cor_10 <- corExp(form=~1|id, nugget = TRUE)

## corRatio : Rational quadratics spatial correlation.
cor_11 <- corRatio(form=~1|id)
cor_12 <- corRatio(form=~1|id, nugget = TRUE)

## corSpher : spherical spatial correlation.
cor_13 <- corSpher(form=~1|id)
cor_14 <- corSpher(form=~1|id, nugget = TRUE)

## corARMA : autoregressive moving average process
cor_16 <- corARMA(form = ~ 1 | id, p=0, q=1)
cor_17 <- corARMA(form = ~ 1 | id, p=1, q=0)
cor_18 <- corARMA(form = ~ 1 | id, p=1, q=1) 
cor_19 <- corARMA(form = ~ 1 | id, p=1, q=2)
cor_20 <- corARMA(form = ~ 1 | id, p=2, q=1)

cor_str_id_list <- NULL
cor_str_id_list <- list(cor_1, cor_2, cor_3, cor_5, cor_6, cor_9, cor_10, cor_11, cor_12, cor_13, cor_14, cor_16, cor_17, cor_18, cor_19, cor_20)

cor_str_names <- NULL
cor_str_names <- c( "corAR1(form=~1|id)", 
                    "corAR1(0.1,form=~1|id)", 
                    "corCAR1(0.1,form=~1|id)", 
                     "corGaus(form=~1|id)", 
                     "corGaus(form=~1|id, nugget = TRUE)", 
                     "corExp(form=~1|id)", 
                     "corExp(form=~1|id, nugget = TRUE)", 
                     "corRatio(form=~1|id)", 
                     "corRatio(form=~1|id, nugget = TRUE)",
                     "corSpher(form=~1|id)", 
                     "corSpher(form=~1|id, nugget = TRUE)", 
                     "corARMA(form = ~ 1 | id, p=0, q=1)",
                     "corARMA(form = ~ 1 | id, p=1, q=0)", 
                     "corARMA(form = ~ 1 | id, p=1, q=1)", 
                     "corARMA(form = ~ 1 | id, p=1, q=2)",
                     "corARMA(form = ~ 1 | id, p=2, q=1)")

######################################### models by ID ###################################

gamm_models_id_list <- list()

if(size == TRUE & acc == FALSE & hydration == FALSE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + size,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}

                                          } else if(size == FALSE & acc == FALSE & hydration == FALSE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs"),
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}

                                          } else if(size == TRUE & acc == TRUE & hydration == FALSE) {
                                                  
for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + size + acctemp,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}

}
 
										  } else if(size == FALSE & acc == TRUE & hydration == FALSE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + acctemp,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}
                                        
                                        	  } else if(size == FALSE & acc == TRUE & hydration == TRUE) {
	
for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + size + hydration,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}

                                          } else if(size == FALSE & acc == FALSE & hydration == TRUE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + hydration,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}

                                          } else if(size == TRUE & acc == FALSE & hydration == TRUE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + size + hydration,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}

                                          } else if(size == TRUE & acc == TRUE & hydration == TRUE) {
                                                  	
for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + size + acctemp + hydration,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}

}
 
										  } else if(size == FALSE & acc == TRUE & hydration == TRUE) {

for (i in 1:length(cor_str_id_list)) {
            gamm_temp <- try(gamm(performance~te(temp,k=k,bs="cs") + acctemp + hydration,
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)
                            if ('try-error' %in% class(gamm_temp)) { gamm_models_id_list[i]  <- 'correlation structure failed'
                                                                    next
                            } else {gamm_models_id_list[i]  <- list(gamm_temp)}
}
                                                  	
}

# names list based on correlation names

names(gamm_models_id_list) <- cor_str_names

# remove fail correlation structures

oo <- gamm_models_id_list[gamm_models_id_list != 'correlation structure failed']

gamm_models_id_list <- oo

# save models $gam and $lme to list

gamm_models_id_list_lme <- list()

for (i in 1:length(gamm_models_id_list)) {
                                      gamm_models_id_list_lme[i] <-  list(gamm_models_id_list[[i]]$lme)
                                          } # save models $lme to list for anova comparisons

# save models $gam and $lme to list

gamm_models_id_list_gam <- list()

for (i in 1:length( gamm_models_id_list)) {
                                      gamm_models_id_list_gam[i] <-  list(gamm_models_id_list[[i]]$gam)
                                          } # save models $gam to list for anova comparisons

gamm_models_id_list_lme_logLik <- list()

for (i in 1:length( gamm_models_id_list_lme)) {
                                      gamm_models_id_list_lme_logLik[i] <-  list(gamm_models_id_list_lme[[i]]$logLik)
                                          }

gamm_models_id_list_lme_logLik_unlist <- unlist(gamm_models_id_list_lme_logLik)

####################################  model comparison ###################################

## creates a dataframe with list of retained models

updated_names_gamm_models_id_list <- as.data.frame(names(gamm_models_id_list)) 

## gets AIC and BIC of the retained models

all_models_scores_df <- NULL # clean up previous runs

for (i in 1:length(gamm_models_id_list_lme)){
      if (!exists("all_models_scores_df")){
                AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
                BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
                logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
                all_models_scores_df <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("all_models_scores_df")){
                AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
                BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
                logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
                temp_scores <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
                all_models_scores_df<-rbind(all_models_scores_df, temp_scores)
    rm(temp_scores)
  }
 }

moto <- cbind(updated_names_gamm_models_id_list, all_models_scores_df) # binds rows

#Compare models: either use AIC, BIC or Log-Likelihood ratio tests from table below to decide on your top model

moto$delta_AIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$AIC_gamm_models))
moto$delta_BIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$BIC_gamm_models))
moto$knots <- k

names(moto) <- c("gamm_correlation_structure_class", "AIC", "BIC", "logLik", "delta_AIC", "delta_BIC", "knots")

### select the best model for prediction

best_gamm_model_AIC <- gamm_models_id_list[[which.min(moto[,5] )]] ## model_best_AIC <- which.min(moto[,5] ) 
best_gamm_model_BIC <- gamm_models_id_list[[which.min(moto[,6] )]] ## model_best_BIC <- which.min(moto[,6] )

## summary for best models
cat("\n\n")
cat("best_gamm_model_AIC")
cat("\n")
print (summary(best_gamm_model_AIC$gam))
cat("\n\n")
print (best_gamm_model_AIC)
cat("\n\n")
cat("best_gamm_model_AIC -- adjusted R squared")
cat("\n")
print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
cat("\n\n")
cat("best_gamm_model_BIC")
cat("\n")
print (summary(best_gamm_model_BIC$gam))
cat("\n\n")
print (best_gamm_model_BIC)
cat("\n\n")
cat("best_gamm_model_BIC -- adjusted R squared")
cat("\n")
print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
cat("\n\n")
cat("Model comparison results")
cat("\n")
print(moto)
cat("\n\n")

# plot with size or not



plot(plot_dispersion_species)

devAskNewPage(ask=T)
plot(best_gamm_model_AIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
title("Best GAMM model AIC")

if(size == TRUE) {

devAskNewPage(ask=T)
vis.gam(best_gamm_model_AIC$gam,
				view=c('size','temp'),
                type = "response",
                theta=55,
                phi=30,
                color="heat",
                xlab="size mm",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model AIC")

}


if(acc == TRUE) {
	
devAskNewPage(ask=T)
vis.gam(best_gamm_model_AIC$gam,
				view=c('acctemp','temp'),
               	type = "response",
                theta=55,
                phi=30,
                color="heat",
                xlab="Acclimation Temperature °C",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model AIC")
}

if(hydration == TRUE) {
	
devAskNewPage(ask=T)
vis.gam(best_gamm_model_AIC$gam,
				view=c('hydration','temp'),
               	type = "response",
                theta=55,
                phi=30,
                color="heat",
                xlab="Hydration % of body mass",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model AIC")
}

devAskNewPage(ask=T)
plot(plot_dispersion_species)

devAskNewPage(ask=T)
plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
title("Best GAMM model BIC")

if(size == TRUE) {

devAskNewPage(ask=T)
vis.gam(best_gamm_model_BIC$gam,
				view=c('size','temp'),
                type = "response",
                theta=55,
                phi=30,
                color="heat",
                xlab="size mm",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model BIC")

}

if(acc == TRUE) {

devAskNewPage(ask=T)
vis.gam(best_gamm_model_BIC$gam,
                type = "response",
                view=c('acctemp','temp'),
                theta=55,
                phi=30,
                color="heat",
                xlab="Acclimation Temperature °C",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model BIC")
}                          


if(hydration == TRUE) {

devAskNewPage(ask=T)
vis.gam(best_gamm_model_BIC$gam,
                type = "response",
                view=c('hydration','temp'),
                theta=55,
                phi=30,
                color="heat",
                xlab="Hydration % of body mass",
                ylab="Temperature °C",
                zlab="Performance",
                ticktype="detailed",
                n.grid=50) # color: gray; theta is the perspective angle of the graph
title("Best GAMM model BIC")
}                                         

if(size == TRUE & acc == TRUE & hydration == FALSE) {
Lizard_gam_TPC_AIC <- function (temp_value, size_value, acctemp_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, size_value, acctemp_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}

if(size == TRUE & acc == FALSE & hydration == FALSE) {
Lizard_gam_TPC_AIC <- function (temp_value, size_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, size_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}

if(size == FALSE & acc == FALSE & hydration == FALSE) {
Lizard_gam_TPC_AIC <- function (temp_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}


}

if(size == FALSE & acc == TRUE & hydration == FALSE) {
Lizard_gam_TPC_AIC <- function (temp_value, acctemp_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, acctemp=acctemp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, acctemp_value) {
                                       formula_gam <- best_gamm_model_BICl$gam
                                       pred_data <- data.frame(temp=temp_value, acctemp=acctemp_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}

##

if(size == TRUE & acc == TRUE & hydration == TRUE) {
Lizard_gam_TPC_AIC <- function (temp_value, size_value, acctemp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, size_value, acctemp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, acctemp=acctemp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}

if(size == TRUE & acc == FALSE & hydration == TRUE) {
Lizard_gam_TPC_AIC <- function (temp_value, size_value, hydration_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, size_value, hydration_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}

if(size == FALSE & acc == FALSE & hydration == TRUE) {
Lizard_gam_TPC_AIC <- function (temp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_BIC$gam
                                       pred_data <- data.frame(temp=temp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}


}

if(size == FALSE & acc == TRUE & hydration == TRUE) {
Lizard_gam_TPC_AIC <- function (temp_value, acctemp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_AIC$gam
                                       pred_data <- data.frame(temp=temp_value, acctemp=acctemp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

Lizard_gam_TPC_BIC <- function (temp_value, acctemp_value, hydration_value) {
                                       formula_gam <- best_gamm_model_BICl$gam
                                       pred_data <- data.frame(temp=temp_value, acctemp=acctemp_value, hydration=hydration_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
}

}



return(list(Lizard_gam_TPC_AIC,Lizard_gam_TPC_BIC,best_gamm_model_BIC,best_gamm_model_AIC))

}

#############################################################################################################################
#PerfGAMM2_1###################################################################################################################
#############################################################################################################################

PerfGAMM2 <- function (performance_data, 
                                     dependent_variable,
                                     smooth_predictors,
                                     linear_predictors,
                                     quadratic_predictors = NULL,
                                     criterion = "BIC",
                                     knots_vector = 7,
                                     plot_variable_on_x,
                                     plot_variable_on_y,
                                     plot_variable_on_z,
                                     TPCFUN_value = TRUE,
                                     output_directory) {

require('ggplot2')
require('grid')
require('mgcv')


# input data 

list_input_performance_ini <- performance_data
dependent_variable_names <- dependent_variable
smooth_variable_names <- smooth_predictors
linear_predictors_names <- linear_predictors
quadratic_predictors_names <- quadratic_predictors

criterion <- criterion
knots_vector <- knots_vector

plot_variable_on_x <- plot_variable_on_x
plot_variable_on_y <- plot_variable_on_y
plot_variable_on_z <- plot_variable_on_z

TPCFUN <- TPCFUN_value

out_dir <- output_directory

master_directory <- getwd()

# dir.create

if(is.null(out_dir) == FALSE) {
                              dir.create(out_dir, showWarnings = F)
                              setwd(out_dir)
                                    }

# read file if not data frame

if(class(list_input_performance_ini) == "data.frame") {
                             list_input_performance_raw <- list_input_performance_ini
                                   } else {
if(grepl(pattern = "*.csv$", list_input_performance_ini) == TRUE) {list_input_performance_raw <- read.table(file = list_input_performance_ini, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", list_input_performance_ini) == TRUE) {list_input_performance_raw <- read.table(file = list_input_performance_ini, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

# get species names

species_name <- unique(list_input_performance_raw$species)
species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$genus_species), species_name)
species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$Genus_species), species_name)
species_name <-ifelse(is.null(species_name), unique(list_input_performance_raw$Genus_Species), species_name)

# function of not into

"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 

# subset_groups: 

performance_reference_1 <- list_input_performance_raw[ , which(names(list_input_performance_raw) %in% c("id", dependent_variable_names,smooth_variable_names,
                                                                             linear_predictors_names,quadratic_predictors_names))]
performance_reference <- performance_reference_1[complete.cases(performance_reference_1),]

get_numbers_individuals <- unique(as.character(performance_reference$id))
get_numbers_PER <- nrow(performance_reference)

cat("\n--------------------------              Performance              --------------------------\n")
cat("Species name: ", species_name, " \n")
cat(paste("Total individuals included:", " ", length(get_numbers_individuals), "\n", sep=""))
cat(paste("Total observations:", " ", get_numbers_PER, "\n", sep=""))
cat("\n-------------------------------------------------------------------------------------------\n")


## explore two 2D

#### plot points againts lat

require(grid)
plot_dispersion_species <- ggplot(performance_reference, aes_string(x=smooth_variable_names[1],y= dependent_variable_names[1], color = "species_name")) + 
                                             geom_point(size = 1, alpha = 0.7) + 
                                             theme_bw() +
                                             guides(colour=FALSE) +
                                             theme(legend.text = element_text(),
                                             panel.grid.major = element_line(colour = "gray"),
                                             panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
                                             geom_rug(col="darkred",alpha=.7)

##########################################################################################
############################## Correlation schemes by ID #################################

## corAR1 : autoregressive process
cor_1 <- corAR1(form=~1|id)   # no autocorrelation
cor_2 <- corAR1(0.1,form=~1|id)   # autocorrelation set to 0.1

## corCAR1 : continuous autoregressive process
cor_3 <- corCAR1(0.1,form=~1|id)  # correlation between two observations one unit of time apart set to 0.1

## corGaus : Gaussian spatial correlation
cor_5 <- corGaus(form=~1|id)
cor_6 <- corGaus(form=~1|id, nugget = TRUE) # account for nugget effect

## corLin : representing a linear spatial correlation structure
# cor_7 <- corLin(form=~1|id)
# cor_8 <- corLin(form=~1|id, nugget = TRUE)
# cor_7, cor_8, -- those do not behave well -- Then excluded

## corExp : exponential spatial correlation structure
cor_9 <- corExp(form=~1|id)
cor_10 <- corExp(form=~1|id, nugget = TRUE)

## corRatio : Rational quadratics spatial correlation
cor_11 <- corRatio(form=~1|id)
cor_12 <- corRatio(form=~1|id, nugget = TRUE)

## corSpher : spherical spatial correlation
cor_13 <- corSpher(form=~1|id)
cor_14 <- corSpher(form=~1|id, nugget = TRUE)

## corARMA : autoregressive moving average process
cor_16 <- corARMA(form = ~ 1 | id, p=0, q=1)
cor_17 <- corARMA(form = ~ 1 | id, p=1, q=0)
cor_18 <- corARMA(form = ~ 1 | id, p=1, q=1) 
cor_19 <- corARMA(form = ~ 1 | id, p=1, q=2)
cor_20 <- corARMA(form = ~ 1 | id, p=2, q=1)

cor_str_id_list <- NULL
cor_str_id_list <- list(cor_1, cor_2, cor_3, cor_5, cor_6, cor_9, cor_10, cor_11, cor_12, cor_13, cor_14, cor_16, cor_17, cor_18, cor_19, cor_20)

cor_str_names <- NULL
cor_str_names <- c( "corAR1(form=~1|id): autoregressive process", 
                    "corAR1(0.1,form=~1|id): autoregressive process", 
                    "corCAR1(0.1,form=~1|id): continuous autoregressive process", 
                     "corGaus(form=~1|id): Gaussian spatial correlation", 
                     "corGaus(form=~1|id, nugget = TRUE): Gaussian spatial correlation", 
                     "corExp(form=~1|id): exponential spatial correlation structure", 
                     "corExp(form=~1|id, nugget = TRUE): exponential spatial correlation structure", 
                     "corRatio(form=~1|id): Rational quadratics spatial correlation", 
                     "corRatio(form=~1|id, nugget = TRUE): Rational quadratics spatial correlation",
                     "corSpher(form=~1|id): spherical spatial correlation", 
                     "corSpher(form=~1|id, nugget = TRUE): spherical spatial correlation", 
                     "corARMA(form = ~ 1 | id, p=0, q=1): autoregressive moving average process",
                     "corARMA(form = ~ 1 | id, p=1, q=0): autoregressive moving average process", 
                     "corARMA(form = ~ 1 | id, p=1, q=1): autoregressive moving average process", 
                     "corARMA(form = ~ 1 | id, p=1, q=2): autoregressive moving average process",
                     "corARMA(form = ~ 1 | id, p=2, q=1): autoregressive moving average process")

##########################################################################################
##########        subseting to dependent, predicto,  covariate, id       #################
##########################################################################################

performance_input <- as.data.frame(performance_reference)
performance_input$order <- row.names(performance_input)

##########################################################################################
##########################################################################################

# use knots

uni_temp <- seq(1:knots_vector)
k_final <- knots_vector # k is number of knots

##########################################################################################
##############                 The formula construction              ####################

# linear

if(!is.null(linear_predictors_names)) {
                 if(length(linear_predictors_names) == 1) {
                         collapsed_linear <- paste(linear_predictors_names, sep="")
                                                           } 
                 if(length(linear_predictors_names) > 1) {
                        collapsed_linear <- paste(linear_predictors_names, sep="", collapse=" + ")
                                                                  }
                                         }

if(is.null(linear_predictors_names)) {collapsed_linear <- NULL}

# quadratic

if(!is.null(quadratic_predictors_names)) {
                 if(length(quadratic_predictors_names) == 1) {
                         collapsed_quadratic <- paste("(", quadratic_predictors_names, ")^2", sep="")
                                                           } 
                 if(length(quadratic_predictors_names) > 1) {
                        collapsed_quadratic_1 <- paste("(", quadratic_predictors_names, sep="", collapse=")^2 + ")
                        collapsed_quadratic <- paste0(collapsed_quadratic_1, ")^2", sep="")
                                                                  }
                                         }

if(is.null(quadratic_predictors_names)) {collapsed_quadratic <- NULL}

################  constructing the user formula

## for non null collapsed_linear

if(!is.null(collapsed_linear)) {
                      if(!is.null(collapsed_quadratic)) {
                                  user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                                                collapsed_linear, ' + ', collapsed_quadratic) 
                                                        }
                      if(is.null(collapsed_quadratic)) {
                                  user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                                                collapsed_linear)
                                                        }
                                                    }

## for null collapsed_linear

if(is.null(collapsed_linear)) {
                      if(!is.null(collapsed_quadratic)) {
                                  user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs") +', 
                                                                collapsed_quadratic) 
                                                        }
                      if(is.null(collapsed_quadratic)) {
                                  user_string_formula <- paste0(dependent_variable_names, ' ~ te(',smooth_variable_names,',k=k_final,bs="cs")')
                                                        }
                                                    }

cat("\n--------------------------              Performance formula              --------------------------\n")
cat("processing formula: ", user_string_formula, "\n")
cat("number of k_final knots: ", k_final, "\n")
cat("\n---------------------------------------------------------------------------------------------------\n")

##########################################################################################
##############    Loop to correation structures with k_final knots    ####################

gamm_models_id_list <- list()
gamm_models_id_failed <- list()
counter_fail <- 0
gamm_models_id_success <- list()
counter_success <- 0


for (i in 1:length(cor_str_id_list)) {

            gamm_temp <- try(gamm(as.formula(user_string_formula),
                                 correlation=cor_str_id_list[[i]],
                                 data=performance_input,
                                 method="REML",
                                 knots=list(x=uni_temp)), silent=TRUE)

                    cat(paste0("processeing correltaion srt: ", class(cor_str_id_list[[i]])[1], "\n"))

                            if ('try-error' %in% class(gamm_temp)) { counter_fail <- counter_fail + 1
                                                                     gamm_models_id_failed[[counter_fail]] <- cor_str_names[i]
                                                            next

                                                            } else {counter_success <- counter_success + 1
                                                                   gamm_models_id_list[counter_success]  <- list(gamm_temp)
                                                                   gamm_models_id_success[[counter_success]]<- cor_str_names[i]
                                            }

                                      }


# models list to vector

gamm_models_id_failed_vector <- as.vector(do.call(rbind,gamm_models_id_failed))
gamm_models_id_success_vector <- as.vector(do.call(rbind,gamm_models_id_success))

# names list based on correlation names

names(gamm_models_id_list) <- gamm_models_id_success_vector

# save models $gam and $lme to list

gamm_models_id_list_lme <- list()

for (i in 1:length( gamm_models_id_list)) {
                                      gamm_models_id_list_lme[i] <-  list(gamm_models_id_list[[i]]$lme)
                                          } # save models $lme to list for anova comparisons

# save models $gam and $lme to list

gamm_models_id_list_gam <- list()

for (i in 1:length( gamm_models_id_list)) {
                                      gamm_models_id_list_gam[i] <-  list(gamm_models_id_list[[i]]$gam)
                                          } # save models $gam to list for anova comparisons

gamm_models_id_list_lme_logLik <- list()

for (i in 1:length( gamm_models_id_list_lme)) {
                                      gamm_models_id_list_lme_logLik[i] <-  list(gamm_models_id_list_lme[[i]]$logLik)
                                          }

gamm_models_id_list_lme_logLik_unlist <- unlist(gamm_models_id_list_lme_logLik)

####################################  model comparison ###################################

## creates a dataframe with list of retained models

updated_names_gamm_models_id_list <- as.data.frame(names(gamm_models_id_list)) 


## gets AIC and BIC of the retained models

all_models_scores_df <- NULL # clean up previous runs

for (i in 1:length(gamm_models_id_list_lme)){
      if (!exists("all_models_scores_df")){
                AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
                BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
                logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
                all_models_scores_df <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("all_models_scores_df")){
                AIC_gamm_models <- AIC(gamm_models_id_list_lme[[i]])
                BIC_gamm_models <- BIC(gamm_models_id_list_lme[[i]])
                logLik_gamm_models <- gamm_models_id_list_lme[[i]]$logLik
                temp_scores <- cbind(AIC_gamm_models, BIC_gamm_models, logLik_gamm_models)
                all_models_scores_df<-rbind(all_models_scores_df, temp_scores)
    rm(temp_scores)
  }
 }

moto <- cbind(updated_names_gamm_models_id_list, all_models_scores_df) # binds rows

#Compare models: either use AIC, BIC or Log-Likelihood ratio tests from table below to decide on your top model

moto$delta_AIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$AIC_gamm_models))
moto$delta_BIC <- do.call(function(x) x-x[which(x==min(x))], list(moto$BIC_gamm_models))
moto$knots <- k_final

names(moto) <- c("gamm_correlation_structure_class", "AIC", "BIC", "logLik", "delta_AIC", "delta_BIC", "knots")

# reorder moto based on Delta BIC

moto <- moto[ order(moto[,6]), ]

### select the best model for prediction

best_gamm_model_AIC <- gamm_models_id_list[[which.min(moto[,5] )]] ## model_best_AIC <- which.min(moto[,5] ) 
best_gamm_model_BIC <- gamm_models_id_list[[which.min(moto[,6] )]] ## model_best_BIC <- which.min(moto[,6] )


# plot 3D 

plot(best_gamm_model_AIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
title(paste("Best GAMM model AIC  -- ", species_name))

pdf(paste(species_name,'species_best_fit_3D_gam.pdf')) # only plot objects can be made pdfs
plot(plot_dispersion_species)
plot(best_gamm_model_BIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
title(paste("Best GAMM model BIC  -- ", species_name))
vis.gam(best_gamm_model_BIC$gam,
                view = c(plot_variable_on_x, plot_variable_on_z),
                type = "response",
                theta=50,
                phi=30,
                color="topo",
                xlab= plot_variable_on_x,
                ylab= plot_variable_on_z,
                zlab= plot_variable_on_y,
                ticktype="detailed",
                n.grid=60) # color: gray; theta is the perspective angle of the graph
title(paste("Best GAMM model BIC  -- ", species_name))

dev.off()

## which one to return

ifelse (criterion == 'BIC', best_model <- best_gamm_model_BIC, best_model <- best_gamm_model_AIC)

# calculate CTmax, CTmin and Topt

temp_test_no_order <- as.data.frame(c(seq(-10,50, by = 0.01), -10, 50)) 
temp_test <- as.data.frame(temp_test_no_order[order(temp_test_no_order[,1]), ])
names(temp_test) <-'temp'

# get means of selected predictors

performance_input_select <- subset(performance_input, select = names(performance_input) %!in% c("id","temp", "performance", "order"))

predictors_df_list <- list()

for(i in 1:ncol(performance_input_select)) {
                         if(is.numeric(performance_input_select[1,i])) {
                                                          one_column_mean <- as.data.frame(t(colMeans(performance_input_select[i])), stringsAsFactors = F)
                                                                       } else {
                                                          one_column_mean <- as.data.frame(t(performance_input_select[1,i]), stringsAsFactors = F)
                                                          names(one_column_mean) <- names(performance_input_select[i])
                                                                       }
                                              predictors_df_list[[i]] <- one_column_mean
                                                                       }

predictors_df <- do.call(cbind, predictors_df_list)
data_test <- merge(temp_test, predictors_df, all.x = TRUE)
predicted_performance <- as.vector(predict.gam(best_model$gam,data_test))
data_test$predicted_performance <- predicted_performance

# Toptimum

Toptimum_mean_predictors <- data_test[data_test$predicted_performance == max(data_test$predicted_performance) , ]

# CTmin 

one_species_max_to_correct_0 <- data_test[which.max(data_test$predicted_performance),]
one_species_pre_opt_to_correct_0 <- subset(data_test, data_test$temp < one_species_max_to_correct_0$temp)

one_species_pre_opt_ID_to_correct_0_min <- one_species_pre_opt_to_correct_0[which.min(one_species_pre_opt_to_correct_0$predicted_performance),]
one_species_pre_opt_ID_to_correct_0_neg <- one_species_pre_opt_to_correct_0[one_species_pre_opt_to_correct_0$predicted_performance < 0,]

if(nrow(one_species_pre_opt_ID_to_correct_0_neg) < 1) {
                                             row_to_report_CTmin <- one_species_pre_opt_ID_to_correct_0_min$temp
                                                      } else {
row_to_report_CTmin <- ifelse(one_species_pre_opt_ID_to_correct_0_min$temp < max(one_species_pre_opt_ID_to_correct_0_neg$temp),
                              max(one_species_pre_opt_ID_to_correct_0_neg$temp), one_species_pre_opt_ID_to_correct_0_min$temp)
                                                             }

# CTmax

one_species_post_opt_to_correct_0 <- subset(data_test, data_test$temp > one_species_max_to_correct_0$temp)
one_species_post_opt_ID_to_correct_0 <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]

one_species_post_opt_ID_to_correct_0_min <- one_species_post_opt_to_correct_0[which.min(one_species_post_opt_to_correct_0$predicted_performance),]
one_species_post_opt_ID_to_correct_0_neg <- one_species_post_opt_to_correct_0[one_species_post_opt_to_correct_0$predicted_performance < 0,]

if(nrow(one_species_post_opt_ID_to_correct_0_neg) < 1) {
                                             row_to_report_CTmin <- one_species_post_opt_ID_to_correct_0_min$temp
                                                      } else {
row_to_report_CTmax <- ifelse(one_species_post_opt_ID_to_correct_0_min$temp > min(one_species_post_opt_ID_to_correct_0_neg$temp),
                              min(one_species_post_opt_ID_to_correct_0_neg$temp), one_species_post_opt_ID_to_correct_0_min$temp)
                                                             }

#### report these

## print to screen

                          cat("------------      TPC GAMM based on given k_final      ------------\n\n")
                          print(species_name)
                          cat("\n") 
                          cat("------------      Model comparison results      ------------")
                          cat("\n\n")
                          print(moto)
                          cat("\n") 
                          cat("------------      best_gamm_model_BIC -- adjusted R squared")
                          cat("\n\n")
                          print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
                          cat("\n")
                          cat("------------      best_gamm_model_BIC      ------------")
                          cat("\n")
                          print (summary(best_gamm_model_BIC$gam))
                          cat("\n\n")
                          cat("------------      best_gamm_model_AIC -- adjusted R squared")
                          cat("\n\n")
                          print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
                          cat("\n")
                          cat("------------      best_gamm_model_AIC      ------------")
                          cat("\n")
                          print (summary(best_gamm_model_AIC$gam))
                          cat("\n\n")
                          cat("------------      Toptimum      ------------")
                          cat("\n")
                          print (Toptimum_mean_predictors)
                          cat("\n\n")
                          cat("------------      CTmax      ------------")
                          cat("\n")
                          print (row_to_report_CTmax)
                          cat("\n\n")
                          cat("------------      CTmin      ------------")
                          cat("\n")
                          print (row_to_report_CTmin)
                          cat("\n\n")
                          cat("----------------------------------------\n")

## write summary for best models

writeLines(capture.output( {
                          cat("------------      TPC GAMM on given k_final     ------------\n\n")
                          print(species_name)
                          cat("\n") 
                          cat("------------      Model comparison results      ------------")
                          cat("\n")
                          print(moto)
                          cat("\n") 
                          cat("------------      best_gamm_model_BIC -- adjusted R squared")
                          cat("\n")
                          print (summary(best_gamm_model_BIC$gam)$r.sq) #adjusted R squared
                          cat("\n")
                          cat("------------      best_gamm_model_BIC      ------------")
                          cat("\n")
                          print (summary(best_gamm_model_BIC$gam))
                          cat("\n\n")
                          print (best_gamm_model_BIC)
                          cat("\n")
                          cat("------------      best_gamm_model_AIC -- adjusted R squared")
                          cat("\n")
                          print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
                          cat("\n")
                          cat("------------      best_gamm_model_AIC      ------------")
                          cat("\n")
                          print (summary(best_gamm_model_AIC$gam))
                          cat("\n\n")
                          print (best_gamm_model_AIC)
                          cat("\n")
                          cat("------------      Toptimum      ------------")
                          cat("\n")
                          print (Toptimum_mean_predictors)
                          cat("\n")
                          cat("------------      CTmax      ------------")
                          cat("\n")
                          print (row_to_report_CTmax)
                          cat("\n")
                          cat("------------      CTmin      ------------")
                          cat("\n")
                          print (row_to_report_CTmin)
                          cat("\n")
                          cat("----------------------------------------\n")

 }
                                      ),con=paste(species_name, '_performance_calc_k_AUTO_raw_gamm_fit.txt', sep=""))

# set back to master directory

setwd(master_directory)

if(TPCFUN==T){

TPCFUN_GAM <- function (temp_value, size_value) {
                                       formula_gam <- best_model$gam
                                       pred_data_raw <- data.frame(temp=temp_value, size = size_value)
                                       predictors_df_subset <- subset(predictors_df, select = c(-size))
                                       pred_data <- merge(pred_data_raw, predictors_df_subset, all.x = TRUE)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
                                               }

##########################################################################################
##########################  pseduo gamm fit function  ###################################
##########################################################################################

TPCFUN_PSEUDO_GAM <- function (size_value_pseudo) {

# internal values

lower_temp_value = 0
upper_temp_value = 50
interval_segments_size_value = 0.01
TCP_gamm_function_name = best_model$gam
svl_value_mean = size_value_pseudo
n_terms_in_lm_function = 10

# user input

lower_temp <- lower_temp_value
upper_temp <- upper_temp_value
interval_segments_size <- interval_segments_size_value
TCP_gamm_function <- TCP_gamm_function_name
svl_value_input <- svl_value_mean
n_terms_in_lm <- n_terms_in_lm_function

# simulation to gam predicted points

simulation_TPC <- as.data.frame(c(seq(lower_temp,upper_temp, by = interval_segments_size))) # Species specific range
names(simulation_TPC) <-'Temp'

for (i in 1:length(simulation_TPC[,1])) {
                                         simulation_TPC$TPC [i] <- TPCFUN_GAM(simulation_TPC[i,1],svl_value_input)
                                        }

lm_model <- lm(TPC ~ poly(Temp, n_terms_in_lm, raw=TRUE), data=simulation_TPC) # 9 terms

lm_pseudo_gam_fun <- function (temp_value, svl_value) {
                                       dummy_var <- svl_value # this is necessary to keep the code consistent
                                       formula_lm <- lm_model
                                       pred_data <- data.frame(Temp=temp_value)
                                       P <- as.vector(predict(formula_lm, newdata=pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) } }

### return lm function

return(lm_pseudo_gam_fun)

}

GAM_functions_list <- list(TPCFUN_GAM = TPCFUN_GAM, TPCFUN_PSEUDO_GAM = TPCFUN_PSEUDO_GAM)

return(GAM_functions_list)

} else {

# return model

return(best_model)
}
}

##############                          END of function                       ############
#############################################################################################################################
#EcoPhysRasters -- JCS              #########################################################################################
#############################################################################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

EcoPhysRasters_JCS       <-       function(rasterpath_value,
                                          hahrmethod_value,
                                          RichCoef_value=NULL,

                                          Tupr_value =NA,
                                          Tlwr_value=NA,
                                          hcap_value=NA,
                                          hr_res_value=1, # try to keep at 1 for high resolution

                                          winter_months_user_value = NULL,
                                          winter_months_mode_value = 'precipitation',

                                          Breeding_value=TRUE,
                                          disjoint_breeding_months_value = NA,
                                          StartBreeding_value=1,
                                          StopBreeding_value=12,
                                          StartBreeding_frac_value=1,
                                          StopBreeding_frac_value=1 ,

                                          diel_value= 'diurnal',
                                          sunrise_sunset_value,
                                          scenarios_value=c('all'),
                                          variables_value=c('standard', 'hydrology', 'performance'), 

                                          sp_coord_value=NULL,
                                          PerfFUN_value=NA,
                                          ForestOffset_value,
                                          size_value,

                                          acclag_value=NA,
                                          EWLPerf_value=F,
                                          EWLCoef_value=NA,
                                          dehydrationtime_value=1,
                                          EWLdry_value=T,
                                          EWLshade_value=T,

                                          crop_logical_value = FALSE,
                                          Lon_max_value,
                                          Lon_min_value,
                                          Lat_max_value,
                                          Lat_min_value,

                                          output_dir_value){


require('raster')
require('rlist')
require('mgcv')

# input from user

rasterpath = rasterpath_value

if(hahrmethod_value %in% c("Senoid", "thermoconformer", "thermo", "senoid", "sinenoid", "sinusoid")) { hahrmethod <- 'thermoconformer' }
if(hahrmethod_value %in% c("Richards", "heliotherm", "Heliotherm", "Richard", "richards")) { hahrmethod <- 'heliotherm' }

RichCoef = RichCoef_value

Tupr = Tupr_value
Tlwr = Tlwr_value
hcap = hcap_value
hr_res = hr_res_value

winter_months_user = winter_months_user_value
winter_months_selection = winter_months_mode_value


Breeding = Breeding_value
disjoint_breeding_months = disjoint_breeding_months_value
StartBreeding = StartBreeding_value
StopBreeding = StopBreeding_value
StartBreeding_frac = StartBreeding_frac_value
StopBreeding_frac = StopBreeding_frac_value

diel = diel_value
sunrise_sunset = sunrise_sunset_value
scenarios = scenarios_value
variables = variables_value

sp_coord_file = sp_coord_value
PerfFUN_list = PerfFUN_value
ForestOffset <- ForestOffset_value
size = size_value

acclag = acclag_value
EWLPerf = EWLPerf_value
EWLCoef = EWLCoef_value
dehydrationtime = dehydrationtime_value
EWLdry = EWLdry_value
EWLshade = EWLshade_value


crop_logical = crop_logical_value
Lon_max = Lon_max_value
Lon_min = Lon_min_value
Lat_max = Lat_max_value
Lat_min = Lat_min_value

# write_rasters_logical = write_rasters_logical_value
output_dir = output_dir_value

master_directory  <- getwd()

###################            create dir             #########################

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                                    }

###################            read files or are data_frames             ################

if(class(sp_coord_file) == "data.frame") {
                             sp_coord <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- unique(sp_coord$species)

##########################################################################################
###################            Load and crop rasters             #########################
##########################################################################################

# Crop extent

if (crop_logical == FALSE) {ext_species <- NULL} 
if (crop_logical == TRUE) {
                          if(is.null(Lon_max)) {ext_species <- extent((min(sp_coord$Lon)-1), (max(sp_coord$Lon)+1), (min(sp_coord$Lat)-1), (max(sp_coord$Lat)+1))}
                          if(!is.null(Lon_max)){ext_species <- extent(Lon_min, Lon_max, Lat_min, Lat_max)}
                                      }

# load rasters

alt_file <- list.files(path = rasterpath, pattern="^alt_.*.gri$",full.names=T, ignore.case=T)
sunlight_file <- list.files(path = rasterpath, pattern="^sunlight_.*.gri$",full.names=T, ignore.case=T)
solar_radiation_file <- list.files(path = rasterpath, pattern="^solar_.*.gri$",full.names=T, ignore.case=T)
vapor_file <- list.files(path = rasterpath, pattern="^vapor_.*.gri$",full.names=T, ignore.case=T)
wind_file <- list.files(path = rasterpath, pattern="^wind_.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

alt_stack <- stack(alt_file)
if(crop_logical == TRUE) {alt_stack <- crop(alt_stack, ext_species) }

sunlight_stack <- stack(sunlight_file)
if(crop_logical == TRUE) {sunlight_stack <- crop(sunlight_stack, ext_species) }

sunrad_stack <- stack(solar_radiation_file)
if(crop_logical == TRUE) {sunrad_stack <- crop(sunrad_stack, ext_species) }

vapor_stack <- stack(vapor_file)
if(crop_logical == TRUE) {vapor_stack <- crop(vapor_stack, ext_species) }

wind_stack <- stack(wind_file)
if(crop_logical == TRUE) {wind_stack <- crop(wind_stack, ext_species) }

##################           present and future prec, tmax, tmin raster

scenario_names<-c()
all_crop<-list()

#####Present---1975######

if (isTRUE(scenarios=='all') | is.element('present',scenarios)) {

# load rasters

bioclim_present_file <- list.files(path = rasterpath, pattern="^bioclim_*.present.*.gri$",full.names=T, ignore.case=T)

prec_present_file <- list.files(path = rasterpath, pattern="^prec_*.present.*.gri$",full.names=T, ignore.case=T)
tmax_present_file <- list.files(path = rasterpath, pattern="^tmax_*.present.*.gri$",full.names=T, ignore.case=T)
tmin_present_file <- list.files(path = rasterpath, pattern="^tmin_*.present.*.gri$",full.names=T, ignore.case=T)

relative_humidity_present_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.present.*.gri$",full.names=T, ignore.case=T)
PET_present_file <- list.files(path = rasterpath, pattern="^PET_.*.present.*.gri$",full.names=T, ignore.case=T)
AET_present_file <- list.files(path = rasterpath, pattern="^AET_.*.present.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_present_file <- list.files(path = rasterpath, pattern="^SolarRad.*.present.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_present_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.present.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_present_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.present.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_present_stack <- stack(bioclim_present_file)
if(crop_logical == TRUE) {bioclim_present_stack <- crop(bioclim_present_stack, ext_species) }
prec_present_stack <- stack(prec_present_file)
if(crop_logical == TRUE) {prec_present_stack <- crop(prec_present_stack, ext_species) }
tmax_present_stack <- stack(tmax_present_file)
if(crop_logical == TRUE) {tmax_present_stack <- crop(tmax_present_stack, ext_species) }
tmin_present_stack <- stack(tmin_present_file)
if(crop_logical == TRUE) {tmin_present_stack <- crop(tmin_present_stack, ext_species) }

rh_present_stack <- stack(relative_humidity_present_file)
if(crop_logical == TRUE) {rh_present_stack <- crop(rh_present_stack, ext_species) }
PET_present_stack <- stack(PET_present_file)
if(crop_logical == TRUE) {PET_present_stack <- crop(PET_present_stack, ext_species) }
AET_present_stack <- stack(AET_present_file)
if(crop_logical == TRUE) {AET_present_stack <- crop(AET_present_stack, ext_species) }
SolarRad_present_stack <- stack(SolarRadHydro_present_file)
if(crop_logical == TRUE) {SolarRad_present_stack <- crop(SolarRad_present_stack, ext_species) }

WinterPrecLogical_present_stack <- stack(WinterPrecLogical_present_file)
if(crop_logical == TRUE) {WinterPrecLogical_present_stack <- crop(WinterPrecLogical_present_stack, ext_species) }
WinterSolarLogical_present_stack <- stack(WinterSolarLogical_present_file)
if(crop_logical == TRUE) {WinterSolarLogical_present_stack <- crop(WinterSolarLogical_present_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_present = bioclim_present_stack, 
                                          prec_present = prec_present_stack, 
                                          tmax_present = tmax_present_stack, 
                                          tmin_present = tmin_present_stack,
                                            rh_present = rh_present_stack,
                                           PET_present = PET_present_stack,
                                           AET_present = AET_present_stack,
                                      SolarRad_present = SolarRad_present_stack,
                             WinterPrecLogical_present = WinterPrecLogical_present_stack,
                            WinterSolarLogical_present = WinterSolarLogical_present_stack)
scenario_names<-c(scenario_names,'present')
                      } # end of if loop if present


#####   2050_26_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2050_26_rpc',scenarios)) {

# load rasters

bioclim_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2050_26_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2050_26_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2050_26_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2050_26_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2050_26_rpc_stack <- stack(bioclim_2050_26_rpc_file)
if(crop_logical == TRUE) {bioclim_2050_26_rpc_stack <- crop(bioclim_2050_26_rpc_stack, ext_species) }
prec_2050_26_rpc_stack <- stack(prec_2050_26_rpc_file)
if(crop_logical == TRUE) {prec_2050_26_rpc_stack <- crop(prec_2050_26_rpc_stack, ext_species) }
tmax_2050_26_rpc_stack <- stack(tmax_2050_26_rpc_file)
if(crop_logical == TRUE) {tmax_2050_26_rpc_stack <- crop(tmax_2050_26_rpc_stack, ext_species) }
tmin_2050_26_rpc_stack <- stack(tmin_2050_26_rpc_file)
if(crop_logical == TRUE) {tmin_2050_26_rpc_stack <- crop(tmin_2050_26_rpc_stack, ext_species) }

rh_2050_26_rpc_stack <- stack(relative_humidity_2050_26_rpc_file)
if(crop_logical == TRUE) {rh_2050_26_rpc_stack <- crop(rh_2050_26_rpc_stack, ext_species) }
PET_2050_26_rpc_stack <- stack(PET_2050_26_rpc_file)
if(crop_logical == TRUE) {PET_2050_26_rpc_stack <- crop(PET_2050_26_rpc_stack, ext_species) }
AET_2050_26_rpc_stack <- stack(AET_2050_26_rpc_file)
if(crop_logical == TRUE) {AET_2050_26_rpc_stack <- crop(AET_2050_26_rpc_stack, ext_species) }
SolarRad_2050_26_rpc_stack <- stack(SolarRadHydro_2050_26_rpc_file)
if(crop_logical == TRUE) {SolarRad_2050_26_rpc_stack <- crop(SolarRad_2050_26_rpc_stack, ext_species) }

WinterPrecLogical_2050_26_rpc_stack <- stack(WinterPrecLogical_2050_26_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2050_26_rpc_stack <- crop(WinterPrecLogical_2050_26_rpc_stack, ext_species) }
WinterSolarLogical_2050_26_rpc_stack <- stack(WinterSolarLogical_2050_26_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2050_26_rpc_stack <- crop(WinterSolarLogical_2050_26_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2050_26_rpc = bioclim_2050_26_rpc_stack, 
                                          prec_2050_26_rpc = prec_2050_26_rpc_stack, 
                                          tmax_2050_26_rpc = tmax_2050_26_rpc_stack, 
                                          tmin_2050_26_rpc = tmin_2050_26_rpc_stack,
                                            rh_2050_26_rpc = rh_2050_26_rpc_stack,
                                           PET_2050_26_rpc = PET_2050_26_rpc_stack,
                                           AET_2050_26_rpc = AET_2050_26_rpc_stack,
                                      SolarRad_2050_26_rpc = SolarRad_2050_26_rpc_stack,
                             WinterPrecLogical_2050_26_rpc = WinterPrecLogical_2050_26_rpc_stack,
                            WinterSolarLogical_2050_26_rpc = WinterSolarLogical_2050_26_rpc_stack)
scenario_names<-c(scenario_names,'2050_26_rpc')
                      } # end of if loop if 2050_26_rpc


#####   2050_45_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2050_45_rpc',scenarios)) {

# load rasters

bioclim_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2050_45_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2050_45_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2050_45_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2050_45_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2050_45_rpc_stack <- stack(bioclim_2050_45_rpc_file)
if(crop_logical == TRUE) {bioclim_2050_45_rpc_stack <- crop(bioclim_2050_45_rpc_stack, ext_species) }
prec_2050_45_rpc_stack <- stack(prec_2050_45_rpc_file)
if(crop_logical == TRUE) {prec_2050_45_rpc_stack <- crop(prec_2050_45_rpc_stack, ext_species) }
tmax_2050_45_rpc_stack <- stack(tmax_2050_45_rpc_file)
if(crop_logical == TRUE) {tmax_2050_45_rpc_stack <- crop(tmax_2050_45_rpc_stack, ext_species) }
tmin_2050_45_rpc_stack <- stack(tmin_2050_45_rpc_file)
if(crop_logical == TRUE) {tmin_2050_45_rpc_stack <- crop(tmin_2050_45_rpc_stack, ext_species) }

rh_2050_45_rpc_stack <- stack(relative_humidity_2050_45_rpc_file)
if(crop_logical == TRUE) {rh_2050_45_rpc_stack <- crop(rh_2050_45_rpc_stack, ext_species) }
PET_2050_45_rpc_stack <- stack(PET_2050_45_rpc_file)
if(crop_logical == TRUE) {PET_2050_45_rpc_stack <- crop(PET_2050_45_rpc_stack, ext_species) }
AET_2050_45_rpc_stack <- stack(AET_2050_45_rpc_file)
if(crop_logical == TRUE) {AET_2050_45_rpc_stack <- crop(AET_2050_45_rpc_stack, ext_species) }
SolarRad_2050_45_rpc_stack <- stack(SolarRadHydro_2050_45_rpc_file)
if(crop_logical == TRUE) {SolarRad_2050_45_rpc_stack <- crop(SolarRad_2050_45_rpc_stack, ext_species) }

WinterPrecLogical_2050_45_rpc_stack <- stack(WinterPrecLogical_2050_45_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2050_45_rpc_stack <- crop(WinterPrecLogical_2050_45_rpc_stack, ext_species) }
WinterSolarLogical_2050_45_rpc_stack <- stack(WinterSolarLogical_2050_45_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2050_45_rpc_stack <- crop(WinterSolarLogical_2050_45_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2050_45_rpc = bioclim_2050_45_rpc_stack, 
                                          prec_2050_45_rpc = prec_2050_45_rpc_stack, 
                                          tmax_2050_45_rpc = tmax_2050_45_rpc_stack, 
                                          tmin_2050_45_rpc = tmin_2050_45_rpc_stack,
                                            rh_2050_45_rpc = rh_2050_45_rpc_stack,
                                           PET_2050_45_rpc = PET_2050_45_rpc_stack,
                                           AET_2050_45_rpc = AET_2050_45_rpc_stack,
                                      SolarRad_2050_45_rpc = SolarRad_2050_45_rpc_stack,
                             WinterPrecLogical_2050_45_rpc = WinterPrecLogical_2050_45_rpc_stack,
                            WinterSolarLogical_2050_45_rpc = WinterSolarLogical_2050_45_rpc_stack)
scenario_names<-c(scenario_names,'2050_45_rpc')
                      } # end of if loop if 2050_45_rpc


#####   2050_85_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2050_85_rpc',scenarios)) {

# load rasters

bioclim_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2050_85_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2050_85_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2050_85_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2050_85_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2050_85_rpc_stack <- stack(bioclim_2050_85_rpc_file)
if(crop_logical == TRUE) {bioclim_2050_85_rpc_stack <- crop(bioclim_2050_85_rpc_stack, ext_species) }
prec_2050_85_rpc_stack <- stack(prec_2050_85_rpc_file)
if(crop_logical == TRUE) {prec_2050_85_rpc_stack <- crop(prec_2050_85_rpc_stack, ext_species) }
tmax_2050_85_rpc_stack <- stack(tmax_2050_85_rpc_file)
if(crop_logical == TRUE) {tmax_2050_85_rpc_stack <- crop(tmax_2050_85_rpc_stack, ext_species) }
tmin_2050_85_rpc_stack <- stack(tmin_2050_85_rpc_file)
if(crop_logical == TRUE) {tmin_2050_85_rpc_stack <- crop(tmin_2050_85_rpc_stack, ext_species) }

rh_2050_85_rpc_stack <- stack(relative_humidity_2050_85_rpc_file)
if(crop_logical == TRUE) {rh_2050_85_rpc_stack <- crop(rh_2050_85_rpc_stack, ext_species) }
PET_2050_85_rpc_stack <- stack(PET_2050_85_rpc_file)
if(crop_logical == TRUE) {PET_2050_85_rpc_stack <- crop(PET_2050_85_rpc_stack, ext_species) }
AET_2050_85_rpc_stack <- stack(AET_2050_85_rpc_file)
if(crop_logical == TRUE) {AET_2050_85_rpc_stack <- crop(AET_2050_85_rpc_stack, ext_species) }
SolarRad_2050_85_rpc_stack <- stack(SolarRadHydro_2050_85_rpc_file)
if(crop_logical == TRUE) {SolarRad_2050_85_rpc_stack <- crop(SolarRad_2050_85_rpc_stack, ext_species) }

WinterPrecLogical_2050_85_rpc_stack <- stack(WinterPrecLogical_2050_85_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2050_85_rpc_stack <- crop(WinterPrecLogical_2050_85_rpc_stack, ext_species) }
WinterSolarLogical_2050_85_rpc_stack <- stack(WinterSolarLogical_2050_85_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2050_85_rpc_stack <- crop(WinterSolarLogical_2050_85_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2050_85_rpc = bioclim_2050_85_rpc_stack, 
                                          prec_2050_85_rpc = prec_2050_85_rpc_stack, 
                                          tmax_2050_85_rpc = tmax_2050_85_rpc_stack, 
                                          tmin_2050_85_rpc = tmin_2050_85_rpc_stack,
                                            rh_2050_85_rpc = rh_2050_85_rpc_stack,
                                           PET_2050_85_rpc = PET_2050_85_rpc_stack,
                                           AET_2050_85_rpc = AET_2050_85_rpc_stack,
                                      SolarRad_2050_85_rpc = SolarRad_2050_85_rpc_stack,
                             WinterPrecLogical_2050_85_rpc = WinterPrecLogical_2050_85_rpc_stack,
                            WinterSolarLogical_2050_85_rpc = WinterSolarLogical_2050_85_rpc_stack)
scenario_names<-c(scenario_names,'2050_85_rpc')
                      } # end of if loop if 2050_85_rpc


#####   2070_26_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2070_26_rpc',scenarios)) {

# load rasters

bioclim_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2070_26_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2070_26_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2070_26_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2070_26_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2070_26_rpc_stack <- stack(bioclim_2070_26_rpc_file)
if(crop_logical == TRUE) {bioclim_2070_26_rpc_stack <- crop(bioclim_2070_26_rpc_stack, ext_species) }
prec_2070_26_rpc_stack <- stack(prec_2070_26_rpc_file)
if(crop_logical == TRUE) {prec_2070_26_rpc_stack <- crop(prec_2070_26_rpc_stack, ext_species) }
tmax_2070_26_rpc_stack <- stack(tmax_2070_26_rpc_file)
if(crop_logical == TRUE) {tmax_2070_26_rpc_stack <- crop(tmax_2070_26_rpc_stack, ext_species) }
tmin_2070_26_rpc_stack <- stack(tmin_2070_26_rpc_file)
if(crop_logical == TRUE) {tmin_2070_26_rpc_stack <- crop(tmin_2070_26_rpc_stack, ext_species) }

rh_2070_26_rpc_stack <- stack(relative_humidity_2070_26_rpc_file)
if(crop_logical == TRUE) {rh_2070_26_rpc_stack <- crop(rh_2070_26_rpc_stack, ext_species) }
PET_2070_26_rpc_stack <- stack(PET_2070_26_rpc_file)
if(crop_logical == TRUE) {PET_2070_26_rpc_stack <- crop(PET_2070_26_rpc_stack, ext_species) }
AET_2070_26_rpc_stack <- stack(AET_2070_26_rpc_file)
if(crop_logical == TRUE) {AET_2070_26_rpc_stack <- crop(AET_2070_26_rpc_stack, ext_species) }
SolarRad_2070_26_rpc_stack <- stack(SolarRadHydro_2070_26_rpc_file)
if(crop_logical == TRUE) {SolarRad_2070_26_rpc_stack <- crop(SolarRad_2070_26_rpc_stack, ext_species) }

WinterPrecLogical_2070_26_rpc_stack <- stack(WinterPrecLogical_2070_26_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2070_26_rpc_stack <- crop(WinterPrecLogical_2070_26_rpc_stack, ext_species) }
WinterSolarLogical_2070_26_rpc_stack <- stack(WinterSolarLogical_2070_26_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2070_26_rpc_stack <- crop(WinterSolarLogical_2070_26_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2070_26_rpc = bioclim_2070_26_rpc_stack, 
                                          prec_2070_26_rpc = prec_2070_26_rpc_stack, 
                                          tmax_2070_26_rpc = tmax_2070_26_rpc_stack, 
                                          tmin_2070_26_rpc = tmin_2070_26_rpc_stack,
                                            rh_2070_26_rpc = rh_2070_26_rpc_stack,
                                           PET_2070_26_rpc = PET_2070_26_rpc_stack,
                                           AET_2070_26_rpc = AET_2070_26_rpc_stack,
                                      SolarRad_2070_26_rpc = SolarRad_2070_26_rpc_stack,
                             WinterPrecLogical_2070_26_rpc = WinterPrecLogical_2070_26_rpc_stack,
                            WinterSolarLogical_2070_26_rpc = WinterSolarLogical_2070_26_rpc_stack)
scenario_names<-c(scenario_names,'2070_26_rpc')
                      } # end of if loop if 2070_26_rpc


#####   2070_45_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2070_45_rpc',scenarios)) {

# load rasters

bioclim_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2070_45_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2070_45_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2070_45_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2070_45_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2070_45_rpc_stack <- stack(bioclim_2070_45_rpc_file)
if(crop_logical == TRUE) {bioclim_2070_45_rpc_stack <- crop(bioclim_2070_45_rpc_stack, ext_species) }
prec_2070_45_rpc_stack <- stack(prec_2070_45_rpc_file)
if(crop_logical == TRUE) {prec_2070_45_rpc_stack <- crop(prec_2070_45_rpc_stack, ext_species) }
tmax_2070_45_rpc_stack <- stack(tmax_2070_45_rpc_file)
if(crop_logical == TRUE) {tmax_2070_45_rpc_stack <- crop(tmax_2070_45_rpc_stack, ext_species) }
tmin_2070_45_rpc_stack <- stack(tmin_2070_45_rpc_file)
if(crop_logical == TRUE) {tmin_2070_45_rpc_stack <- crop(tmin_2070_45_rpc_stack, ext_species) }

rh_2070_45_rpc_stack <- stack(relative_humidity_2070_45_rpc_file)
if(crop_logical == TRUE) {rh_2070_45_rpc_stack <- crop(rh_2070_45_rpc_stack, ext_species) }
PET_2070_45_rpc_stack <- stack(PET_2070_45_rpc_file)
if(crop_logical == TRUE) {PET_2070_45_rpc_stack <- crop(PET_2070_45_rpc_stack, ext_species) }
AET_2070_45_rpc_stack <- stack(AET_2070_45_rpc_file)
if(crop_logical == TRUE) {AET_2070_45_rpc_stack <- crop(AET_2070_45_rpc_stack, ext_species) }
SolarRad_2070_45_rpc_stack <- stack(SolarRadHydro_2070_45_rpc_file)
if(crop_logical == TRUE) {SolarRad_2070_45_rpc_stack <- crop(SolarRad_2070_45_rpc_stack, ext_species) }

WinterPrecLogical_2070_45_rpc_stack <- stack(WinterPrecLogical_2070_45_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2070_45_rpc_stack <- crop(WinterPrecLogical_2070_45_rpc_stack, ext_species) }
WinterSolarLogical_2070_45_rpc_stack <- stack(WinterSolarLogical_2070_45_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2070_45_rpc_stack <- crop(WinterSolarLogical_2070_45_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2070_45_rpc = bioclim_2070_45_rpc_stack, 
                                          prec_2070_45_rpc = prec_2070_45_rpc_stack, 
                                          tmax_2070_45_rpc = tmax_2070_45_rpc_stack, 
                                          tmin_2070_45_rpc = tmin_2070_45_rpc_stack,
                                            rh_2070_45_rpc = rh_2070_45_rpc_stack,
                                           PET_2070_45_rpc = PET_2070_45_rpc_stack,
                                           AET_2070_45_rpc = AET_2070_45_rpc_stack,
                                      SolarRad_2070_45_rpc = SolarRad_2070_45_rpc_stack,
                             WinterPrecLogical_2070_45_rpc = WinterPrecLogical_2070_45_rpc_stack,
                            WinterSolarLogical_2070_45_rpc = WinterSolarLogical_2070_45_rpc_stack)
scenario_names<-c(scenario_names,'2070_45_rpc')
                      } # end of if loop if 2070_45_rpc


#####   2070_85_rpc   ######

if (isTRUE(scenarios=='all') | is.element('2070_85_rpc',scenarios)) {

# load rasters

bioclim_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^bioclim_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)

prec_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^prec_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
tmax_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^tmax_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
tmin_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^tmin_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)

relative_humidity_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^rel_hum_.*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
PET_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^PET_.*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
AET_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^AET_.*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
SolarRadHydro_2070_85_rpc_file <- list.files(path = rasterpath, pattern="^SolarRad.*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)

WinterPrecLogical_2070_85_rpc_file <- list.files(path = rasterpath, pattern="WinterPrecLogical_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)
WinterSolarLogical_2070_85_rpc_file <- list.files(path = rasterpath, pattern="WinterSolarLogical_*.2070_85_rpc.*.gri$",full.names=T, ignore.case=T)

# read raster and crop if TRUE

bioclim_2070_85_rpc_stack <- stack(bioclim_2070_85_rpc_file)
if(crop_logical == TRUE) {bioclim_2070_85_rpc_stack <- crop(bioclim_2070_85_rpc_stack, ext_species) }
prec_2070_85_rpc_stack <- stack(prec_2070_85_rpc_file)
if(crop_logical == TRUE) {prec_2070_85_rpc_stack <- crop(prec_2070_85_rpc_stack, ext_species) }
tmax_2070_85_rpc_stack <- stack(tmax_2070_85_rpc_file)
if(crop_logical == TRUE) {tmax_2070_85_rpc_stack <- crop(tmax_2070_85_rpc_stack, ext_species) }
tmin_2070_85_rpc_stack <- stack(tmin_2070_85_rpc_file)
if(crop_logical == TRUE) {tmin_2070_85_rpc_stack <- crop(tmin_2070_85_rpc_stack, ext_species) }

rh_2070_85_rpc_stack <- stack(relative_humidity_2070_85_rpc_file)
if(crop_logical == TRUE) {rh_2070_85_rpc_stack <- crop(rh_2070_85_rpc_stack, ext_species) }
PET_2070_85_rpc_stack <- stack(PET_2070_85_rpc_file)
if(crop_logical == TRUE) {PET_2070_85_rpc_stack <- crop(PET_2070_85_rpc_stack, ext_species) }
AET_2070_85_rpc_stack <- stack(AET_2070_85_rpc_file)
if(crop_logical == TRUE) {AET_2070_85_rpc_stack <- crop(AET_2070_85_rpc_stack, ext_species) }
SolarRad_2070_85_rpc_stack <- stack(SolarRadHydro_2070_85_rpc_file)
if(crop_logical == TRUE) {SolarRad_2070_85_rpc_stack <- crop(SolarRad_2070_85_rpc_stack, ext_species) }

WinterPrecLogical_2070_85_rpc_stack <- stack(WinterPrecLogical_2070_85_rpc_file)
if(crop_logical == TRUE) {WinterPrecLogical_2070_85_rpc_stack <- crop(WinterPrecLogical_2070_85_rpc_stack, ext_species) }
WinterSolarLogical_2070_85_rpc_stack <- stack(WinterSolarLogical_2070_85_rpc_file)
if(crop_logical == TRUE) {WinterSolarLogical_2070_85_rpc_stack <- crop(WinterSolarLogical_2070_85_rpc_stack, ext_species) }


# put on list of rasters

all_crop[[length(all_crop)+1]] <- list(bioclim_2070_85_rpc = bioclim_2070_85_rpc_stack, 
                                          prec_2070_85_rpc = prec_2070_85_rpc_stack, 
                                          tmax_2070_85_rpc = tmax_2070_85_rpc_stack, 
                                          tmin_2070_85_rpc = tmin_2070_85_rpc_stack,
                                            rh_2070_85_rpc = rh_2070_85_rpc_stack,
                                           PET_2070_85_rpc = PET_2070_85_rpc_stack,
                                           AET_2070_85_rpc = AET_2070_85_rpc_stack,
                                      SolarRad_2070_85_rpc = SolarRad_2070_85_rpc_stack,
                             WinterPrecLogical_2070_85_rpc = WinterPrecLogical_2070_85_rpc_stack,
                            WinterSolarLogical_2070_85_rpc = WinterSolarLogical_2070_85_rpc_stack)
scenario_names<-c(scenario_names,'2070_85_rpc')
                      } # end of if loop if 2070_85_rpc

##########################################################################################
#EcoPhys
##########################################################################################

############  monthly limits on hours

if(!is.null(diel)){

if(class(sunrise_sunset) == "data.frame") {
                             sunrise_sunset_df <- sunrise_sunset
                                   } else {
if(grepl(pattern = "*.csv$", sunrise_sunset) == TRUE) {sunrise_sunset_df <- read.table(file = sunrise_sunset, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sunrise_sunset) == TRUE) {sunrise_sunset_df <- read.table(file = sunrise_sunset, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

if("species" %in% colnames(sunrise_sunset_df))  {
                    sunrise_sunset_df[,-c(1,5)] <-round(sunrise_sunset_df[,-c(1,5)],0)
                                                } else {
                    sunrise_sunset_df[,-1] <-round(sunrise_sunset_df[,-1],0)
                                                }

                     }

####################           sunlight

#### for stack 12 bands for sunlight

sunlight_seq_year_list <-list()

for (j in 1:12) {
  sun_bin<-as.matrix(sunlight_stack[[j]]/100)
  (sun_bin[sun_bin < - 998] <- NA) #assign -999 to missing
  sunlight_seq_year_list[[j]] <- sun_bin
}
rm(j)


##########################################################################################
####################     get extend to move raster to matix and viceversa     ############
##########################################################################################

raster_extend <- extent(alt_stack[[1]])

###################### the alt matrix

alt_matrix <- as.matrix(alt_stack[[1]])

##################   controller of master strack to ecophysiology

number_of_models <- length(scenario_names)

cat("\n---------------------- finish reading and/or cropping starting rasters ----------------------\n")

##########################################################################################
################################  loop per scenario  ###################################

processed_ecophysiology_rasters <- list()
processed_bioclim_rasters <- list()

for(N_scenario in 1:length(scenario_names)) {

#################                one scenarion                           #################

one_scenario_rasters <- all_crop[[N_scenario]]
model_year <- scenario_names[[N_scenario]]

# one_scenario_rasters <- all_crop[[1]]
# model_year <- scenario_names[[1]]

##########################     From raster to matrices    ################################

#######################              GENERAL MATRICES              #######################
#######################                 precipitation              #######################

prec_stack<-one_scenario_rasters[[grep("prec_*", names(one_scenario_rasters))]]
winterPrec_stack <- one_scenario_rasters[[grep("WinterPrecLogical_*", names(one_scenario_rasters))]]
winterSolar_stack <- one_scenario_rasters[[grep("WinterSolarLogical_*", names(one_scenario_rasters))]]

# get this one
# precipitation year

total_prec_raster <- sum(prec_stack)
names(total_prec_raster) <- "total_prec"

# prec matrices

prec_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_prec <- subset(prec_stack, m)
one_month_prec_matrix <- as.matrix(one_month_prec)
one_month_prec_matrix [is.na(one_month_prec_matrix)] <- 0 
prec_total_matrix_list [[counter]] <- one_month_prec_matrix
                  }
rm(m)

####################### Summer months

if(is.numeric(winter_months_user)) { 

# select summer months
all_months <- c(1,2,3,4,5,6,7,8,9,10,11,12)
summer_months_user <- setdiff(all_months, winter_months_user)

# summer precipitation if given months

summer_prec_matrix_list <- list()
counter <- 0

for(mm in 1:length(summer_months_user))     {
counter <- counter + 1
one_month_precipitation <- subset(prec_stack, summer_months_user[mm])
one_month_prec_matrix <- as.matrix(one_month_precipitation)
one_month_prec_matrix [is.na(one_month_prec_matrix)] <- 0 
summer_prec_matrix_list [[counter]] <- one_month_prec_matrix
                  }

## summer and winter sums

SumPrecmatrix <-Reduce('+',summer_prec_matrix_list)
SumPrecmatrix[SumPrecmatrix < 0] <- 0
SumPrecmatrix[is.na(alt_matrix)] <- NA

# get this one
## from matrix to raster

SumPrecmatrix_raster <-stack(raster(SumPrecmatrix, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(SumPrecmatrix_raster) <- "SumPrecmatrix"
SumPrecmatrix_raster[is.na(alt_stack)] <- NA 

                                 }

##########################################################################################

if(is.null(winter_months_user)) { 

# summer precipitation if not given months

summer_prec_matrix_list <- list()

if(winter_months_selection == 'daylength') {

summer_prec_matrix_list <- list()

for(m in 1:12)     {
one_month_precipitation <- subset(prec_stack, m)
one_month_sunlight <- subset(winterSolar_stack, m)
one_month_prec_matrix <- as.matrix(one_month_precipitation)
one_month_sunlight_logical_matrix <- as.matrix(one_month_sunlight)
one_month_prec_matrix [one_month_sunlight_logical_matrix == 1] <- 0 # 0 for summer:  sungligth > 12 h is 1 then if sungligth < 12 h is winter is 0
one_month_prec_matrix [is.na(one_month_prec_matrix)] <- 0 
summer_prec_matrix_list [[m]] <- one_month_prec_matrix
                  }
rm(m)
                                            }

if(winter_months_selection == 'precipitation') {

summer_prec_matrix_list <- list()

for(m in 1:12)     {
one_month_precipitation <- subset(prec_stack, m)
one_month_winterPrec <- subset(winterPrec_stack, m)
one_month_prec_matrix <- as.matrix(one_month_precipitation)
one_month_winterPrec_logical_matrix <- as.matrix(one_month_winterPrec)
one_month_prec_matrix [one_month_winterPrec_logical_matrix == 0] <- 0 # 0 for summer and 1 for winter
one_month_prec_matrix [is.na(one_month_prec_matrix)] <- 0 
summer_prec_matrix_list [[m]] <- one_month_prec_matrix
                  }
rm(m)
                                            }


## summer and winter sums

SumPrecmatrix <-Reduce('+',summer_prec_matrix_list)
SumPrecmatrix[SumPrecmatrix < 0] <- 0
SumPrecmatrix[is.na(alt_matrix)] <- NA

# get this one
## from matrix to raster

SumPrecmatrix_raster <-stack(raster(SumPrecmatrix, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(SumPrecmatrix_raster) <- "SumPrecmatrix"
SumPrecmatrix_raster[is.na(alt_stack)] <- NA 

                                                                    }

# get this one
# winter precipitation

WinPrecmatrix_raster <- total_prec_raster - SumPrecmatrix_raster
names(WinPrecmatrix_raster) <- "WinPrecmatrix"

# Breeding

if(Breeding==T) {

        if(!any(is.na(disjoint_breeding_months))) {
                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_prec_raster_all <- subset(prec_stack, disjoint_breeding_months)
                                    Breeding_prec_raster <- sum(Breeding_prec_raster_all)
                                    names(Breeding_prec_raster) <- "Breeding_prec"
                                    Breeding_prec_raster [is.na(alt_stack)] <- NA 
                                                                }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_prec_raster_all <- prec_stack * winterPrec_stack
                                    Breeding_prec_raster <- sum(Breeding_prec_raster_all)
                                    names(Breeding_prec_raster) <- "Breeding_prec"
                                    Breeding_prec_raster [is.na(alt_stack)] <- NA 
                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {
            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_prec <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_prec <- StartBreeding_frac*prec_total_matrix_list[[StartBreeding]] + StopBreeding_frac*prec_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_prec_raster <-stack(raster(Breeding_prec, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_prec_raster) <- "Breeding_prec"
Breeding_prec_raster [is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)


# get this one

non_Breeding_prec_raster <- total_prec_raster - Breeding_prec_raster
names(non_Breeding_prec_raster) <- "non_Breeding_prec"

                                         } # closes the if loop: Breeding

cat("\n---------------------- precipitation calculations completed from model: ", model_year, " ----------------------\n")

##########################################################################################
#####################            not implemente yet              #########################

if(is.element('EWL',variables) | EWLPerf==T){

####for stack 12 bands for rh

rh_seq_year_list <-list()

for (j in 1:12) {
  rh_bin<-as.matrix(all_crop[[i]][[rhindex]][[j]])
  (rh_bin[rh_bin==-32768] <- NA) #assign -999 to missing
  rh_seq_year_list[[j]] <- rh_bin
 
}

matrix_var_list[[length(matrix_var_list) + 1]]<-rh_seq_year_list

}

#####################            not implemente yet              #########################
##########################################################################################

#######################             relative humidity              #######################

rh_stack<-one_scenario_rasters[[grep("^rh_*", names(one_scenario_rasters))]]/10000 # needs to divide by 10000 to get correct %

# get this one
# relative humidity year

total_rh_raster <- sum(rh_stack)
names(total_rh_raster) <- "rh_total"
average_year_rh_raster <- calc(rh_stack, mean)
names(average_year_rh_raster) <- "rh_year_mean"

# rh matrices

rh_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_rh <- subset(rh_stack, m)
one_month_rh_matrix <- as.matrix(one_month_rh)
one_month_rh_matrix [is.na(one_month_rh_matrix)] <- 0 
rh_total_matrix_list [[counter]] <- one_month_rh_matrix
                  }
rm(m)
####################### Summer and winter months rh

if(is.numeric(winter_months_user)) { 

# summer rh if given months

all_months <- c(1,2,3,4,5,6,7,8,9,10,11,12)
summer_months_user <- setdiff(all_months, winter_months_user)

                summer_rh_raster_all <- subset(rh_stack, summer_months_user)
                summer_rh_raster <- calc(summer_rh_raster_all, mean)
                names(summer_rh_raster) <- "rh_summer_mean"
                summer_rh_raster[is.na(alt_stack)] <- NA 


# winter rh if given months
                winter_rh_raster_all <- subset(rh_stack, winter_months_user)
                winter_rh_raster <- calc(winter_rh_raster_all, mean)
                names(winter_rh_raster) <- "rh_winter_mean"
                winter_rh_raster[is.na(alt_stack)] <- NA

                                 }

##########################################################################################

if(is.null(winter_months_user)) { 

# summer RH if not given months

if(winter_months_selection == 'daylength') {

                 rh_stack_sunglight_summer <- rh_stack * winterSolar_stack # in winterSolar_stack: 1 is =>12 hours (i.e., summer) and 0 is <12 hours (i.e., winter)
                 summer_rh_raster <- calc(rh_stack_sunglight_summer, mean)
                 names(summer_rh_raster) <- "rh_summer_mean"
                 summer_rh_raster[is.na(alt_stack)] <- NA

                 summerSolar_stack <- winterSolar_stack
                 summerSolar_stack[winterSolar_stack==1] <- 0 # need to change 1 (i.e., > 12 hours) to 0 so multiplication is 0; and change 0 to 1 so multiplication is > 0
                 summerSolar_stack[winterSolar_stack==0] <- 1

                 rh_stack_sunglight_winter <- rh_stack * summerSolar_stack
                 winter_rh_raster <- calc(rh_stack_sunglight_winter, mean)
                 names(winter_rh_raster) <- "rh_winter_mean"
                 winter_rh_raster[is.na(alt_stack)] <- NA

                                          }

if(winter_months_selection == 'precipitation') {

                 rh_stack_prec_winter <- rh_stack * winterPrec_stack # in winterPrec_stack: 1 is precipitation > threshold (e.g., 3 quantile of precipitation year) and 0 is < threshold
                 winter_rh_raster <- calc(rh_stack_prec_winter, mean)
                 names(winter_rh_raster) <- "rh_winter_mean"
                 winter_rh_raster[is.na(alt_stack)] <- NA

                 summerPrec_stack <- winterPrec_stack
                 summerPrec_stack[winterPrec_stack==1] <- 0 # need to change 1 (i.e., > 3 quantile) to 0 so multiplication is 0; and change 0 to 1 so multiplication is > 0
                 summerPrec_stack[winterPrec_stack==0] <- 1

                 rh_stack_Prec_summer <- rh_stack * summerPrec_stack
                 summer_rh_raster <- calc(rh_stack_Prec_summer, mean)
                 names(summer_rh_raster) <- "rh_summer_mean"
                 summer_rh_raster[is.na(alt_stack)] <- NA

                                          }

                                                                    }

# Breeding

if(Breeding==T) {

# disjoint
if(!any(is.na(disjoint_breeding_months))) { 
                        if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_rh_raster_all <- subset(rh_stack, disjoint_breeding_months)
                                    Breeding_rh_raster <- calc(Breeding_rh_raster_all, mean)
                                    names(Breeding_rh_raster) <- "Breeding_rh_mean"
                                    Breeding_rh_raster[is.na(alt_stack)] <- NA

all_months <- c(1,2,3,4,5,6,7,8,9,10,11,12)
non_breeding_months_user <- setdiff(all_months, disjoint_breeding_months)

                                    non_Breeding_rh_raster_all <- subset(rh_stack, non_breeding_months_user)
                                    non_Breeding_rh_raster <- calc(non_Breeding_rh_raster_all, mean)
                                    names(non_Breeding_rh_raster) <- "non_Breeding_rh_mean"
                                    non_Breeding_rh_raster[is.na(alt_stack)] <- NA
                                                                 }

                            if(is.character(disjoint_breeding_months)) {

                                  rh_stack_prec_winter <- rh_stack * winterPrec_stack # in winterPrec_stack: 1 is precipitation > threshold (e.g., 3 quantile of precipitation year) and 0 is < threshold
                                      Breeding_rh_raster <- calc(rh_stack_prec_winter, mean)
                               names(Breeding_rh_raster) <- "Breeding_rh_mean"
                                     Breeding_rh_raster[is.na(alt_stack)] <- NA

                                      summerPrec_stack <- winterPrec_stack
                 summerPrec_stack[winterPrec_stack==1] <- 0 # need to change 1 (i.e., > 3 quantile) to 0 so multiplication is 0; and change 0 to 1 so multiplication is > 0
                 summerPrec_stack[winterPrec_stack==0] <- 1
                                  rh_stack_Prec_summer <- rh_stack * summerPrec_stack
                                non_Breeding_rh_raster <- calc(rh_stack_Prec_summer, mean)
                         names(non_Breeding_rh_raster) <- "non_Breeding_rh_mean"
                               non_Breeding_rh_raster[is.na(alt_stack)] <- NA

                                                                      }
                                                  }

# joint
if(any(is.na(disjoint_breeding_months))) {
            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_rh <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(rh_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(rh_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(rh_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(rh_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_rh <- StartBreeding_frac*rh_total_matrix_list[[StartBreeding]] + StopBreeding_frac*rh_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(rh_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get mean for breading

Breeding_rh_mean_matrix <-Breeding_rh / total_months_breed

# get this one

Breeding_rh_raster <-stack(raster(Breeding_rh_mean_matrix, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_rh_raster) <- "Breeding_rh_mean"
Breeding_rh_raster [is.na(alt_stack)] <- NA 

# get this one

non_Breeding_rh_total_matrix <- Reduce('+', rh_total_matrix_list) - Breeding_rh
total_months_non_breed <- 12 - total_months_breed
non_Breeding_rh_mean_matrix <-non_Breeding_rh_total_matrix /total_months_non_breed

non_Breeding_rh_raster <-stack(raster(non_Breeding_rh_mean_matrix, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(non_Breeding_rh_raster) <- "non_Breeding_rh_mean"
non_Breeding_rh_raster[is.na(alt_stack)] <- NA 
                                                }  # closes the if loop: is.na(disjoint_breeding_months)

                                         } # closes the if loop: Breeding

cat("\n---------------------- relative humidity calculations completed from model: ", model_year, " ----------------------\n")


#######################                 tmax              #######################

tmax_stack<-one_scenario_rasters[[grep("tmax_*", names(one_scenario_rasters))]]

# tmax matrices

tmax_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_tmax <- subset(tmax_stack, m)
one_month_tmax_matrix <- as.matrix(one_month_tmax)
one_month_tmax_matrix [is.na(one_month_tmax_matrix)] <- 0 
tmax_matrix_list [[counter]] <- one_month_tmax_matrix
                  }
rm(m)

tmax_matrix <- tmax_matrix_list

#######################                 tmin              #######################

tmin_stack<-one_scenario_rasters[[grep("tmin_*", names(one_scenario_rasters))]]

# tmin matrices

tmin_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_tmin <- subset(tmin_stack, m)
one_month_tmin_matrix <- as.matrix(one_month_tmin)
one_month_tmin_matrix [is.na(one_month_tmin_matrix)] <- 0 
tmin_matrix_list [[counter]] <- one_month_tmin_matrix
                   }
rm(m)

tmin_matrix <- tmin_matrix_list

##################################################################################
#######                           NOTICES

if(any(variables %in% c('performance', 'Performance', 'Perf'))){
cat("\n---------------------- Started performance calculations from model: ", model_year, " ----------------------\n")
                                                               }

if(any(variables %in% c('Hydrology','hydrology', 'hydro'))) {
cat("\n---------------------- Starting hydrology raster cropping from model: ", model_year, " ----------------------\n")
                          } # close loop if(any(variables %in% c('Hydrology','hydrology', 'hydro')))

if(hahrmethod == 'thermoconformer') { 
cat("\n---------------------- Started ha hr under 'thermoconformer mode' calculations completed from model: ", model_year, " ----------------------\n")
                                    }

if(hahrmethod == 'heliotherm') {
cat("\n---------------------- Started ha hr under 'heliotherm mode' calculations for model: ", model_year, " ----------------------\n")
                               }

##################################################################################
###########################        Performance          ##########################

if(any(variables %in% c('performance', 'Performance', 'Perf'))){

tmax_matrix <- tmax_matrix_list

# if EWLPref is TRUE

if(EWLPerf==T){
     if(EWLdry==T){ dry_value=1 } else { dry_value=0 }
     if(EWLshade==T){ shade_value=1 } else { shade_value=0 }

perf_mat_list<-list()	

for(m in 1:12){
	Perf_mat<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))
for(r in 1:nrow(tmax_matrix[[m]])){
	for(c in 1:ncol(tmax_matrix[[m]])){
			hydration=1-EWL_mat_list[[m]][r,c]^dehydrationtime
			Perf_mat[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(x),PerfFUN(temp_value = x[1], size_value=size, hydration=hydration), NA))
				}
             }

perf_mat_list[[m]] <- matrix(Perf_mat,nrow = nrow(tmax_matrix[[1]]),ncol = ncol(tmax_matrix[[1]]))
}}



###############    if EWLPref is FALSE

if(EWLPerf==F){

## is pseudo gam or gam performance function

PerfFUN_test <- try(PerfFUN_list(size), silent = T)

if(class(PerfFUN_test) == "try-error") { 
                                         PerfFUN <- PerfFUN_list
                                       } else {
                                         PerfFUN <- PerfFUN_test
                                       }

#######    if acclag is NA

if(is.na(acclag)){

#######                 Performance (Potential Evapotranspiration)              ##########

pb <- txtProgressBar(min = 0, max = 12, style = 3)
for (i in 1:12) {
    evap <- raster(tmax_stack, 1)
    Tmax <- values(subset(tmax_stack, i))/10
    Tmin <- values(subset(tmin_stack, i))/10
    T_air_noon <- ((ForestOffset + Tmax)-(ForestOffset + Tmin)) /2*sin(pi/12*(12)-3*pi/4) + ((ForestOffset + Tmax)+(ForestOffset + Tmin)) /2
    Performance_calc <- sapply (T_air_noon,function(x) ifelse(!is.na(x),PerfFUN(temp = x, svl_value=size), NA))
    values(evap) <- Performance_calc
    if (i == 1) {
        Perf_brick <<- brick(evap)
    }
    if (i > 1) {
        Perf_brick <<- addLayer(Perf_brick, evap)
    }
           setTxtProgressBar(pb, i)
}
Sys.sleep(1)
close(pb)
rm(i)

# name Performance layers

Perf_months <- c("Perf_1", "Perf_2", "Perf_3", "Perf_4", "Perf_5", "Perf_6", "Perf_7", "Perf_8", "Perf_9", "Perf_10", "Perf_11", "Perf_12")
Perf_1000 <- Perf_brick * 1000
names(Perf_1000) <- Perf_months

                 }


##########################################################################################
#####################            not implemente yet              #########################

if(!is.na(acclag)){
                  stop("performance with acclimatation not implemented yet")
                    } # close !is.na(acclag)

#####################            not implemente yet              #########################
##########################################################################################

                } # EWLPerf==F

#####################            total performance per year            #########################

total_Perf_raster <- sum(Perf_1000)
names(total_Perf_raster) <- "total_Perf_x_1000"
total_Perf_raster[is.na(alt_stack)] <- NA

#####################            total performance per year            #########################


# Breeding

if(Breeding==T) {


        if(!any(is.na(disjoint_breeding_months))) {
                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_Perf_raster_all <- subset(Perf_1000, disjoint_breeding_months)
                                    Breeding_Perf_raster <- sum(Breeding_Perf_raster_all)
                                    names(Breeding_Perf_raster) <- "Breeding_Perf_x_1000"
                                    Breeding_Perf_raster[is.na(alt_stack)] <- NA 
                                                                     }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_Perf_raster_all <- Perf_1000 * winterPrec_stack
                                    Breeding_Perf_raster <- sum(Breeding_Perf_raster_all)
                                    names(Breeding_Perf_raster) <- "Breeding_Perf_x_1000"
                                    Breeding_Perf_raster[is.na(alt_stack)] <- NA 
                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

# make a performance matrices

Perf_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_Pref <- subset(Perf_1000, m)
one_month_Pref_matrix <- as.matrix(one_month_Pref)
one_month_Pref_matrix [is.na(one_month_Pref_matrix)] <- 0 
Perf_total_matrix_list [[counter]] <- one_month_Pref_matrix
                  }
rm(m)

            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_Pref <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(Perf_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(Perf_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(Perf_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(Perf_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_Pref <- StartBreeding_frac*Perf_total_matrix_list[[StartBreeding]] + StopBreeding_frac*Perf_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(Perf_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_Perf_raster <-stack(raster(Breeding_Pref, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_Perf_raster) <- "Breeding_Perf_x_1000"
Breeding_Perf_raster[is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)


# get this one

non_Breeding_Perf_raster <- total_Perf_raster - Breeding_Perf_raster
names(non_Breeding_Perf_raster) <- "non_Breeding_Perf_x_1000"

                                         } # closes the if loop: Breeding

                          } # close loop if(any(variables %in% c('performance', 'Performance', 'Perf')))

# get this ones

# total_Perf_raster
# Breeding_Perf_raster
# non_Breeding_Perf_raster

###########################     END:  Performance       ##########################
##################################################################################

##########################################################################################
#######              PET AET Solar (Potential Evapotranspiration)       ##################
##########################################################################################

if(any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

PET_stack<-one_scenario_rasters[[grep("PET_*", names(one_scenario_rasters))]]
AET_stack<-one_scenario_rasters[[grep("AET_*", names(one_scenario_rasters))]]
SolarRad_stack<-one_scenario_rasters[[grep("SolarRad_*", names(one_scenario_rasters))]]

#####################            total hydrology per year            #########################

total_PET_raster <- sum(PET_stack)
names(total_PET_raster) <- "total_PET"

total_AET_raster <- sum(AET_stack)
names(total_AET_raster) <- "total_AET"

total_SolarRad_raster <- sum(SolarRad_stack)
names(total_SolarRad_raster) <- "total_SolarRad"

# Breeding

if(Breeding==T) {

        if(!any(is.na(disjoint_breeding_months))) {
                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_PET_raster_all <- subset(PET_stack, disjoint_breeding_months)
                                    Breeding_PET_raster <- sum(Breeding_PET_raster_all)
                                    names(Breeding_PET_raster) <- "Breeding_PET"
                                    Breeding_PET_raster[is.na(alt_stack)] <- NA
                                                                     }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_PET_raster_all <- PET_stack * winterPrec_stack
                                    Breeding_PET_raster <- sum(Breeding_PET_raster_all)
                                    names(Breeding_PET_raster) <- "Breeding_PET"
                                    Breeding_PET_raster[is.na(alt_stack)] <- NA
                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

# make a hydrology matrices

PET_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_PET <- subset(PET_stack, m)
one_month_PET_matrix <- as.matrix(one_month_PET)
one_month_PET_matrix [is.na(one_month_PET_matrix)] <- 0 
PET_total_matrix_list [[counter]] <- one_month_PET_matrix
                  }

            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_PET <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(PET_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(PET_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(PET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(PET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_PET <- StartBreeding_frac*PET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*PET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(PET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_PET_raster <-stack(raster(Breeding_PET, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_PET_raster) <- "Breeding_PET"
Breeding_PET_raster[is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)


# get this one

non_Breeding_PET_raster <- total_PET_raster - Breeding_PET_raster
names(non_Breeding_PET_raster) <- "non_Breeding_PET"

########################### AET

        if(!any(is.na(disjoint_breeding_months))) {
                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_AET_raster_all <- subset(AET_stack, disjoint_breeding_months)
                                    Breeding_AET_raster <- sum(Breeding_AET_raster_all)
                                    names(Breeding_AET_raster) <- "Breeding_AET"
                                    Breeding_AET_raster[is.na(alt_stack)] <- NA
                                                                     }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_AET_raster_all <- AET_stack * winterPrec_stack
                                    Breeding_AET_raster <- sum(Breeding_AET_raster_all)
                                    names(Breeding_AET_raster) <- "Breeding_AET"
                                    Breeding_AET_raster[is.na(alt_stack)] <- NA
                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

# make a hydrology matrices

AET_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_AET <- subset(AET_stack, m)
one_month_AET_matrix <- as.matrix(one_month_AET)
one_month_AET_matrix [is.na(one_month_AET_matrix)] <- 0 
AET_total_matrix_list [[counter]] <- one_month_AET_matrix
                  }

if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_AET <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(AET_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(AET_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(AET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(AET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_AET <- StartBreeding_frac*AET_total_matrix_list[[StartBreeding]] + StopBreeding_frac*AET_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(AET_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_AET_raster <-stack(raster(Breeding_AET, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_AET_raster) <- "Breeding_AET"
Breeding_AET_raster[is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)

# get this one

non_Breeding_AET_raster <- total_AET_raster - Breeding_AET_raster
names(non_Breeding_AET_raster) <- "non_Breeding_AET"

########################### SolarRad

        if(!any(is.na(disjoint_breeding_months))) {
                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_SolarRad_raster_all <- subset(SolarRad_stack, disjoint_breeding_months)
                                    Breeding_SolarRad_raster <- sum(Breeding_SolarRad_raster_all)
                                    names(Breeding_SolarRad_raster) <- "Breeding_SolarRad"
                                    Breeding_SolarRad_raster[is.na(alt_stack)] <- NA
                                                                     }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_SolarRad_raster_all <- SolarRad_stack * winterPrec_stack
                                    Breeding_SolarRad_raster <- sum(Breeding_SolarRad_raster_all)
                                    names(Breeding_SolarRad_raster) <- "Breeding_SolarRad"
                                    Breeding_SolarRad_raster[is.na(alt_stack)] <- NA
                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

# make a hydrology matrices

SolarRad_total_matrix_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_SolarRad <- subset(SolarRad_stack, m)
one_month_SolarRad_matrix <- as.matrix(one_month_SolarRad)
one_month_SolarRad_matrix [is.na(one_month_SolarRad_matrix)] <- 0 
SolarRad_total_matrix_list [[counter]] <- one_month_SolarRad_matrix
                  }

if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_SolarRad <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(SolarRad_total_matrix_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(SolarRad_total_matrix_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(SolarRad_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(SolarRad_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_SolarRad <- StartBreeding_frac*SolarRad_total_matrix_list[[StartBreeding]] + StopBreeding_frac*SolarRad_total_matrix_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(SolarRad_total_matrix_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_SolarRad_raster <-stack(raster(Breeding_SolarRad, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_SolarRad_raster) <- "Breeding_SolarRad"
Breeding_SolarRad_raster[is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)

# get this one

non_Breeding_SolarRad_raster <- total_SolarRad_raster - Breeding_SolarRad_raster
names(non_Breeding_SolarRad_raster) <- "non_Breeding_SolarRad"


                                         } # closes the if loop: Breeding

cat("\n---------------------- hydrology calculations completed from model: ", model_year, " ----------------------\n")

                          } # close loop if(any(variables %in% c('Hydrology','hydrology', 'hydro')))


##########################################################################################
#######                                ha and hr                        ##################
##########################################################################################

if(hahrmethod == 'thermoconformer') {

# keeping the same reference number

UCT_value <- Tupr
LCT_value <- Tlwr

# get species distribution sunrise and sunset

                if(!is.null(sunrise_sunset)) {
if(class(sunrise_sunset) == "data.frame") {
                     sunrise_sunset_table <- sunrise_sunset
                                   } else {
if(grepl(pattern = "*.csv$", sunrise_sunset) == TRUE) {sunrise_sunset_table <- read.table(file = sunrise_sunset, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sunrise_sunset) == TRUE) {sunrise_sunset_table <- read.table(file = sunrise_sunset, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }
sunrise_sunset_df <- subset(sunrise_sunset_table, select= c(sunrise,sunset))
                                             }

# function to divide

one_over_10 <- function(x) {x*1/10}
tmin_matrix_hahr <- lapply(tmin_matrix, one_over_10)
tmax_matrix_hahr <- lapply(tmax_matrix, one_over_10)

matrix_ncol <- ncol(tmin_matrix_hahr[[1]])
matrix_nrow <- nrow(tmin_matrix_hahr[[1]])
dimensions <-c(matrix_nrow, matrix_ncol)

## create empty matrices

mat_sumTair_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1]))# 12 matrices 1 per month
mat_sumUCT_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1])) # calculates the # time units that tair was > than UCT
mat_sumLCT_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1])) # calculates the # time units that tair was < than LCT
mat_avg_t_air_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1]))

h_a_mat_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1]))
h_r_mat_list <- lapply(1:12, function(x) matrix(, ncol=dimensions[2], nrow=dimensions[1]))

# create numeric empty matrices

for (x in 1:12) { # make numeric NA matrices
                 mode(mat_sumTair_list[[x]]) <- "double"
                 mode(mat_sumUCT_list[[x]]) <- "double"
                 mode(mat_sumLCT_list[[x]]) <- "double"
                 mode(mat_avg_t_air_list[[x]]) <- "double"
                 mode(h_a_mat_list[[x]]) <- "double"
                 mode(h_r_mat_list[[x]]) <- "double"
                 }


##  constants allocated to calculation matrices

mat_UCT_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1]))
mat_LCT_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1]))

for (n in 1:12 ) {
    mat_UCT_list[[n]] <- mat_UCT_list[[n]] + UCT_value
    mat_LCT_list[[n]] <- mat_LCT_list[[n]] + LCT_value
}

#### NEW SYSTEM

for (m in 1:12) {
					 mat_sumTair_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
					  mat_sumUCT_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
					  mat_sumLCT_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
				   mat_avg_t_air_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
					     h_a_mat_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
					     h_r_mat_list[[m]] [!is.na(tmax_matrix_hahr[[m]])] <- 0
}


################################ Loop 1 ##################################################

t_air<-list()

pb <- txtProgressBar(min = 0, max = 12, style = 3)

                               for(m in 1:12) { # open m loop Number -- 1

# m <-1

                if(is.null(diel)) {sunsrise_sunset_seq <- seq(1,24,1)
                                   hacap_value <- hcap
                                   hrcap_value <- hcap
                                  }

                if(diel == 'diurnal') {sunrise_h <- round(sunrise_sunset_df[m,1],0)
                                       sunset_h <- round(sunrise_sunset_df[m,2],0)
                                       sunsrise_sunset_seq <- seq(sunrise_h, sunset_h, 1)
                                       hacap_value <- length(sunsrise_sunset_seq)
                                       hrcap_value <- length(sunsrise_sunset_seq)
                                      }

                if(diel == 'nocturnal') {sunrise_h <- round(sunrise_sunset_df[m,1],0)
                                         sunset_h <- round(sunrise_sunset_df[m,2],0)
                                         all_hours <- seq(1,24,1)
                                         diurnal_hours <- seq(sunrise_h, sunset_h, 1)
                                         sunsrise_sunset_seq <- setdiff(all_hours, diurnal_hours)
                                         hacap_value <- length(sunsrise_sunset_seq)
                                         hrcap_value <- length(sunsrise_sunset_seq)
                                         }

                                   for(h in sunsrise_sunset_seq){ # open h
                                       for(i in 1:hr_res){ # open i

					t_air[[m]] <- ((ForestOffset + tmax_matrix_hahr[[m]])-(ForestOffset + tmin_matrix_hahr[[m]])) /
					          2*sin(pi/12*(h+i/hr_res)-3*pi/4) + 
					         ((ForestOffset + tmax_matrix_hahr[[m]])+(ForestOffset + tmin_matrix_hahr[[m]])) /
					          2

					mat_sumTair_list[[m]] <- mat_sumTair_list[[m]] + t_air[[m]]

					mat_sumUCT_calculator <-  rapply(t_air, function(x) ifelse(x>UCT_value, 1/hr_res,0), how="replace")
					mat_sumUCT_list[[m]] <- mat_sumUCT_list[[m]] + mat_sumUCT_calculator[[m]]

					mat_sumLCT_calculator <-  rapply(t_air, function(x) ifelse(x<LCT_value, 1/hr_res,0), how="replace")
					mat_sumLCT_list[[m]] <- mat_sumLCT_list[[m]] + mat_sumLCT_calculator[[m]]

                                                          } # close i
                                                } # close h
rm(i)
rm(h)

#### needs to be in m

					mat_avg_t_air_list[[m]] <- mat_avg_t_air_list[[m]] + mat_sumTair_list[[m]]/(length(sunsrise_sunset_seq)*hr_res) 

## sum h_a and h_r

                    hacap <- hacap_value * hr_res
                    ifelse (is.na(hacap_value) == T, h_a_mat_list[[m]] <- length(sunsrise_sunset_seq) - mat_sumLCT_list[[m]],
                                                            ifelse (mat_sumLCT_list[[m]] > hacap, h_a_mat_list[[m]]  <- h_a_mat_list[[m]] + 0, 
                                                                           h_a_mat_list[[m]]  <- h_a_mat_list[[m]] + length(sunsrise_sunset_seq) - mat_sumLCT_list[[m]]))

                     hrcap <- hrcap_value * hr_res
                     ifelse (is.na(hrcap_value) == T, h_r_mat_list[[m]] <- mat_sumUCT_list[[m]],
                                                              ifelse(mat_sumUCT_list[[m]]  > hrcap, h_r_mat_list[[m]]  <- h_r_mat_list[[m]] + hrcap,
                                                                                         h_r_mat_list[[m]]  <- h_r_mat_list[[m]] + mat_sumUCT_list[[m]]))

# for the progress bar
                        setTxtProgressBar(pb, m)
                                                } # close m loop
Sys.sleep(1)
close(pb)
rm(m)

###### Sum of total h_a

h_a_year_mat <- Reduce('+',h_a_mat_list)/hr_res
h_a_year_raster <-stack(raster(h_a_year_mat , 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(h_a_year_raster) <- "h_a_year"
h_a_year_raster[is.na(alt_stack)] <- NA 

######## Sum of total h_r

h_r_year_mat <- Reduce('+',h_r_mat_list)/hr_res
h_r_year_raster <-stack(raster(h_r_year_mat , 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(h_r_year_raster) <- "h_r_year"
h_r_year_raster[is.na(alt_stack)] <- NA 

######################        Stacking and writting Loop 1        ########################

# monthy raster

avg_t_air_raster_list<-list()
h_a_raster_list<-list()
h_r_raster_list<-list()

for (layer in 1:12) {

           avg_t_air_raster_list[layer] <- raster(mat_avg_t_air_list[[layer]], 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

           h_a_raster_list[layer] <- raster(h_a_mat_list[[layer]], 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

           h_r_raster_list[layer] <- raster(h_r_mat_list[[layer]], 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

           }

# monthly stacks

avg_t_air_raster_stack <- stack(avg_t_air_raster_list)
avg_t_air_raster_stack_x_100 <- avg_t_air_raster_stack*100
names(avg_t_air_raster_stack_x_100) <- c('tairAct_1', 'tairAct_2', 'tairAct_3', 'tairAct_4', 'tairAct_5', 'tairAct_6', 'tairAct_7', 'tairAct_8', 'tairAct_9', 'tairAct_10', 'tairAct_11', 'tairAct_12')
avg_t_air_raster_stack_x_100[is.na(alt_stack)] <- NA 

h_a_raster_stack <- stack(h_a_raster_list)/hr_res
h_a_raster_stack_x_10 <- h_a_raster_stack*10
names(h_a_raster_stack_x_10) <- c('h_a_1', 'h_a_2', 'h_a_3', 'h_a_4', 'h_a_5', 'h_a_6', 'h_a_7', 'h_a_8', 'h_a_9', 'h_a_10', 'h_a_11', 'h_a_12')
h_a_raster_stack_x_10[is.na(alt_stack)] <- NA 

h_r_raster_stack <- stack(h_r_raster_list)/hr_res
h_r_raster_stack_x_10 <- h_r_raster_stack*10
names(h_r_raster_stack_x_10) <- c('h_r_1', 'h_r_2', 'h_r_3', 'h_r_4', 'h_r_5', 'h_r_6', 'h_r_7', 'h_r_8', 'h_r_9', 'h_r_10', 'h_r_11', 'h_r_12')
h_r_raster_stack_x_10[is.na(alt_stack)] <- NA 

# Breeding

if(Breeding==T) {

        if(!any(is.na(disjoint_breeding_months))) {

                            if(is.numeric(disjoint_breeding_months)) {
                                    Breeding_ha_raster_all <- subset(h_a_raster_stack_x_10/10, disjoint_breeding_months)
                                    Breeding_ha_raster <- sum(Breeding_ha_raster_all)
                                    names(Breeding_ha_raster) <- "Breeding_ha"
                                    Breeding_ha_raster [is.na(alt_stack)] <- NA 

                                    Breeding_hr_raster_all <- subset(h_r_raster_stack_x_10/10, disjoint_breeding_months)
                                    Breeding_hr_raster <- sum(Breeding_hr_raster_all)
                                    names(Breeding_hr_raster) <- "Breeding_hr"
                                    Breeding_hr_raster [is.na(alt_stack)] <- NA 

                                                                }

                            if(is.character(disjoint_breeding_months)) {
                                    Breeding_ha_raster_all <- h_a_raster_stack_x_10/10 * winterPrec_stack
                                    Breeding_ha_raster <- sum(Breeding_ha_raster_all)
                                    names(Breeding_ha_raster) <- "Breeding_ha"
                                    Breeding_ha_raster [is.na(alt_stack)] <- NA 

                                    Breeding_hr_raster_all <- h_r_raster_stack_x_10/10 * winterPrec_stack
                                    Breeding_hr_raster <- sum(Breeding_hr_raster_all)
                                    names(Breeding_hr_raster) <- "Breeding_hr"
                                    Breeding_hr_raster [is.na(alt_stack)] <- NA 

                                                                }

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

## _ha

            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_ha <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

## _hr

            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_hr <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13

# get this one

Breeding_ha_raster <-stack(raster(Breeding_ha, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_ha_raster) <- "Breeding_ha"
Breeding_ha_raster [is.na(alt_stack)] <- NA 

Breeding_hr_raster <-stack(raster(Breeding_hr, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_hr_raster) <- "Breeding_hr"
Breeding_hr_raster [is.na(alt_stack)] <- NA 

                                                }  # closes the if loop: is.na(disjoint_breeding_months)

# get this one

non_Breeding_ha_raster <- h_a_year_raster - Breeding_ha_raster
names(non_Breeding_ha_raster) <- "non_Breeding_ha"

non_Breeding_hr_raster <- h_r_year_raster - Breeding_hr_raster
names(non_Breeding_hr_raster) <- "non_Breeding_hr"

                                         } # closes the if loop: Breeding

                                                      } # Closes the loop : if(hahrmethod == 'thermoconformer')

#####################################  heliotherm  #######################################

if(hahrmethod == 'heliotherm') {

if(is.null(RichCoef)) { stop("--------------- this 'hahrmethod' option requires a Richard coefficient list: RichCoef ---------------")}
if(is.na(RichCoef$hr_Asym) && is.na(RichCoef$ha_Asym)) { stop("--------------- all Richard coefficients are NA ---------------")}

#### HERE is my modification of the EcoPhysRaster for ha

if(!is.na(RichCoef$hr_Asym)) {

         if(RichCoef$estimator == "FlexParamCurve") {

## ha using Richards -- FlexParamCurve

hr_a_Asym <-RichCoef$hr_Asym
hr_c_K    <-RichCoef$hr_K
hr_b_d_M  <-RichCoef$hr_M
hr_i_Infl <-RichCoef$hr_Infl
Tpref      <-RichCoef$Tupr


           # ha calculations


pb <- txtProgressBar(min = 0, max = 12, style = 3)

month_hr_list <- list()
          for (i in 1:12) {
             month_hr_list[[i]] <- hr_a_Asym / ((1 + hr_b_d_M   * exp(-hr_c_K * ( (tmax_stack[[i]]/10-Tpref) - hr_i_Infl)         ))^(1/hr_b_d_M))
             setTxtProgressBar(pb, i)
                }
Sys.sleep(1)
close(pb)
rm(i)

## ha rasters montly and year

h_r_montly_raster <- stack(month_hr_list)
h_r_raster_stack_x_10 <- h_r_montly_raster*10
names(h_r_raster_stack_x_10) <- c('h_r_1', 'h_r_2', 'h_r_3', 'h_r_4', 'h_r_5', 'h_r_6', 'h_r_7', 'h_r_8', 'h_r_9', 'h_r_10', 'h_r_11', 'h_r_12')
h_r_raster_stack_x_10[is.na(alt_stack)] <- NA 

h_r_year_raster <- Reduce('+',month_hr_list)
names(h_r_year_raster) <- "h_r_year"
h_r_year_raster[is.na(alt_stack)] <- NA 

#########     h_r matrices

h_r_mat_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_h_r <- subset(h_r_montly_raster, m)
one_month_h_r_matrix <- as.matrix(one_month_h_r)
one_month_h_r_matrix [is.na(one_month_h_r_matrix)] <- 0 
h_r_mat_list [[counter]] <- one_month_h_r_matrix
                  }
rm(m)

                                    } # close loop hr "FlexParamCurve"

         if(RichCoef$estimator == "bbmle") {
         
hr_Asym<-RichCoef$hr_Asym
hr_K<-RichCoef$hr_K
hr_M<-RichCoef$hr_M
hr_Infl<-RichCoef$hr_Infl
Tupr<-RichCoef$Tupr

month_hr_list <- list()

pb <- txtProgressBar(min = 0, max = 12, style = 3)

for (j in 1:12) {
# This line has the old equation I used in fitting h_a functions
month_hr_list [[j]] <- hr_Asym / ((1 + hr_K * exp(( -hr_Infl * (tmax_stack[[j]]/10 - Tupr))))^(1/hr_M))
setTxtProgressBar(pb, j)
                }
Sys.sleep(1)
close(pb)
rm(j)

## ha rasters montly and year

h_r_montly_raster <- stack(month_hr_list)
h_r_raster_stack_x_10 <- h_r_montly_raster*10
names(h_r_raster_stack_x_10) <- c('h_r_1', 'h_r_2', 'h_r_3', 'h_r_4', 'h_r_5', 'h_r_6', 'h_r_7', 'h_r_8', 'h_r_9', 'h_r_10', 'h_r_11', 'h_r_12')
h_r_raster_stack_x_10[is.na(alt_stack)] <- NA 

h_r_year_raster <- Reduce('+',month_hr_list)
names(h_r_year_raster) <- "h_r_year"
h_r_year_raster[is.na(alt_stack)] <- NA 

#########     h_r matrices

h_r_mat_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_h_r <- subset(h_r_montly_raster, m)
one_month_h_r_matrix <- as.matrix(one_month_h_r)
one_month_h_r_matrix [is.na(one_month_h_r_matrix)] <- 0 
h_r_mat_list [[counter]] <- one_month_h_r_matrix
                  }
rm(m)
                                    } # close loop hr "bbmle"

                               } # close loop for hr

#### HERE is my modification of the EcoPhysRaster for ha

if(!is.na(RichCoef$ha_Asym)) {

         if(RichCoef$estimator == "FlexParamCurve") {

## ha using Richards -- FlexParamCurve

ha_a_Asym <-RichCoef$ha_Asym
ha_c_K    <-RichCoef$ha_K
ha_b_d_M  <-RichCoef$ha_M
ha_i_Infl <-RichCoef$ha_Infl
Tpref      <-RichCoef$Tupr


           # ha calculations

pb <- txtProgressBar(min = 0, max = 12, style = 3)

month_ha_list <- list()
          for (i in 1:12) {
             month_ha_list[[i]] <- ha_a_Asym / ((1 + ha_b_d_M   * exp(-ha_c_K * ( (tmax_stack[[i]]/10-Tpref) - ha_i_Infl)         ))^(1/ha_b_d_M))
             setTxtProgressBar(pb, i)
                          }
Sys.sleep(1)
close(pb)
rm(i)

## ha rasters montly and year

h_a_montly_raster <- stack(month_ha_list)
h_a_raster_stack_x_10 <- h_a_montly_raster*10
names(h_a_raster_stack_x_10) <- c('h_a_1', 'h_a_2', 'h_a_3', 'h_a_4', 'h_a_5', 'h_a_6', 'h_a_7', 'h_a_8', 'h_a_9', 'h_a_10', 'h_a_11', 'h_a_12')
h_a_raster_stack_x_10[is.na(alt_stack)] <- NA 

h_a_year_raster <- Reduce('+',month_ha_list)
names(h_a_year_raster) <- "h_a_year"
h_a_year_raster[is.na(alt_stack)] <- NA 

#########     h_a matrices

h_a_mat_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_h_a <- subset(h_a_montly_raster, m)
one_month_h_a_matrix <- as.matrix(one_month_h_a)
one_month_h_a_matrix [is.na(one_month_h_a_matrix)] <- 0 
h_a_mat_list [[counter]] <- one_month_h_a_matrix
                  }
rm(m)

                                    } # close loop ha "FlexParamCurve"

         if(RichCoef$estimator == "bbmle") {
         
ha_Asym<-RichCoef$ha_Asym
ha_K<-RichCoef$ha_K
ha_M<-RichCoef$ha_M
ha_Infl<-RichCoef$ha_Infl
Tupr<-RichCoef$Tupr

month_ha_list <- list()

pb <- txtProgressBar(min = 0, max = 12, style = 3)

for (j in 1:12) {
# This line has the old equation I used in fitting h_a functions
month_ha_list [[j]] <- ha_Asym / ((1 + ha_K * exp(( -ha_Infl * (tmax_stack[[j]]/10 - Tupr))))^(1/ha_M))
setTxtProgressBar(pb, j)
                }
Sys.sleep(1)
close(pb)
rm(j)


## ha rasters montly and year

h_a_montly_raster <- stack(month_ha_list)
h_a_raster_stack_x_10 <- h_a_montly_raster*10
names(h_a_raster_stack_x_10) <- c('h_a_1', 'h_a_2', 'h_a_3', 'h_a_4', 'h_a_5', 'h_a_6', 'h_a_7', 'h_a_8', 'h_a_9', 'h_a_10', 'h_a_11', 'h_a_12')
h_a_raster_stack_x_10[is.na(alt_stack)] <- NA 

h_a_year_raster <- Reduce('+',month_ha_list)
names(h_a_year_raster) <- "h_a_year"
h_a_year_raster[is.na(alt_stack)] <- NA 

#########     h_a matrices

h_a_mat_list <- list()
counter <- 0

for(m in 1:12)     {
counter <- counter + 1
one_month_h_a <- subset(h_a_montly_raster, m)
one_month_h_a_matrix <- as.matrix(one_month_h_a)
one_month_h_a_matrix [is.na(one_month_h_a_matrix)] <- 0 
h_a_mat_list [[counter]] <- one_month_h_a_matrix
                  }
rm(m)

                                    } # close loop ha "bbmle"
                               } # close loop for ha


# Breeding

if(Breeding==T) {

        if(!any(is.na(disjoint_breeding_months))) {

                            if(is.numeric(disjoint_breeding_months)) {
                                if(exists("h_a_montly_raster")) {
                                    Breeding_ha_raster_all <- subset(h_a_raster_stack_x_10/10, disjoint_breeding_months)
                                    Breeding_ha_raster <- sum(Breeding_ha_raster_all)
                                    names(Breeding_ha_raster) <- "Breeding_ha"
                                    Breeding_ha_raster [is.na(alt_stack)] <- NA 
                                                                  }

                                if(exists("h_r_montly_raster")) {
                                    Breeding_hr_raster_all <- subset(h_r_raster_stack_x_10/10, disjoint_breeding_months)
                                    Breeding_hr_raster <- sum(Breeding_hr_raster_all)
                                    names(Breeding_hr_raster) <- "Breeding_hr"
                                    Breeding_hr_raster [is.na(alt_stack)] <- NA 
                                                                }
                                                                      } # close loop if(is.numeric(disjoint_breeding_months))

                            if(is.character(disjoint_breeding_months)) {
                                if(exists("h_a_montly_raster")) {
                                    Breeding_ha_raster_all <- h_a_raster_stack_x_10/10 * winterPrec_stack
                                    Breeding_ha_raster <- sum(Breeding_ha_raster_all)
                                    names(Breeding_ha_raster) <- "Breeding_ha"
                                    Breeding_ha_raster [is.na(alt_stack)] <- NA 
                                                                  }

                                if(exists("h_r_montly_raster")) {
                                    Breeding_hr_raster_all <- h_r_raster_stack_x_10/10 * winterPrec_stack
                                    Breeding_hr_raster <- sum(Breeding_hr_raster_all)
                                    names(Breeding_hr_raster) <- "Breeding_hr"
                                    Breeding_hr_raster [is.na(alt_stack)] <- NA 
                                                                }
                                                                     } # close loop if(is.character(disjoint_breeding_months))

                                                  }

        if(any(is.na(disjoint_breeding_months))) {

# h_a_mat_list
# h_r_mat_list

## _ha
          if(exists("h_a_montly_raster")) {
            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_ha <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_ha <- StartBreeding_frac*h_a_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13
                                        }

## _hr
          if(exists("h_r_montly_raster")) {
            if(StartBreeding > 0 && StopBreeding < 13) {
Breeding_hr <- if (StartBreeding-StopBreeding < -1) {
                                   print ("StartBreeding-StopBreeding < -1")
                                   print ( c(StartBreeding:StopBreeding) )
                                   total_months_breed <- length (c(StartBreeding:StopBreeding)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac) 
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                                   print ( c(StartBreeding:12, 1) )
                                   total_months_breed <- length (c(StartBreeding:12, 1)) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c(((StartBreeding+1):12)) )))
    } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                                   print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                                   print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                   total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0])) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))
    } else if (StartBreeding-StopBreeding == -1) { 
                                   print ("StartBreeding-StopBreeding == -1")
                                   print (c( StartBreeding:StopBreeding ) )
                                   total_months_breed <- length (c( StartBreeding:StopBreeding ) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                   Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]]
    } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) ))) 
    } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) { 
                                  print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                                  print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                                  total_months_breed <- length (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) ) - (1 - StartBreeding_frac) - (1 - StopBreeding_frac)
                                  Breeding_hr <- StartBreeding_frac*h_r_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  ))) 
    } else {stop (print("Breeding months are NOT OK"))}
                                                 } # closes the if loop: StartBreeding > 0 && StopBreeding < 13
                                     }

# get this one
if(exists("h_a_montly_raster")) {
Breeding_ha_raster <-stack(raster(Breeding_ha, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_ha_raster) <- "Breeding_ha"
Breeding_ha_raster [is.na(alt_stack)] <- NA 
                                }

if(exists("h_r_montly_raster")) {
Breeding_hr_raster <-stack(raster(Breeding_hr, 
           xmn = raster_extend[1],
           xmx = raster_extend[2],
           ymn = raster_extend[3],
           ymx = raster_extend[4],
           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
names(Breeding_hr_raster) <- "Breeding_hr"
Breeding_hr_raster [is.na(alt_stack)] <- NA 
                                }

                                                }  # closes the if loop: is.na(disjoint_breeding_months)

# get this one

if(exists("h_a_montly_raster")) {
non_Breeding_ha_raster <- h_a_year_raster - Breeding_ha_raster
names(non_Breeding_ha_raster) <- "non_Breeding_ha"
                                }

if(exists("h_r_montly_raster")) {
non_Breeding_hr_raster <- h_r_year_raster - Breeding_hr_raster
names(non_Breeding_hr_raster) <- "non_Breeding_hr"
                                }

                                         } # closes the if loop: Breeding

                                }  # Closes the loop : if(hahrmethod == 'heliotherm')


cat("\n---------------------- calculations completed from model: ", model_year, " ----------------------\n")


##########################################################################################
#######                                master stacks                    ##################
##########################################################################################

# general rasters

# raster that has to exists

summer_rh_raster_x_1000 <- summer_rh_raster * 1000
names(summer_rh_raster_x_1000) <- "rh_summer_mean_x_1000"
winter_rh_raster_x_1000 <- winter_rh_raster * 1000
names(winter_rh_raster_x_1000) <- "rh_winter_mean_x_1000"

average_year_rh_raster_x_10000 <- average_year_rh_raster * 10000
names(average_year_rh_raster_x_10000) <- "rh_year_mean_x_10000"

h_a_year_raster_x_100 <- h_a_year_raster * 100
names(h_a_year_raster_x_100) <- "h_a_year_x_100"
h_r_year_raster_x_100 <- h_r_year_raster * 100
names(h_r_year_raster_x_100) <- "h_r_year_x_100"

# if these exist

if(exists("Breeding_rh_raster")) {Breeding_rh_raster_x_1000 <- Breeding_rh_raster * 1000
                            names(Breeding_rh_raster_x_1000) <- "Breeding_rh_raster_x_1000" }

if(exists("Breeding_ha_raster")) {Breeding_ha_raster_x_100 <- Breeding_ha_raster * 100
                            names(Breeding_ha_raster_x_100) <- "Breeding_ha_raster_x_100" }

if(exists("Breeding_hr_raster")) {Breeding_hr_raster_x_100 <- Breeding_hr_raster * 100
                            names(Breeding_hr_raster_x_100) <- "Breeding_hr_raster_x_100" }

if(exists("non_Breeding_rh_raster")) {non_Breeding_rh_raster_x_1000 <- non_Breeding_rh_raster * 1000
                                names(non_Breeding_rh_raster_x_1000) <- "non_Breeding_rh_raster_x_1000" }

if(exists("non_Breeding_ha_raster")) {non_Breeding_ha_raster_x_100 <- non_Breeding_ha_raster * 100
                                names(non_Breeding_ha_raster_x_100) <- "non_Breeding_ha_raster_x_100" }

if(exists("non_Breeding_hr_raster")) {non_Breeding_hr_raster_x_100 <- non_Breeding_hr_raster * 100
                                names(non_Breeding_hr_raster_x_100) <- "non_Breeding_hr_raster_x_100" }

# prec_rh_seansons stacks

one_scenario_rasters_prec_rh_seansons <- stack(SumPrecmatrix_raster, WinPrecmatrix_raster, summer_rh_raster_x_1000, winter_rh_raster_x_1000)


if(Breeding == FALSE) {

if(any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

           if(any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                    one_scenario_rasters_hydrology <- stack(total_PET_raster, total_AET_raster, total_SolarRad_raster/10)
                                    one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                      average_year_rh_raster_x_10000, 
                                                                      h_a_year_raster_x_100, 
                                                                      h_r_year_raster_x_100, 
                                                                      total_AET_raster, 
                                                                      total_Perf_raster)
                                                                           }

           if(!any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                    one_scenario_rasters_hydrology <- stack(total_PET_raster, total_AET_raster, total_SolarRad_raster/10)
                                    one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                      average_year_rh_raster_x_10000, 
                                                                      h_a_year_raster_x_100, 
                                                                      h_r_year_raster_x_100, 
                                                                      total_AET_raster)
                                                                           }

                                                              }

if(!any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

           if(any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                    one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                      average_year_rh_raster_x_10000, 
                                                                      h_a_year_raster_x_100, 
                                                                      h_r_year_raster_x_100, 
                                                                      total_Perf_raster)
                                                                           }

           if(!any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                    one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                      average_year_rh_raster_x_10000, 
                                                                      h_a_year_raster_x_100, 
                                                                      h_r_year_raster_x_100)
                                                                           }

                                                              }
                 } # close loop if(Breeding == FALSE)



if(Breeding == TRUE) {

if(any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

           if(any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                          one_scenario_rasters_hydrology <- stack(total_PET_raster, Breeding_PET_raster, non_Breeding_PET_raster, 
                                                                                  total_AET_raster, Breeding_AET_raster, non_Breeding_AET_raster, 
                                                                                  total_SolarRad_raster/10, Breeding_SolarRad_raster/10, non_Breeding_SolarRad_raster/10)

                                             one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                                    average_year_rh_raster_x_10000, 
                                                                                    h_a_year_raster_x_100, 
                                                                                    h_r_year_raster_x_100, 
                                                                                    total_AET_raster, 
                                                                                    total_Perf_raster, 
                                                                                    Breeding_prec_raster, 
                                                                                    Breeding_rh_raster_x_1000, 
                                                                                    Breeding_ha_raster_x_100, 
                                                                                    Breeding_hr_raster_x_100, 
                                                                                    Breeding_AET_raster, 
                                                                                    Breeding_Perf_raster,
                                                                                    non_Breeding_prec_raster, 
                                                                                    non_Breeding_rh_raster_x_1000, 
                                                                                    non_Breeding_ha_raster_x_100, 
                                                                                    non_Breeding_hr_raster_x_100, 
                                                                                    non_Breeding_AET_raster,
                                                                                    non_Breeding_Perf_raster)
                                                                            }

           if(!any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                          one_scenario_rasters_hydrology <- stack(total_PET_raster, Breeding_PET_raster, non_Breeding_PET_raster, 
                                                                                  total_AET_raster, Breeding_AET_raster, non_Breeding_AET_raster, 
                                                                                  total_SolarRad_raster/10, Breeding_SolarRad_raster/10, non_Breeding_SolarRad_raster/10)

                                             one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                                    average_year_rh_raster_x_10000, 
                                                                                    h_a_year_raster_x_100, 
                                                                                    h_r_year_raster_x_100, 
                                                                                    total_AET_raster, 
                                                                                    Breeding_prec_raster, 
                                                                                    Breeding_rh_raster_x_1000, 
                                                                                    Breeding_ha_raster_x_100, 
                                                                                    Breeding_hr_raster_x_100, 
                                                                                    Breeding_AET_raster, 
                                                                                    non_Breeding_prec_raster, 
                                                                                    non_Breeding_rh_raster_x_1000, 
                                                                                    non_Breeding_ha_raster_x_100, 
                                                                                    non_Breeding_hr_raster_x_100, 
                                                                                    non_Breeding_AET_raster)
                                                                            }

                                                            } # if(any(variables %in% c('Hydrology','hydrology', 'hydro')))

if(!any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

           if(any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                             one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                                    average_year_rh_raster_x_10000, 
                                                                                    h_a_year_raster_x_100, 
                                                                                    h_r_year_raster_x_100, 
                                                                                    total_Perf_raster, 
                                                                                    Breeding_prec_raster, 
                                                                                    Breeding_rh_raster_x_1000, 
                                                                                    Breeding_ha_raster_x_100, 
                                                                                    Breeding_hr_raster_x_100, 
                                                                                    Breeding_Perf_raster,
                                                                                    non_Breeding_prec_raster, 
                                                                                    non_Breeding_rh_raster_x_1000, 
                                                                                    non_Breeding_ha_raster_x_100, 
                                                                                    non_Breeding_hr_raster_x_100, 
                                                                                    non_Breeding_Perf_raster)
                                                                            }

           if(!any(variables %in% c('performance', 'Performance', 'Perf'))) {
                                             one_scenario_rasters_general <- stack( total_prec_raster, 
                                                                                    average_year_rh_raster_x_10000, 
                                                                                    h_a_year_raster_x_100, 
                                                                                    h_r_year_raster_x_100, 
                                                                                    Breeding_prec_raster, 
                                                                                    Breeding_rh_raster_x_1000, 
                                                                                    Breeding_ha_raster_x_100, 
                                                                                    Breeding_hr_raster_x_100, 
                                                                                    non_Breeding_prec_raster, 
                                                                                    non_Breeding_rh_raster_x_1000, 
                                                                                    non_Breeding_ha_raster_x_100, 
                                                                                    non_Breeding_hr_raster_x_100)
                                                                            }

                                                            } # if(!any(variables %in% c('Hydrology','hydrology', 'hydro')))
                 } # close loop if(Breeding == TRUE)

# putting calculated ecophysiology rasters in list of rasters and write those stacks

processed_ecophysiology_rasters [[model_year]] <- one_scenario_rasters_general

one_scenarion_bioclim <-one_scenario_rasters[[grep("bioclim_*", names(one_scenario_rasters))]]
processed_bioclim_rasters [[model_year]] <- one_scenarion_bioclim

# resolution name

if(round(res(one_scenario_rasters_general)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(one_scenario_rasters_general)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(one_scenario_rasters_general)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(one_scenario_rasters_general)[1], 4) == 0.1667) {resolution_name = "10_min"}

# write.raster

writeRaster(one_scenarion_bioclim , filename=paste0(species_name, "_", model_year, "_res_", resolution_name, "_bioclim"), datatype='INT2S', overwrite=TRUE)
writeRaster(one_scenario_rasters_prec_rh_seansons , filename=paste0(species_name, "_", model_year, "_res_", resolution_name, "_precipitation_rh_seansons"), datatype='INT2S', overwrite=TRUE)
writeRaster(one_scenario_rasters_general , filename=paste0(species_name, "_", model_year, "_res_", resolution_name, "_ecophysiology_rasters"), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(one_scenarion_bioclim))

cat("------------- Ecophysiology Raster file properties ------------\n")
cat("grid names: ", names(one_scenarion_bioclim), "\n")
cat("grid units:  see bioclim units \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(one_scenarion_bioclim), "\n")
cat("minimum values: ", minValue(one_scenarion_bioclim), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", model_year, "_res_", resolution_name, "_bioclim"), "\n")
cat("max_longitude: ", raster_extend[2], "\n","min_longitude: ", raster_extend[1], "\n", "max_latitude: ", raster_extend[4], "\n","min_latitude: ",  raster_extend[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")


projection_raster_stack <- capture.output(crs(one_scenario_rasters_prec_rh_seansons))

cat("------------- Ecophysiology Raster file properties ------------\n")
cat("grid names: ", names(one_scenario_rasters_prec_rh_seansons), "\n")
cat("grid units:  mm  mm  rh*10000  rh*10000 \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(one_scenario_rasters_prec_rh_seansons), "\n")
cat("minimum values: ", minValue(one_scenario_rasters_prec_rh_seansons), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", model_year, "_res_", resolution_name, "_precipitation_rh_seansons"), "\n")
cat("max_longitude: ", raster_extend[2], "\n","min_longitude: ", raster_extend[1], "\n", "max_latitude: ", raster_extend[4], "\n","min_latitude: ",  raster_extend[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")

projection_raster_stack <- capture.output(crs(one_scenario_rasters_general))

cat("------------- Ecophysiology Raster file properties ------------\n")
cat("grid names: ", names(one_scenario_rasters_general), "\n")
cat("grid units:  see names \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(one_scenario_rasters_general), "\n")
cat("minimum values: ", minValue(one_scenario_rasters_general), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", model_year, "_res_", resolution_name, "_ecophysiology_rasters"), "\n")
cat("max_longitude: ", raster_extend[2], "\n","min_longitude: ", raster_extend[1], "\n", "max_latitude: ", raster_extend[4], "\n","min_latitude: ",  raster_extend[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")

## hydrology

if(any(variables %in% c('Hydrology','hydrology', 'hydro'))) {

writeRaster(one_scenario_rasters_hydrology , filename=paste0(species_name, "_", model_year, "_res_", resolution_name, "_hydrology_rasters"), datatype='INT2S', overwrite=TRUE)

projection_raster_stack <- capture.output(crs(one_scenario_rasters_hydrology))

cat("------------- Ecophysiology Raster file properties ------------\n")
cat("grid names: ", names(one_scenario_rasters_hydrology), "\n")
cat("grid units:  PET  PET  PET  AET  AET  AET  total_SolarRad/10  total_SolarRad/10  total_SolarRad/10\n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(one_scenario_rasters_hydrology), "\n")
cat("minimum values: ", minValue(one_scenario_rasters_hydrology), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", model_year, "_res_", resolution_name, "_hydrology_rasters"), "\n")
cat("max_longitude: ", raster_extend[2], "\n","min_longitude: ", raster_extend[1], "\n", "max_latitude: ", raster_extend[4], "\n","min_latitude: ",  raster_extend[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")

cat("\n===================== END CALCULATION FOR MODEL: ", model_year, " =========================\n")

                                                              }  # close loop if(any(variables %in% c('Hydrology','hydrology', 'hydro')))

##########################################################################################

                                           } # close the loop for(N_scenario in 1:length(scenario_names))

##########################################################################################

processed_grids_list <- list(bioclim = processed_bioclim_rasters, ecophysiology= processed_ecophysiology_rasters)

setwd(master_directory)

return(processed_grids_list)

            } # END FUNCTION

##############                          END of function                       ############
##########################################################################################

# use pb
# cat("\n---------------------- Started ha hr under 'heliotherm mode' calculations using FlexParamCurve parametrization for model: ", model_year, " ----------------------\n")
# pb <- txtProgressBar(min = 0, max = 12, style = 3)
# for (i in 1:12) { setTxtProgressBar(pb, i)}
# Sys.sleep(1)
# close(pb)
# cat("\n---------------------- Started ha under 'heliotherm mode' calculations using FlexParamCurve parametrization for model: ", model_year, " ----------------------\n")
# pb <- txtProgressBar(min = 0, max = 12, style = 3)
# setTxtProgressBar(pb, j)                }
# Sys.sleep(1)
# close(pb)
# rm(j)

##########################################################################################
######################            Selection by correlation         #######################
##########################################################################################

colinearity_test_correlation <- function (input_stack_value, 
                                              spcoord_value, 
                                correlation_threshold_value = 0.75, 
                                     path_output_dir_value = NULL) {

# require libraries
require('raster')

# input from user

input_stack_file <- input_stack_value
sp_coord_file <- spcoord_value
coor_threshold <- correlation_threshold_value
output_dir <- path_output_dir_value

master_directory <- getwd()

# read stacks and spcoordinates from file or data.frame

if(class(input_stack_file)[1] == "RasterBrick") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "RasterStack") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "character") { input_stack <- stack(input_stack_file)}

if(class(sp_coord_file) == "data.frame") {
                             species_coordinates_1 <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- try(unique(species_coordinates_1$species), silent = T)
if(is.null(species_name)) { stop ("please add the species name to the coordinates file as a column: species") }

species_coordinates <- data.matrix(species_coordinates_1[c("Lon","Lat")])

###################            create dir to write output            #########################

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                                    }

###############       extract values from raster and eliminate incomplete rows    ################

presvals_species_raw <- extract(input_stack, species_coordinates) 
presvals_species <- na.omit(presvals_species_raw)

## FUNCTIONS REQUIRED FOR CORRELATION ANALYSES

# correlation probability function (for continuous):

cor.prob <- function (X, dfr = nrow(X) - 2) {
             R <- cor(X, use="pairwise.complete.obs")
             above <- row(R) < col(R)
             r2 <- R[above]^2
             Fstat <- r2 * dfr/(1 - r2)
             R[above] <- 1 - pf(Fstat, 1, dfr)
             R[row(R) == col(R)] <- NA
             R
}

##correlation probability function (for continuous):

cor.prob_kendall <- function (X, dfr = nrow(X) - 2) {
                                                     R <- cor(X, method = "kendall", use="pairwise.complete.obs")
                                                     above <- row(R) < col(R)
                                                     r2 <- R[above]^2
                                                     Fstat <- r2 * dfr/(1 - r2)
                                                     R[above] <- 1 - pf(Fstat, 1, dfr)
                                                     R[row(R) == col(R)] <- NA
                                                     R
                                                     }

## Use this to dump the cor.prob output to a 4 column matrix

flattenSquareMatrix <- function(m) {
                                    if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
                                    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
                                    ut <- upper.tri(m)
                                    data.frame(i = rownames(m)[row(m)[ut]],
                                    j = rownames(m)[col(m)[ut]],
                                    R_cor=t(m)[ut],
                                    p_value=m[ut])
                                    }

##  Correlation function

test_correlation_sdm <- function (x) {
                                      test_correlation_df <- x
                                     scaled_variables_sdm <- scale(test_correlation_df)
                     cor_scaled.test_coli_bioclim_flatten <- flattenSquareMatrix(cor.prob(scaled_variables_sdm))
                                               t_corr_all <- transform(cor_scaled.test_coli_bioclim_flatten, abs_R_cor = abs(cor_scaled.test_coli_bioclim_flatten$R_cor))
                                                   t_corr <- subset(t_corr_all, coor_threshold < abs(t_corr_all$R_cor))
                                                   return(t_corr)
                                     }

# correlation results

t_corr <- test_correlation_sdm (presvals_species)

prec_factors <- sapply(t_corr, is.factor)
t_corr[prec_factors] <- lapply(t_corr[prec_factors], as.character)

# unique in either column

i_unique <- unique(t_corr$i)
j_unique <- unique(t_corr$j)

predictors_with_high_correlations <- unique(c(i_unique, j_unique))

# select iteralitvely one predictor and elimitane all other rows that have that predictor

df_1 <- t_corr
selected_predictors <- c()
counter <- 0

while(!is.na(df_1$i[1])) {
                          counter <- counter + 1
                          one_predictor <- as.character(df_1$i[1])
                          df_2 <- df_1[df_1[, "i"] == one_predictor,] # remove rows that have that predictor
                          df_2_redundant_predictors <- c(df_2$j)
                          too_remove <- c(one_predictor, df_2_redundant_predictors)
                          df_1 <- df_1[!df_1[, "i"] %in% too_remove,] # remove rows that have that predictor
                          df_1 <- df_1[!df_1[, "j"] %in% too_remove,] # remove rows that have that predictor
                          selected_predictors [counter] <- one_predictor
                          }

predictors_selected_high_correlations <- selected_predictors

# list of variables with low correlation with the others

all_predictors <- names(input_stack)

predictors_with_no_high_correlations <- setdiff(all_predictors, predictors_with_high_correlations)

# end predictors

end_predictors <- c(predictors_selected_high_correlations, predictors_with_no_high_correlations)

# list of results

end_predictors_list <- list(species_name = species_name, collinearity_analyses_results = t_corr, selected_predictors_by_correlation = end_predictors)

##write table

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              writeLines(capture.output(end_predictors_list),con=paste0(species_name, "_colinearity_results_using_correlations.txt"))
                                    }

# return to master directory

setwd(master_directory)

# return selected layers

return(end_predictors_list)

                   }  # END FUNCTION

##############                          END of function                       ############
##########################################################################################

##########################################################################################
######################            Selection by RF and GBM        #########################
##########################################################################################


colinearity_RF_GBM_predictors_test <- function (input_stack_value, 
                                             species_coordinates_value, 
                                             true_absences_value = NULL,
                                             circles_diameter_km = 150, 
                                             pseudoabsences_samples_N_value = 2000,
                                             ntrees_GBM_RF_value = 5000, 
                                             quantile_threshold_value = 0.3,
                                             method_value = "RF",
                                             path_output_dir_value) {

# require
require('dismo')
require('raster')
require('rgeos')
require('randomForest')

# input from user

input_stack_file <- input_stack_value
sp_coord_file <- species_coordinates_value
true_absences <- true_absences_value
circles_diameter <- circles_diameter_km
pseudoabsences_samples_N <- pseudoabsences_samples_N_value
quantile_threshold <- quantile_threshold_value
ntrees_GBM_RF <- ntrees_GBM_RF_value 
method <- method_value

output_dir <- path_output_dir_value

master_directory <- getwd()

# read stacks and spcoordinates from file or data.frame

if(class(input_stack_file)[1] == "RasterBrick") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "RasterStack") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "character") { input_stack <- stack(input_stack_file)}

if(class(sp_coord_file) == "data.frame") {
                             species_coordinates_1 <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- try(unique(species_coordinates_1$species), silent = T)
if(is.null(species_name)) { stop ("please add the species name to the coordinates file as a column: species") }

species_coordinates <- data.matrix(species_coordinates_1[c("Lon","Lat")])

#############################       Presences first   ##############################

# extract values from raster and eliminate incomplete rows

presvals_species_raw <- extract(input_stack, species_coordinates) # y is the enviromental brick
presvals_species <- na.omit(presvals_species_raw)

# trim presences to the extent of raster

extent_input_stack <- extent(input_stack)

species_coordinates_subset <- species_coordinates[species_coordinates[,1] > extent_input_stack[1],]
species_coordinates_subset <- species_coordinates_subset[species_coordinates_subset[,1] < extent_input_stack[2],]
species_coordinates_subset <- species_coordinates_subset[species_coordinates_subset[,2] > extent_input_stack[3],]
species_coordinates_subset <- species_coordinates_subset[species_coordinates_subset[,2] < extent_input_stack[4],]

# generate pseudoabsences: prepare projection

pseudo_abs_lon_lat <- as.data.frame(species_coordinates_subset, stringsAsFactors = FALSE)
coordinates(pseudo_abs_lon_lat) <- ~Lon+Lat
projection(pseudo_abs_lon_lat) <- CRS('+proj=longlat')

# generate pseudoabsences: create polygons

pseudo_abs_lon_lat_circles <- circles(pseudo_abs_lon_lat, d=circles_diameter*1000, lonlat=TRUE) 
pseudo_abs_lon_lat_pol <- gUnaryUnion(pseudo_abs_lon_lat_circles@polygons)

# generate pseudoabsences: number of pseudoabsences samples

pseudo_abs_lon_lat_samp1 <- spsample(pseudo_abs_lon_lat_pol, pseudoabsences_samples_N, type='random', iter=25) 
pseudo_abs_cells <- cellFromXY(input_stack, pseudo_abs_lon_lat_samp1) 
pseudo_abs_cells <- unique(pseudo_abs_cells)
pseudo_abs_cells_xy <- xyFromCell(input_stack, pseudo_abs_cells) 

pseudo_abs_cells_spxy <- SpatialPoints(pseudo_abs_cells_xy, proj4string=CRS('+proj=longlat'))
pseudo_abs_o <- over(pseudo_abs_cells_spxy, pseudo_abs_lon_lat_circles@polygons)
pseudo_abs_xyInside <- pseudo_abs_cells_xy[!is.na(pseudo_abs_o), ]
psedo_backgr <-pseudo_abs_xyInside 

psedo_coordinates <- psedo_backgr
colnames(psedo_coordinates) <- c("Lon", "Lat")

psedo_backgr_br <- extract(input_stack, psedo_backgr) # k is the enviromental brick
psedo_backgr_br <- unique(psedo_backgr_br) # remove duplicated rows 
psedo_backgr_br <- as.data.frame(psedo_backgr_br) # convert to data frame
psedo_backgr_br <- na.omit(psedo_backgr_br) # remove rows with missing values

presvals_pseudo <- psedo_backgr_br

# combine_pseudo_presences

pb <- c(rep(1, nrow(presvals_species)), rep(0, nrow(presvals_pseudo)))
sdmdata <- data.frame(cbind(pb, rbind(presvals_species, presvals_pseudo)))

################                   add true absences if present

                      if(!is.null(true_absences)) {

if(class(true_absences) == "data.frame") {
                             true_abs <- true_absences
                                   } else {
if(grepl(pattern = "*.csv$", true_absences) == TRUE) {true_abs <- read.table(file = true_absences, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", true_absences) == TRUE) {true_abs <- read.table(file = true_absences, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

true_abs_coordinates <- data.matrix(true_abs[c("Lon","Lat")])
true_abs_raw <- extract(input_stack, true_abs_coordinates)
true_abs_matrix <- na.omit(true_abs_raw)
true_abs_df <- data.frame(pb=0, true_abs_matrix, stringsAsFactors = F)

sdmdata <- rbind(sdmdata, true_abs_df)

                                                   }

# end input variable process

sdmdata_pres_abs <- na.omit(sdmdata)

#######################        variable selection process        #########################

if(method %in% c("RF", "random forest", "random_forest", "RandomForest", "Random_Forest")) { 

# estimation using gbm_rf_sdm_importance

test_correlation_df <- sdmdata_pres_abs[, -1] #smd data and will eliminate pb (presence/absence)
scaled_variables_sdm <- scale(test_correlation_df)
RF_data <- data.frame(sdmdata_pres_abs$pb, scaled_variables_sdm)

colnames(RF_data)[1] <- "pb" #renames only the column name appended pb

################################### random forest ########################################

rf_results <- randomForest(sdmdata_pres_abs$pb~.,data=sdmdata_pres_abs[,-1],ntree=ntrees_GBM_RF, mtry=20, importance = T, na.action = na.exclude)

#Determine importance for RF

sdmdata_pb_rf_imp <- as.data.frame(importance(rf_results))
sdmdata_pb_rf_imp$var <- row.names(sdmdata_pb_rf_imp)
sdmdata_pb_rf_imp$rel.influence <- sdmdata_pb_rf_imp$IncNodePurity/sum(sdmdata_pb_rf_imp$IncNodePurity)*100
sdmdata_pb_rf_imp$IncNodePurity <- NULL

##merge RF relative importance, order, and name

sdmdata_pb_rf_imp <- sdmdata_pb_rf_imp[order(-sdmdata_pb_rf_imp$rel.influence),]
names(sdmdata_pb_rf_imp) <- c("%IncMSE", "raster_predictor","RF_Rel_Influ")

## select predictors by quantile threshold on influence vector

RF_vector_influence <- sdmdata_pb_rf_imp$RF_Rel_Influ
RF_vector_influence_quantile_value <- quantile(RF_vector_influence,  probs = quantile_threshold)

selected_RF_predictors <- subset(sdmdata_pb_rf_imp, RF_Rel_Influ >= RF_vector_influence_quantile_value,  select = raster_predictor)
selected_RF_predictors_char <- as.vector(as.matrix(selected_RF_predictors))

# report results

importance_rf <- sdmdata_pb_rf_imp
selected_predictors_rf <- selected_RF_predictors_char
results_list_rf <- list(species_name = species_name, collinearity_analyses_results_rf = importance_rf, selected_predictors = selected_predictors_rf)

                                      }  # end loop if(method %in% c("RF", "random forest", "random_forest", "RandomForest", "Random_Forest"))

############################    GBM: multiple regression    ##############################

if(method %in% c("GBM")) { 

###create the full regression

gbm_VIF_coli_bioclim_regression <- gbm::gbm(sdmdata_pres_abs$pb~.,data=sdmdata_pres_abs[,-1], distribution="gaussian", n.trees=ntrees_GBM_RF,  shrinkage=0.05, interaction.depth=1)

gbm_summary <- gbm::summary.gbm(gbm_VIF_coli_bioclim_regression, plotit=FALSE)

### names and save output

names(gbm_summary) <- c("raster_predictor","GBM_Rel_Influ")

## select predictors by quantile threshold on influence vector

GBM_vector_influence <- gbm_summary$GBM_Rel_Influ
GBM_vector_influence_quantile_value <- quantile(GBM_vector_influence,  probs = quantile_threshold)

selected_GBM_predictors <- subset(gbm_summary, GBM_Rel_Influ >= GBM_vector_influence_quantile_value,  select = raster_predictor)
selected_GBM_predictors_char <- as.vector(as.matrix(selected_GBM_predictors))

# report results

importance_gbm <- gbm_summary
selected_predictors_gbm <- selected_GBM_predictors_char
results_list_gbm <- list(species_name = species_name, collinearity_analyses_results_gbm = importance_gbm, selected_predictors = selected_predictors_gbm)

                                      }  # end loop if(method %in% c("GBM"))

##########################################################################################

if(method %in% c("GBM")) { 

cat("\n------------------------------ GBM: multiple regression  -------------------------------\n")

print(results_list_gbm)

cat("\n----------------------------------------------------------------------------------------\n")

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              writeLines(capture.output({
                              cat("\n------------------------------ GBM: multiple regression  -------------------------------\n")
                              print(results_list_gbm)
                              cat("\n----------------------------------------------------------------------------------------\n")
                                                        }),con=paste(species_name, "_selected_predictors_GBM.txt", sep=""))
                               }
                            }

###########################################################################################

if(method %in% c("RF", "random forest", "random_forest", "RandomForest", "Random_Forest")) { 

cat("\n------------------------------ Random forest: multiple regression  ---------------------\n")

print(results_list_rf)

cat("\n----------------------------------------------------------------------------------------\n")

###################            create dir to write output            #########################
if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              writeLines(capture.output({
                              cat("\n------------------------------ Random forest: multiple regression  ---------------------\n")
                              print(results_list_rf)
                              cat("\n----------------------------------------------------------------------------------------\n")
                                                        }),con=paste(species_name, "_selected_predictors_RF.txt", sep=""))
                               }
                            }

# return to master directory

setwd(master_directory)

# return selected layers

if(method %in% c("GBM")) { return(results_list_gbm) }

if(method %in% c("RF", "random forest", "random_forest", "RandomForest", "Random_Forest")) { return(results_list_rf) }

                                             }

##############                          END of function                       ############
##########################################################################################

colinearity_PCA_test <- function (input_stack_value, 
                              spcoord_value, 
                              path_output_dir_value) {

# required packages
require('raster')
require('psych')

# input from user

input_stack_file <- input_stack_value
sp_coord_file <- species_coordinates_value
output_dir <- path_output_dir_value

master_directory <- getwd()

# read stacks and spcoordinates from file or data.frame

if(class(input_stack_file)[1] == "RasterBrick") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "RasterStack") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "character") { input_stack <- stack(input_stack_file)}

if(class(sp_coord_file) == "data.frame") {
                             species_coordinates_1 <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- try(unique(species_coordinates_1$species), silent = T)
if(is.null(species_name)) { stop ("please add the species name to the coordinates file as a column: species") }

species_coordinates <- data.matrix(species_coordinates_1[c("Lon","Lat")])

##########################################################################################

presvals_species_raw <- extract(input_stack, species_coordinates) # y is the enviromental brick
presvals_species <- na.omit(presvals_species_raw)

# number of component to extract with Verlicer MAP

analyses_components <- vss(presvals_species, n = length(names(input_stack))-1, rotate = "varimax", diagonal = FALSE, fm = "minres",plot=FALSE)
analyses_components_vector <- capture.output(analyses_components)
MAP_line <- analyses_components_vector[grep(pattern= "*MAP*", analyses_components_vector)]
n_components_MAP <- as.numeric(gsub("[^\\d]+", "", MAP_line, perl=TRUE)) # gets only the number

# get PCA with varimax rotation with the set of components

PCA_results <- principal(presvals_species, nfactors=n_components_MAP, rotate="varimax")

PCA_summary <- capture.output(PCA_results)

# detach required packages

detach("package:psych", unload=TRUE)

# select the highest loading predictors for n_components_MAP
# nice function 'unclass' allows to break output to transforme to data frame

PCA_results_df <- as.data.frame(abs(round(unclass(PCA_results$loadings), 4)))
PCA_results_df$predictor <- rownames(PCA_results_df)

select_predictors_list <- list()

for (i in 1:(ncol(PCA_results_df)-1)) {
                                           temp <- PCA_results_df[rev(order(PCA_results_df[[i]])),]
                    select_predictors_list[[i]] <- head(temp$predictor,n=1)
                                      }
rm(i)

# save the selected predictors

save_predictor_vector <- unique(unlist(select_predictors_list))

# set output directory and write rasters

pca_results_list <- list(pca_varimax = PCA_results, selected_predictors = save_predictor_vector)

##write table

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              writeLines(capture.output({
                              cat("\n------------------------------ PCA with varimax roation  -------------------------------\n")
                              print(pca_results_list)
                              cat("\n----------------------------------------------------------------------------------------\n")
                                                        }),con=paste(species_name, "_selected_predictors_PCA.txt", sep=""))
                               }

# return to master directory

setwd(master_directory)

# return selected layers

return(pca_results_list)

                                       }

##############                          END of function                       ############
##########################################################################################

colinearity_VIF_test <- function (input_stack_value, 
                              threshold_vif_value = 10,
                              spcoord_value, 
                              path_output_dir_value = NULL) {

require('raster')
require('usdm')

# input from user

input_stack_file <- input_stack_value
threshold_vif <- threshold_vif_value
sp_coord_file <- spcoord_value
output_dir <- path_output_dir_value

master_directory <- getwd()

# read stacks and spcoordinates from file or data.frame

if(class(input_stack_file)[1] == "RasterBrick") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "RasterStack") { input_stack <- input_stack_file }
if(class(input_stack_file)[1] == "character") { input_stack <- stack(input_stack_file)}

if(class(sp_coord_file) == "data.frame") {
                             species_coordinates_1 <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {species_coordinates_1 <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- try(unique(species_coordinates_1$species), silent = T)
if(is.null(species_name)) { stop ("please add the species name to the coordinates file as a column: species") }

# vif analyses

vif_output <- vifstep(input_stack,th=threshold_vif)

# detach required packages

detach("package:usdm", unload=TRUE)

# outersect function


outersect <- function(x, y) {
                            sort(c(setdiff(x, y),
                            setdiff(y, x)))
                            }

# outsect data

vif_included <- outersect(vif_output@variables, vif_output@excluded)

# set output directory and write rasters

vif_results_list <- list(vif_output = vif_output, selected_predictors = vif_included)

##write table

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              writeLines(capture.output({
                              cat("\n-------------------------------------  Variance Inflation Factor (VIF) stepwise procedure  -------------------------------\n")
                              print(vif_results_list)
                              cat("\n--------------------------------------------------------------------------------------------------------------------------\n")
                                                        }),con=paste(species_name, "_selected_predictors_VIF.txt", sep=""))
                               }

# return to master directory

setwd(master_directory)

# return selected layers

return(vif_results_list)

                                       }

##############                          END of function                       ############
##########################################################################################



##########################################################################################
# colinearity.test #######################################################################
##########################################################################################

colinearity.test<-function(inputvar, spcoord, method, 
				
				#arguments for method='corr'
				
				corr_threshold=0.75, 
				
				#arguments for method='RF'
				
				type_absences = "pseudoabsences",
				quantile_threshold=0.3,  
				circles_diameter = 150, 
                pseudoabsences_samples = 2000,
                ntrees_GBM_RF = 5000,
                selection_criteria='RF',
                true_absences=NULL,
                
                #arguments for method='VIF'
                
                threshold_vif = 10){

require('psych')
require('usdm')
require('rgeos')
require('randomForest')
	
##########################################################################################
######################            Selection by correlation         #######################
##########################################################################################

if(method=='corr'){
	
# extract values from raster and eliminate incomplete rows

coord<-subset(spcoord, select =c(Lon, Lat))

presvals_species_raw <- extract(inputvar, coord) # y is the enviromental brick
presvals_species <- na.omit(presvals_species_raw)

## FUNCTIONS REQUIRED FOR CORRELATION ANALYSES

# correlation probability function (for continuous):

cor.prob <- function (X, dfr = nrow(X) - 2) {
             R <- cor(X, use="pairwise.complete.obs")
             above <- row(R) < col(R)
             r2 <- R[above]^2
             Fstat <- r2 * dfr/(1 - r2)
             R[above] <- 1 - pf(Fstat, 1, dfr)
             R[row(R) == col(R)] <- NA
             R
}

##correlation probability function (for continuous):

cor.prob_kendall <- function (X, dfr = nrow(X) - 2) {
                                                     R <- cor(X, method = "kendall", use="pairwise.complete.obs")
                                                     above <- row(R) < col(R)
                                                     r2 <- R[above]^2
                                                     Fstat <- r2 * dfr/(1 - r2)
                                                     R[above] <- 1 - pf(Fstat, 1, dfr)
                                                     R[row(R) == col(R)] <- NA
                                                     R
                                                     }

## Use this to dump the cor.prob output to a 4 column matrix

flattenSquareMatrix <- function(m) {
                                    if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
                                    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
                                    ut <- upper.tri(m)
                                    data.frame(i = rownames(m)[row(m)[ut]],
                                    j = rownames(m)[col(m)[ut]],
                                    R_cor=t(m)[ut],
                                    p_value=m[ut])
                                    }

##  Correlation function

test_correlation_sdm <- function (x) {
                                      test_correlation_df <- x
                                     scaled_variables_sdm <- scale(test_correlation_df)
                     cor_scaled.test_coli_bioclim_flatten <- flattenSquareMatrix(cor.prob(scaled_variables_sdm))
                                               t_corr_all <- transform(cor_scaled.test_coli_bioclim_flatten, abs_R_cor = abs(cor_scaled.test_coli_bioclim_flatten$R_cor))
                                                   t_corr <- subset(t_corr_all, corr_threshold > abs(t_corr_all$R_cor))
                                     }

# correlation results

t_corr <- test_correlation_sdm (presvals_species)

# unique in either column

i_unique <- unique(as.character(t_corr$i))
j_unique <- unique(as.character(t_corr$j))

variables_for_analyses <- intersect(i_unique, j_unique)

return(list(t_corr,variables_for_analyses))
	
}

##########################################################################################
######################            Selection by RF and GBM        #########################
##########################################################################################

####Not Ready	
if(method=='RF'){
	


########################       Does df have true absence def by "pb"  ####################

if(!is.null(true_absences)) {

#############################       True absences process   ##############################


# separate pb info from coordinates

pb <- c(rep(1, nrow(coord)), rep(0, nrow(true_absences)))
presence_true_absence_coord <- rbind(subset(coord, select =c(Lon, Lat)),subset(true_absences, select =c(Lon, Lat)))

# extract raster values

pres_true_absence_raster_values <- extract(inputvar, presence_true_absence_coord)

# combine pb with extracted values

sdmdata_pres_true_abs <- data.frame(cbind(pb, pres_true_absence_raster_values))
sdmdata_pres_abs <- na.omit(sdmdata_pres_true_abs)

##########################       END: True absences process   ############################

                                    } else {

#############################       Presences only process   ##############################

# extract values from raster and eliminate incomplete rows

presvals_species_raw <- extract(inputvar, coord) # y is the enviromental brick
presvals_species <- na.omit(presvals_species_raw)

# generate pseudoabsences: prepare projection

pseudo_abs_lon_lat <- coord
coordinates(pseudo_abs_lon_lat) <- ~Lon+Lat
projection(pseudo_abs_lon_lat) <- CRS('+proj=longlat')

# generate pseudoabsences: create polygons

pseudo_abs_lon_lat_circles <- circles(pseudo_abs_lon_lat, d=circles_diameter*1000, lonlat=TRUE) 
pseudo_abs_lon_lat_pol <- gUnaryUnion(pseudo_abs_lon_lat_circles@polygons)

# generate pseudoabsences: number of pseudoabsences samples

pseudo_abs_lon_lat_samp1 <- spsample(pseudo_abs_lon_lat_pol, pseudoabsences_samples_N, type='random', iter=25) 
pseudo_abs_cells <- cellFromXY(inputvar, pseudo_abs_lon_lat_samp1) # inputvar is the bioclim stack
pseudo_abs_cells <- unique(pseudo_abs_cells)
pseudo_abs_cells_xy <- xyFromCell(inputvar, pseudo_abs_cells) 

pseudo_abs_cells_spxy <- SpatialPoints(pseudo_abs_cells_xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
pseudo_abs_o <- over(pseudo_abs_cells_spxy, pseudo_abs_lon_lat_circles@polygons)
pseudo_abs_xyInside <- pseudo_abs_cells_xy[!is.na(pseudo_abs_o), ]
psedo_backgr <-pseudo_abs_xyInside 

psedo_coordinates <- psedo_backgr
colnames(psedo_coordinates) <- c("Lon", "Lat")

psedo_backgr_br <- extract(inputvar, psedo_backgr) # k is the enviromental brick
psedo_backgr_br <- unique(psedo_backgr_br) # remove duplicated rows 
psedo_backgr_br <- as.data.frame(psedo_backgr_br) # convert to data frame
psedo_backgr_br <- na.omit(psedo_backgr_br) # remove rows with missing values

presvals_pseudo <- psedo_backgr_br

# combine_pseudo_presences

pb <- c(rep(1, nrow(presvals_species)), rep(0, nrow(presvals_pseudo)))
sdmdata <- data.frame(cbind(pb, rbind(presvals_species, presvals_pseudo)))
sdmdata_pres_abs <- na.omit(sdmdata)

#########################       END: Presences only process       ########################

                                            }

#######################        variable selection process        #########################

# estimation using gbm_rf_sdm_importance

test_correlation_df <- sdmdata_pres_abs #smd data and will eliminate pb (presence/absence)
scaled_variables_sdm <- scale(test_correlation_df)
RF_data <- data.frame(sdmdata_pres_abs$pb, scaled_variables_sdm)

colnames(RF_data)[1] <- "pb" #renames only the column name appended pb

################################### random forest ########################################

# rf_tune <- tuneRF(x=sdmdata_pres_abs[,-1],y= sdmdata_pres_abs$pb, stepFactor=1.5, ntreeTry=500, improve=0.01, plot =FALSE)

rf_results <- randomForest(sdmdata_pres_abs$pb~.,data=sdmdata_pres_abs[,-1],ntree=ntrees_GBM_RF, mtry=20, importance = T, na.action = na.exclude)

#Determine importance for RF

sdmdata_pb_rf_imp <- as.data.frame(importance(rf_results))
sdmdata_pb_rf_imp$var <- row.names(sdmdata_pb_rf_imp)
sdmdata_pb_rf_imp$rel.influence <- sdmdata_pb_rf_imp$IncNodePurity/sum(sdmdata_pb_rf_imp$IncNodePurity)*100
sdmdata_pb_rf_imp$IncNodePurity <- NULL

##merge RF relative importance, order, and name

sdmdata_pb_rf_imp <- sdmdata_pb_rf_imp[order(-sdmdata_pb_rf_imp$rel.influence),]
names(sdmdata_pb_rf_imp) <- c("%IncMSE", "raster_predictor","RF_Rel_Influ")

## select predictors by quantile threshold on influence vector

RF_vector_influence <- sdmdata_pb_rf_imp$RF_Rel_Influ
RF_vector_influence_quantile_value <- quantile(RF_vector_influence,  probs = quantile_threshold)

selected_RF_predictors <- subset(sdmdata_pb_rf_imp, RF_Rel_Influ >= RF_vector_influence_quantile_value,  select = raster_predictor)
selected_RF_predictors_char <- as.vector(as.matrix(selected_RF_predictors))

############################    GBM: multiple regression    ##############################

###create the full regression

gbm_VIF_coli_bioclim_regression <- gbm::gbm(sdmdata_pres_abs$pb~.,data=sdmdata_pres_abs[,-1], distribution="gaussian", n.trees=ntrees_GBM_RF,  shrinkage=0.05, interaction.depth=1)

gbm_summary <- gbm::summary.gbm(gbm_VIF_coli_bioclim_regression, plotit=FALSE)

### names and save output

names(gbm_summary) <- c("raster_predictor","GBM_Rel_Influ")

## select predictors by quantile threshold on influence vector

GBM_vector_influence <- gbm_summary$GBM_Rel_Influ
GBM_vector_influence_quantile_value <- quantile(GBM_vector_influence,  probs = quantile_threshold)

selected_GBM_predictors <- subset(gbm_summary, GBM_Rel_Influ >= GBM_vector_influence_quantile_value,  select = raster_predictor)
selected_GBM_predictors_char <- as.vector(as.matrix(selected_GBM_predictors))

##################################  Union/ Intersection/ One #################################

if (selection_criteria == 'union') {
                                   selection_criteria_out <- union(selected_RF_predictors_char, selected_GBM_predictors_char)
                                   }
if (selection_criteria == 'intersection') {
                                   selection_criteria_out <- intersect(selected_RF_predictors_char, selected_GBM_predictors_char)
                                   }
if (selection_criteria == 'RF') {
                                   selection_criteria_out <- selected_RF_predictors_char
                                   }
if (selection_criteria == 'GBM') {
                                   selection_criteria_out <- selected_GBM_predictors_char
                                   }

return(selection_criteria_out)
	
}

##########################################################################################
######################               Selection by PCA              #######################
##########################################################################################

if(method=='PCA'){
	
# extract values from raster and eliminate incomplete rows

presvals_species_raw <- extract(inputvar, spcoord) # y is the enviromental brick
presvals_species <- na.omit(presvals_species_raw)

# number of component to extract with Verlicer MAP

analyses_components <- vss(presvals_species, n = length(names(inputvar))-1, rotate = "varimax", diagonal = FALSE, fm = "minres",plot=FALSE)
analyses_components_vector <- capture.output(analyses_components)
MAP_line <- analyses_components_vector[grep(pattern= "*MAP*", analyses_components_vector)]
n_components_MAP <- as.numeric(gsub("[^\\d]+", "", MAP_line, perl=TRUE)) # gets only the number

# get PCA with varimax rotation with the set of components

PCA_results <- principal(presvals_species, nfactors=n_components_MAP, rotate="varimax")

PCA_summary <- capture.output(PCA_results)

# select the highest loading predictors for n_components_MAP
# nice function 'unclass' allows to break output to transforme to data frame

PCA_results_df <- as.data.frame(abs(round(unclass(PCA_results$loadings), 4)))
PCA_results_df$predictor <- rownames(PCA_results_df)

select_predictors_list <- list()

for (i in 1:(ncol(PCA_results_df)-1)) {
                                           temp <- PCA_results_df[rev(order(PCA_results_df[[i]])),]
                    select_predictors_list[[i]] <- head(temp$predictor,n=1)
                                      }

# save the selected predictors

save_predictor_vector <- unique(unlist(select_predictors_list))

return(list(PCA_results_df,save_predictor_vector))
	
}

##########################################################################################
############    Selection by VIF (Variance Inflation Factor)    ##########################
##########################################################################################

if(method=='VIF'){
	
# vif analyses

vif_output <- vifstep(inputvar,th=threshold_vif)

# outersect function


outersect <- function(x, y) {
                            sort(c(setdiff(x, y),
                            setdiff(y, x)))
                            }

# outsect data

vif_included <- outersect(vif_output@variables, vif_output@excluded)

return(list(vif_output,vif_included))
	
}

}

#############################################################################################################################
#Mapinguari_sdm_JCS##########################################################################################################
#############################################################################################################################

Mapinguari_sdm_using_biomod2 <-            function (environmental_stack_dir_value,
                                                         type_rasters_name_value,
                                                         future_models_to_project_value,
                                                         subset_predictors_value = NULL,
                                                         species_coordinates_value,
                                                         model_value = c("GLM", "GAM", "ANN", "MAXENT"),
                                                         gam_k_value = 7,
                                                         PA_nb_rep_value = 3,
                                                         n_p_absences_value,
                                                         PA_dist_min_km_value = 50,
                                                         PA_dist_max_km_value = 200,
                                                         DataSplit_percent_value = 75,
                                                         NbRunEval_runs_value = 3,
                                                         threshold_EnsembleModel_value = 0.5,
                                                         path_output_dir_value) {

# require function

require("biomod2")
# install_version(package ="slam", version = "0.1-32", repos = "http://cran.us.r-project.org")

# input from user

environmental_stack_dir <- environmental_stack_dir_value
type_rasters <- type_rasters_name_value
future_models <- future_models_to_project_value
subset_predictors <- subset_predictors_value
sp_coord_file <- species_coordinates_value

model <- model_value

gam_k_mgcv <- gam_k_value
gam_k_gam <- gam_k_value

PA_nb_rep <- PA_nb_rep_value
n_p_absences <- n_p_absences_value
PA_dist_min_km <- PA_dist_min_km_value
PA_dist_max_km <- PA_dist_max_km_value

DataSplit_percent <- DataSplit_percent_value
NbRunEval_runs <- NbRunEval_runs_value

threshold_EnsembleModel <- threshold_EnsembleModel_value

path_output_dir <-path_output_dir_value

master_directory <- getwd()

# gel list of rasters assigning rasters
##########################################################################################

if(type_rasters %in% c("bioclim", "Bioclim", "bioclimatic", "Bioclimatic")) {

environmental_present_raw <- stack(list.files(path = environmental_stack_dir, pattern="*.present.*bioclim.gri$",full.names=T, ignore.case=T))

if(!is.null(subset_predictors)) {present_layers <- subset(environmental_present_raw, subset_predictors)
                                    } else {present_layers <- environmental_present_raw}

                                       future_layers_list <- list ()
if("2050_26_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_26_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_26_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_26_rpc"]] <- temp_future}}
if("2050_45_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_45_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_45_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_45_rpc"]] <- temp_future}}
if("2050_85_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_85_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_85_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_85_rpc"]] <- temp_future}}
if("2070_26_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_26_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_26_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_26_rpc"]] <- temp_future}}
if("2070_45_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_45_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_45_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_45_rpc"]] <- temp_future}}
if("2070_85_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_85_rpc.*bioclim.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_85_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_85_rpc"]] <- temp_future}}

                                                                           } # end loop if(type_rasters %in% c("bioclim", "Bioclim", "bioclimatic", "Bioclimatic"))

##########################################################################################

if(type_rasters %in% c("ecophysiology", "Ecophysiology", "thermophysiology", "eco")) {

environmental_present_raw <- stack(list.files(path = environmental_stack_dir, pattern="*.present.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))

if(!is.null(subset_predictors)) {present_layers <- subset(environmental_present_raw, subset_predictors)
                                    } else {present_layers <- environmental_present_raw}

                                       future_layers_list <- list ()
if("2050_26_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_26_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_26_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_26_rpc"]] <- temp_future}}
if("2050_45_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_45_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_45_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_45_rpc"]] <- temp_future}}
if("2050_85_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2050_85_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2050_85_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2050_85_rpc"]] <- temp_future}}
if("2070_26_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_26_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_26_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_26_rpc"]] <- temp_future}}
if("2070_45_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_45_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_45_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_45_rpc"]] <- temp_future}}
if("2070_85_rpc" %in% future_models)  {temp_future <- stack (list.files(path = environmental_stack_dir, pattern="*.2070_85_rpc.*.ecophysiology.*.gri$",full.names=T, ignore.case=T))
                                                  if(!is.null(subset_predictors)) {future_layers_list[["2070_85_rpc"]] <- subset(temp_future, subset_predictors)
                                                                          } else { future_layers_list[["2070_85_rpc"]] <- temp_future}}

                                                                           } # end loop if(type_rasters %in% c("bioclim", "Bioclim", "bioclimatic", "Bioclimatic"))

##########################################################################################

###################            create dir             #########################

output_dir <- path_output_dir

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                                    }

###################            read files or are data_frames             ################

if(class(sp_coord_file) == "data.frame") {
                             sp_coord <- sp_coord_file
                                   } else {
if(grepl(pattern = "*.csv$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep=",", stringsAsFactors = FALSE) }
if(grepl(pattern = "*.txt$", sp_coord_file) == TRUE) {sp_coord <- read.table(file = sp_coord_file, header=TRUE, sep="\t", stringsAsFactors = FALSE) }
                                          }

species_name <- unique(sp_coord$species)

species_coordinates <- sp_coord

##########################################################################################
###################               Begin of biomod modeling               #################
##########################################################################################

# setting explanatory variables

myExpl <- present_layers

######   Preparing response variable if presences only or presences and absences   #######

#### create this key function # NOTICE: %in% and its oposite %!in%

'%!in%' <- function(x,y)!('%in%'(x,y))

###

if (class(species_coordinates) == "data.frame") {
                                           myRespXY <- data.frame(Lon = species_coordinates$Lon, Lat = species_coordinates$Lat)
                                             myResp <- rep(1,length(species_coordinates$Lon))  # number of input coordinates
                                  species_presences <- myRespXY
                                          } else if (class(species_coordinates) == "data.frame" && "pb" %!in% names(species_coordinates)) {
                                          myRespXY <- species_coordinates
                                           myResp <- rep(1,length(species_coordinates$Lon))
                                species_presences <- myRespXY
                                          } else {
                                          myRespXY <- subset(species_coordinates, select = c(Lon,Lat))
                                           myResp <- as.vector(as.matrix(subset(species_coordinates, select = pb)))
                                species_presences <- subset (species_coordinates, pb == 1, select = c(Lon,Lat))
                                 species_absences <- subset (species_coordinates, pb == 0, select = c(Lon,Lat))
                                                 }

# set up the proper test in this case GLM with interactions at level 1 or what I think is things like r_hr x prec09to02, etc. and quadratic terms

#######################        Biomod Model Options                 ######################

myRespName <- species_name

#######################        Construction of model options        ######################

if("GLM" %in% model) {
                      GLM_list = list( type = 'quadratic',
                          interaction.level = 1,
                                  myFormula = NULL,
                                       test = 'BIC',
                                     family = 'binomial',
                                    control = glm.control(epsilon = 1e-08, maxit = 1000,trace = FALSE))
                     } else { GLM_list = NULL}

if("GAM" %in% model) {
                      GAM_mgcv_list = list( algo = 'BAM_mgcv',
                                                     type = 's_smoother',
                                                            k = gam_k_mgcv,
                                            interaction.level = 1,
                                                    myFormula = NULL,
                                                       family = binomial(link = 'logit'),
                                                       method = 'GCV.Cp',
                                                    optimizer = c('outer','newton'),
                                                       select = FALSE,
                                                        knots = NULL,
                                                      paraPen = NULL,
                                                      control = NULL)
                     } else { GAM_mgcv_list = NULL}

if("GAM" %in% model) {
                      GAM_gam_list = list( algo = 'GAM_gam',
                                                     type = 's_smoother',
                                                            k = gam_k_gam,
                                            interaction.level = 1,
                                                    myFormula = NULL,
                                                       family = binomial(link = 'logit'),
                                                       method = 'GCV.Cp',
                                                    optimizer = c('outer','newton'),
                                                       select = FALSE,
                                                        knots = NULL,
                                                      paraPen = NULL,
                                                      control = NULL)
                     } else { GAM_gam_list = NULL}


if("ANN" %in% model) {
                      ANN_list = list( NbCV = 5,
                                       size = NULL,
                                      decay = NULL,
                                       rang = 0.1,
                                      maxit = 700)
                     } else { ANN_list = NULL}

if("MAXENT" %in% model) {
                      MAXENT.Tsuruoka_list = list( l1_regularizer = 1,
                                                   l2_regularizer = 0,
                                                          use_sgd = FALSE,
                                                      set_heldout = 0,
                                                          verbose = FALSE)
                     } else { MAXENT.Tsuruoka_list = NULL}

#######################        Construction of model options        ######################

myBiomodOptions_gam <- BIOMOD_ModelingOptions(GLM = GLM_list,
                                              GAM = GAM_gam_list,
                                              ANN = ANN_list,
                                              MAXENT.Tsuruoka = MAXENT.Tsuruoka_list)

myBiomodOptions_mgcv <- BIOMOD_ModelingOptions(GLM = GLM_list,
                                              GAM = GAM_mgcv_list,
                                              ANN = ANN_list,
                                              MAXENT.Tsuruoka = MAXENT.Tsuruoka_list)

#######################        Biomod Data Formating                ######################

# Required
# myRespXY
# myResp
# !!!!!!!!!!!!!! requires that myResp is a numeric vector NO AN INTERGER

	myBiomodData <-BIOMOD_FormatingData(resp.var=as.numeric(myResp),
	                                    expl.var=myExpl,
	                                    resp.xy = myRespXY,
	                                    resp.name = myRespName,
	                                    PA.nb.rep = PA_nb_rep, 
	                                    PA.nb.absences = n_p_absences, # 10*total presences pseudoabsences
	                                    PA.strategy = 'disk',
	                                    PA.dist.min = PA_dist_min_km*1000, # keep to 50 km min
	                                    PA.dist.max = PA_dist_max_km*1000, # reduce to 200 km max
	                                    PA.sre.quant = 0.025,
	                                    PA.table = NULL,
	                                    na.rm = TRUE)

print(myBiomodData)

#######################        BIOMOD Modeling                ######################

model_input <- model

if("MAXENT" %in% model) { model_input[model_input=="MAXENT"] <- "MAXENT.Tsuruoka"}

## !!! NOTICE two gam algorithms this overcomes the limitation seem on mgcv

            myBiomodModelOut_all_gam <- try(BIOMOD_Modeling(myBiomodData,
	                                    models = model_input,
	                                    models.options = myBiomodOptions_gam,
	                                    NbRunEval=NbRunEval_runs,
	                                    DataSplit=DataSplit_percent,
	                                    models.eval.meth = c('ROC','TSS'), 
	                                    do.full.models=FALSE,
	                                    modeling.id= paste(myRespName, "models_out", sep = "_")))

out_all_gam <-capture.output(myBiomodModelOut_all_gam)

if ('Failed Models :  none' %!in% out_all_gam)     { 

	myBiomodModelOut_all_mgcv <- try(BIOMOD_Modeling(myBiomodData,
	                                    models = model_input,
	                                    models.options = myBiomodOptions_mgcv,
	                                    NbRunEval=NbRunEval_runs,
	                                    DataSplit=DataSplit_percent,
	                                    models.eval.meth = c('ROC','TSS'), 
	                                    do.full.models=FALSE,
	                                    modeling.id= paste(myRespName, "models_out", sep = "_")))

            myBiomodModelOut_all <- myBiomodModelOut_all_mgcv

                                    } else { 

            myBiomodModelOut_all <- myBiomodModelOut_all_gam

                                    }

print(myBiomodModelOut_all)

######################     select models that match algorithm        ######################

GLM_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_GLM", myBiomodModelOut_all@models.computed)]
GAM_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_GAM", myBiomodModelOut_all@models.computed)]
ANN_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_ANN", myBiomodModelOut_all@models.computed)]
MAXENT_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_MAXENT", myBiomodModelOut_all@models.computed)]

#######################        Biomod Ensemble Model                 ######################

myBiomodEM_GLM <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = GLM_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                  committee.averaging = FALSE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))

myBiomodEM_GAM <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = GAM_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                  committee.averaging = FALSE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))

myBiomodEM_ANN <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = ANN_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                  committee.averaging = FALSE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))

myBiomodEM_MAXENT <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = MAXENT_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                  committee.averaging = FALSE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))

myBiomodEM_all <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = 'all',
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = FALSE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                  committee.averaging = FALSE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))


#######################                 Select models                  ######################

myGLMs <- try(BIOMOD_LoadModels(myBiomodModelOut_all, models=c('GLM')), silent = T)
myGAMs <- try(BIOMOD_LoadModels(myBiomodModelOut_all, models=c('GAM')), silent = T)
myANNs <- try(BIOMOD_LoadModels(myBiomodModelOut_all, models=c('ANN')), silent = T)
myMAXENTs <- try(BIOMOD_LoadModels(myBiomodModelOut_all, models=c('MAXENT.Tsuruoka')), silent = T)

#######################                 Load Models                 ######################

if(length(GLM_models) > 0 ) {
myBiomodProj_GLM <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = GLM_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')
                            }

if(length(GAM_models) > 0 ) {
myBiomodProj_GAM <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = GAM_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')
                            }

if(length(ANN_models) > 0 ) {
myBiomodProj_ANN <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = ANN_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')
                            }

if(length(MAXENT_models) > 0 ) {
myBiomodProj_MAXENT <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = MAXENT_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')
                              }


myBiomodProj_all <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = 'all',
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')

#####################         BIOMOD_EnsembleForecasting         #########################

myBiomod_Ensem_Fore_GLM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GLM,
                                        projection.output = myBiomodProj_GLM,
                                        proj.name = 'current_Ensemble_GLM_1975',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_Fore_GAM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GAM,
                                        projection.output = myBiomodProj_GAM,
                                        proj.name = 'current_Ensemble_GAM_1975',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_Fore_ANN <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_ANN,
                                        projection.output = myBiomodProj_ANN,
                                        proj.name = 'current_Ensemble_ANN_1975',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_Fore_MAXENT <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_MAXENT,
                                        projection.output = myBiomodProj_MAXENT,
                                        proj.name = 'current_Ensemble_MAXENT_1975',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_Fore_all <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_all,
                                        projection.output = myBiomodProj_all,
                                        proj.name = 'current_Ensemble_all_1975',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'))

##########################################################################################

pred_Ensem_Fore_GLM_meanByROC <- try(get_predictions(myBiomod_Ensem_Fore_GLM)[[1]], silent = T)
if(!class(pred_Ensem_Fore_GLM_meanByROC) == "try-error") { names(pred_Ensem_Fore_GLM_meanByROC) <- "Ensemble_present_GLM"} else { pred_Ensem_Fore_GLM_meanByROC <- NA}

pred_Ensem_Fore_GAM_meanByROC <- try(get_predictions(myBiomod_Ensem_Fore_GAM)[[1]], silent = T)
if(!class(pred_Ensem_Fore_GAM_meanByROC) == "try-error") { names(pred_Ensem_Fore_GAM_meanByROC) <- "Ensemble_present_GAM"} else { pred_Ensem_Fore_GAM_meanByROC <- NA}

pred_Ensem_Fore_ANN_meanByROC <- try(get_predictions(myBiomod_Ensem_Fore_ANN)[[1]], silent = T)
if(!class(pred_Ensem_Fore_ANN_meanByROC) == "try-error") { names(pred_Ensem_Fore_ANN_meanByROC) <- "Ensemble_present_ANN"} else { pred_Ensem_Fore_ANN_meanByROC <- NA}

pred_Ensem_Fore_MAXENT_meanByROC <- try(get_predictions(myBiomod_Ensem_Fore_MAXENT)[[1]], silent = T)
if(!class(pred_Ensem_Fore_MAXENT_meanByROC) == "try-error") { names(pred_Ensem_Fore_MAXENT_meanByROC) <- "Ensemble_present_MAXENT"} else { pred_Ensem_Fore_MAXENT_meanByROC <- NA}

pred_Ensem_Fore_all_meanByROC <- try(get_predictions(myBiomod_Ensem_Fore_all)[[1]], silent = T)
names(pred_Ensem_Fore_all_meanByROC) <- "Ensemble_present_All"

###################             stack the emsembles for present        ###################

all_emsembled_list_raw <- list(pred_Ensem_Fore_all_meanByROC, pred_Ensem_Fore_GLM_meanByROC, pred_Ensem_Fore_GAM_meanByROC, pred_Ensem_Fore_ANN_meanByROC, pred_Ensem_Fore_MAXENT_meanByROC)
all_emsembled_list <- all_emsembled_list_raw[!is.na(all_emsembled_list_raw)]

all_emsembled_stack <- stack(all_emsembled_list)

# resolution name

if(round(res(all_emsembled_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(all_emsembled_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(all_emsembled_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(all_emsembled_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

writeRaster(all_emsembled_stack , filename=paste0(species_name, "_", type_rasters, "_Emsemble_present_res_", resolution_name, "_stack"), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(all_emsembled_stack))
stack_extent <- extent(all_emsembled_stack)

cat("------------- Raster stack Emsemble file properties ------------\n")
cat("grid names: ", names(all_emsembled_stack), "\n")
cat("grid units:  0 to 1000  \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(all_emsembled_stack), "\n")
cat("minimum values: ", minValue(all_emsembled_stack), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", type_rasters, "_Emsemble_present_res_", resolution_name, "_stack"), "\n")
cat("max_longitude: ", stack_extent[2], "\n","min_longitude: ", stack_extent[1], "\n", "max_latitude: ", stack_extent[4], "\n","min_latitude: ",  stack_extent[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")

##########################################################################################
###################               end of biomod modeling               ###################
##########################################################################################

### write summaries

if("GLM" %in% model) {gml_model_summary_raw <- get_formal_model(get(myGLMs))
                      gml_model_summary <- summary(gml_model_summary_raw)} else { gml_model_summary <- NA}

writeLines(capture.output(print(myBiomodData)), con=paste0(myRespName, "_", type_rasters, "_myBiomodData_summary.txt"))
write(capture.output(print(gml_model_summary)),file=paste0(myRespName, "_", type_rasters, "_myBiomodData_summary.txt"),append=TRUE)
write(capture.output(print(get_evaluations(myBiomodModelOut_all))),file=paste0(myRespName, "_", type_rasters, "_myBiomodData_summary.txt"),append=TRUE)

##########################################################################################
###################                     Projections                      #################
##########################################################################################

future_projection_ensemble_list <- list()

for(i in 1:length(future_layers_list)) {

myBiomod_Ensem_future_GLM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GLM,
                                        new.env = future_layers_list[[i]],
                                        proj.name = 'future_model_GLM',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)



if(!class(myBiomod_Ensem_Fore_GAM) == "try-error") {
myBiomod_Ensem_future_GAM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GAM,
                                        new.env = future_layers_list[[i]],
                                        proj.name = 'future_model_GAM',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)
                                                                                 }


myBiomod_Ensem_future_ANN <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_ANN,
                                        new.env = future_layers_list[[i]],
                                        proj.name = 'future_model_ANN',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_future_MAXENT <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_MAXENT,
                                        new.env = future_layers_list[[i]],
                                        proj.name = 'future_model_MAXENT',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'), silent = T)

myBiomod_Ensem_future_all <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_all,
                                        new.env = future_layers_list[[i]],
                                        proj.name = 'future_model_ALL',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'))

##########################################################################################

name_future_model <- future_models[i]

##########################################################################################

future_GLM_raster <- try(get_predictions(myBiomod_Ensem_future_GLM)[[1]], silent = T)
if(!class(future_GLM_raster) == "try-error") { names(future_GLM_raster) <- paste0("Ensemble_", name_future_model, "_GLM")} else { future_GLM_raster <- NA}

future_GAM_raster <- try(get_predictions(myBiomod_Ensem_future_GAM)[[1]], silent = T)
if(!class(future_GAM_raster) == "try-error") { names(future_GAM_raster) <- paste0("Ensemble_", name_future_model, "_GAM")} else { future_GAM_raster <- NA}

future_ANN_raster <- try(get_predictions(myBiomod_Ensem_future_ANN)[[1]], silent = T)
if(!class(future_ANN_raster) == "try-error") { names(future_ANN_raster) <- paste0("Ensemble_", name_future_model, "_ANN")} else { future_ANN_raster <- NA}

future_MAXENT_raster <- try(get_predictions(myBiomod_Ensem_future_MAXENT)[[1]], silent = T)
if(!class(future_MAXENT_raster) == "try-error") { names(future_MAXENT_raster) <- paste0("Ensemble_", name_future_model, "_MAXENT")} else { future_MAXENT_raster <- NA}

future_ALL_raster <- try(get_predictions(myBiomod_Ensem_future_all)[[1]], silent = T)
if(!class(future_ALL_raster) == "try-error") { names(future_ALL_raster) <- paste0("Ensemble_", name_future_model, "_All")} else { future_ALL_raster <- NA}

###################             stack the emsembles for present        ###################

all_emsembled_future_list_raw <- list(future_ALL_raster, future_GLM_raster, future_GAM_raster, future_ANN_raster, future_MAXENT_raster)
all_emsembled_future_list <- all_emsembled_future_list_raw[!is.na(all_emsembled_future_list_raw)]

all_emsembled_future_stack <- stack(all_emsembled_future_list)

future_projection_ensemble_list[[i]] <- all_emsembled_future_stack

# resolution name

if(round(res(all_emsembled_future_stack)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(all_emsembled_future_stack)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(all_emsembled_future_stack)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(all_emsembled_future_stack)[1], 4) == 0.1667) {resolution_name = "10_min"}

writeRaster(all_emsembled_future_stack , filename=paste0(species_name, "_", type_rasters, "_Emsemble_", name_future_model, "_res_", resolution_name, "_stack"), datatype='INT2S', overwrite=TRUE)

# output grid

projection_raster_stack <- capture.output(crs(all_emsembled_future_stack))
stack_extent <- extent(all_emsembled_future_stack)

cat("------------- Raster stack Emsemble file properties ------------\n")
cat("grid names: ", names(all_emsembled_future_stack), "\n")
cat("grid units:  0 to 1000  \n")
cat("resolution: ", resolution_name, "\n")
cat("maximum values: ", maxValue(all_emsembled_future_stack), "\n")
cat("minimum values: ", minValue(all_emsembled_future_stack), "\n")
cat("file_name_cropped_written: ", paste0(species_name, "_", type_rasters, "_Emsemble_", name_future_model, "_res_", resolution_name, "_stack"), "\n")
cat("max_longitude: ", stack_extent[2], "\n","min_longitude: ", stack_extent[1], "\n", "max_latitude: ", stack_extent[4], "\n","min_latitude: ",  stack_extent[3], "\n")
cat(projection_raster_stack, "\n")
cat("---------------------------------------------------------------\n")

                                } # en loop for(i in 1:length(future_layers_list))
rm(i)

###################             prepare list to return        ###################

future_projection_ensemble_list[[length(future_projection_ensemble_list)+1]] <- all_emsembled_stack
return_list <- future_projection_ensemble_list

setwd(master_directory)

#####

return(return_list)

                          } # End of function

##############                          END of function                       ############

#############################################################################################################################
#Mapinguari_sdm_lost_gain ###################################################################################################
#############################################################################################################################

Mapinguari_sdm_lost_gain <- function (present_raster_value = NULL,
                                       future_raster_value = NULL,
                                          raster_dir_value = NULL,
                                      type_of_raster_value = NULL,
                              time_models_to_compare_value = NULL,
                                rasters_to_compare_value = "all",
                                      species_name_value = NULL,
                                        model_name_value = NULL,
                        BinaryTransformation_logical_value = TRUE,
                                    binary_threshold_value = 0.75,
                                     path_output_dir_value = NULL) {


# require
require("dismo")
require("raster")
require("biomod2")
# install_version(package ="slam", version = "0.1-32", repos = "http://cran.us.r-project.org")

# input from user

present_raster_file <- present_raster_value
future_raster_file <- future_raster_value

raster_dir <- raster_dir_value
type_of_raster <- type_of_raster_value
time_models_to_compare <- time_models_to_compare_value

species_name <- species_name_value
if(is.null(species_name)) {species_name <- NA}

model_name <- model_name_value
if(is.null(model_name)) {model_name <- NA}

var_to_compare <- rasters_to_compare_value

BinaryTransformation_logical <- BinaryTransformation_logical_value
binary_threshold <- binary_threshold_value

output_dir <- path_output_dir_value

master_directory <- getwd()

###################            read rasters             ################

if(class(present_raster_file) == "character") {
                                present_raster_stack_master <- stack(present_raster_file)
                                 }

if(class(future_raster_file) == "character") {
                                future_raster_stack_master <- stack(future_raster_file)
                                 }

if(class(present_raster_file) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                present_raster_stack_master <- present_raster_file
                                 }

if(class(future_raster_file) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                future_raster_stack_master <- present_raster_file
                                 }

if(!is.null(raster_dir_value)) {
                           setwd(raster_dir)
                           list_files_type_of_raster <- list.files(path = ".", pattern=paste0("*.", type_of_raster, ".*"),full.names=T, ignore.case=T)

                           list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.gri$"), list_files_type_of_raster)]
                           if(length(list_files_models_raw_1) == 0) {list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.tif$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_1) == 0) {list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.bil$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_1) == 0) {stop("It appears that the time_models_to_compare are not present in the folder or the raster files are not .gri, .tif or .bil")}
                           present_raster_stack_master <- stack(list_files_models_raw_1)

                           list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.gri$"), list_files_type_of_raster)]
                           if(length(list_files_models_raw_2) == 0) {list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.tif$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_2) == 0) {list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.bil$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_2) == 0) {stop("It appears that the time_models_to_compare are not present in the folder or the raster files are not .gri, .tif or .bil")}
                           future_raster_stack_master <- stack(list_files_models_raw_2)

                           model_name <- type_of_raster
                           species_name_raw <- sub("./", "", list_files_models_raw_1)
                           species_name_raw <- unlist(strsplit(species_name_raw, "_"))
                           species_name <- paste0(species_name_raw[1], "_", species_name_raw[2])

                           setwd(master_directory)
                              }

###################            subset rasters             ################

if(var_to_compare == "all") {
                         present_stack <- present_raster_stack_master
                         future_stack <- future_raster_stack_master
                         } else {
                         names_present_stack <- names(present_raster_stack_master)
                         present_names_subset <- names_present_stack[grep(var_to_compare, names_present_stack)]
                         names_future_stack <- names(future_raster_stack_master)
                         future_names_subset <- names_future_stack[grep(var_to_compare, names_future_stack)]
                         present_stack <- subset(present_raster_stack_master, present_names_subset)
                          future_stack <- subset(future_raster_stack_master, future_names_subset)
                                }

# resolution name

if(round(res(present_stack)[1], 4) == 0.0083) {resolution_name_present = "30_sec"}
if(round(res(present_stack)[1], 4) == 0.0417) {resolution_name_present = "2_5_min"}
if(round(res(present_stack)[1], 4) == 0.0833) {resolution_name_present = "5_min"}
if(round(res(present_stack)[1], 4) == 0.1667) {resolution_name_present = "10_min"}

if(round(res(future_stack)[1], 4) == 0.0083) {resolution_name_future = "30_sec"}
if(round(res(future_stack)[1], 4) == 0.0417) {resolution_name_future = "2_5_min"}
if(round(res(future_stack)[1], 4) == 0.0833) {resolution_name_future = "5_min"}
if(round(res(future_stack)[1], 4) == 0.1667) {resolution_name_future = "10_min"}


cat("\n------------------------      rasters that will be compared      ------------------------\n")
cat("\n species name:   ", species_name, "    \n")
cat("\n type predictor variables:   ", model_name, "    \n")
cat("\n group 1:   ", names(present_stack), "    \n")
cat("\n group 1 resolution:   ", resolution_name_present, "    \n")
cat("\n group 2:   ", names(future_stack), "    \n")
cat("\n group 2 resolution:   ", resolution_name_future, "    \n")
cat("\n-----------------------------------------------------------------------------------------\n")

########################               binary and calculations              #########################

layer_1 <- character()
layer_2 <- character()
niche_overlap_Warren_I <- numeric()
niche_overlap_Warren_D <- numeric()

list_of_present_binary_layers <- list()
list_of_future_binary_layers <- list()


total_present_1 <- numeric()
total_present_0 <- numeric()
total_future_1 <- numeric()
total_future_0 <- numeric()


no_change_0 <- numeric()
no_change_2 <- numeric()
total_no_gain_no_lost <- numeric()
gain_1 <- numeric()
lose_0 <- numeric()

niche_overlap_Warren_I_bin <- numeric()
niche_overlap_Warren_D_bin <- numeric()

       for(i in 1:nlayers(present_stack)) {

                         one_present_layer <- present_stack[[i]]
                         one_future_layer <- future_stack[[i]]

#                         one_present_layer <- present_stack[[1]]
#                         one_future_layer <- future_stack[[1]]

                          layer_1[i] <- names(one_present_layer)
                          layer_2[i] <- names(one_future_layer)

# Niche overlap

          niche_overlap_Warren_I[i] <- nicheOverlap(one_present_layer, one_future_layer, stat='I')
          niche_overlap_Warren_D[i] <- nicheOverlap(one_present_layer, one_future_layer, stat='D')

if( BinaryTransformation_logical == TRUE) {
                     one_present_layer_bin <- BinaryTransformation(one_present_layer/maxValue(one_present_layer), binary_threshold)
                     names(one_present_layer_bin) <- paste0(layer_1[i], "_bin")
                     list_of_present_binary_layers[[i]] <- one_present_layer_bin
                     one_future_layer_bin <- BinaryTransformation(one_future_layer/maxValue(one_future_layer), binary_threshold)
                     names(one_future_layer_bin) <- paste0(layer_2[i], "_bin")
                     list_of_future_binary_layers[[i]] <- one_future_layer_bin

                     total_present_values <- getValues(one_present_layer_bin)
                     total_present_table <- as.data.frame(table(total_present_values))
                     total_present_1[i] <- total_present_table$Freq[total_present_table$total_present_values == 1]
                     total_present_0[i] <- total_present_table$Freq[total_present_table$total_present_values == 0]

                     total_future_values <- getValues(one_future_layer_bin)
                     total_future_table <- as.data.frame(table(total_future_values))
                     total_future_1[i] <- total_future_table$Freq[total_future_table$total_future_values == 1]
                     total_future_0[i] <- total_future_table$Freq[total_future_table$total_future_values == 0]

                     gain_lost_raster <- one_present_layer_bin + one_future_layer_bin
                     gain_lost_raster_values <- getValues(gain_lost_raster)
                     gain_lost_raster_table <- as.data.frame(table(gain_lost_raster_values))

                     no_change_0[i] <- ifelse(length(gain_lost_raster_table$Freq[gain_lost_raster_table$gain_lost_raster_values == 0]) == 0, 0, gain_lost_raster_table$Freq[gain_lost_raster_table$gain_lost_raster_values == 0])
                     no_change_2[i] <- ifelse(length(gain_lost_raster_table$Freq[gain_lost_raster_table$gain_lost_raster_values == 2]) == 0, 0, gain_lost_raster_table$Freq[gain_lost_raster_table$gain_lost_raster_values == 2])

                     total_no_gain_no_lost[i] <- no_change_0[i] + no_change_2[i]

                     gain_lost_raster_na <- gain_lost_raster
                     gain_lost_raster_na[gain_lost_raster == 0] <- NA
                     gain_lost_raster_na[gain_lost_raster == 2] <- NA

                     gain_lost_raster_na_x_future <- gain_lost_raster_na * one_future_layer_bin
                     gain_lost_raster_na_x_future_values <- getValues(gain_lost_raster_na_x_future)
                     gain_lost_raster_na_x_future_table <- as.data.frame(table(gain_lost_raster_na_x_future_values))

                     gain_1[i] <- ifelse(length(gain_lost_raster_na_x_future_table$Freq[gain_lost_raster_na_x_future_table$gain_lost_raster_na_x_future_values == 1]) == 0, 0, gain_lost_raster_na_x_future_table$Freq[gain_lost_raster_na_x_future_table$gain_lost_raster_na_x_future_values == 1])
                     lose_0[i] <- ifelse(length(gain_lost_raster_na_x_future_table$Freq[gain_lost_raster_na_x_future_table$gain_lost_raster_na_x_future_values == 0]) == 0, 0, gain_lost_raster_na_x_future_table$Freq[gain_lost_raster_na_x_future_table$gain_lost_raster_na_x_future_values == 0])

                     niche_overlap_Warren_I_bin[i] <- nicheOverlap(one_present_layer_bin, one_future_layer_bin, stat='I')
                     niche_overlap_Warren_D_bin[i] <- nicheOverlap(one_present_layer_bin, one_future_layer_bin, stat='D')
                                           }

                                              }

########### construct data frames to resuts

# resolution name

if(round(res(present_stack)[1], 4) == 0.0083) {resolution_in_km2 = 0.86 }
if(round(res(present_stack)[1], 4) == 0.0417) {resolution_in_km2 = 21.6225}
if(round(res(present_stack)[1], 4) == 0.0833) {resolution_in_km2 = 86.49}
if(round(res(present_stack)[1], 4) == 0.1667) {resolution_in_km2 = 345.96}

if( BinaryTransformation_logical == TRUE) {
        counting_layer_df <- data.frame(species_name = species_name, 
                                        model = model_name,
                                        layer_1 = layer_1, 
                                        layer_2 = layer_2, 
                                        niche_overlap_Warren_I= niche_overlap_Warren_I, 
                                        niche_overlap_Warren_D = niche_overlap_Warren_D,
       total_area_predicted_for_species_now_km2 = total_present_1*resolution_in_km2,
       total_area_predicted_for_species_future_km2 = total_future_1*resolution_in_km2,
   total_area_non_predicted_for_species_now_km2 = total_present_0*resolution_in_km2,
   total_area_non_predicted_for_species_future_km2 = total_future_0*resolution_in_km2,
                                        no_net_gain_in_km2 = no_change_0*resolution_in_km2, 
                                        no_net_lose_in_km2 = no_change_2*resolution_in_km2,
                                        net_gain_in_km2 = gain_1*resolution_in_km2, 
                                        net_lose_in_km2 = lose_0*resolution_in_km2,
                                        niche_overlap_Warren_I_bin= niche_overlap_Warren_I_bin, 
                                        niche_overlap_Warren_D_bin = niche_overlap_Warren_D_bin,
                                        stringsAsFactors = FALSE)
        counting_layer_df$binThreshold <- binary_threshold
        counting_layer_df$one_cell_is_km2 <- resolution_in_km2
                                        present_rasters_bin <- stack(list_of_present_binary_layers)
                                        future_raster_bin <- stack(list_of_future_binary_layers)
                                          }

if( BinaryTransformation_logical == FALSE) {
        counting_layer_df <- data.frame(species_name = species_name, model = model_name,
                                        layer_1 = layer_1, layer_2 = layer_2, niche_overlap_Warren_I= niche_overlap_Warren_I, niche_overlap_Warren_D = niche_overlap_Warren_D,
                                        stringsAsFactors = FALSE)
                                          }

cat("\n------------------------      rasters comparison results      ------------------------\n")
print(counting_layer_df)
cat("\n--------------------------------------------------------------------------------------\n")

###################            create dir             #########################

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              write.table(counting_layer_df, file= paste0(species_name, "_", model_name, "_comparison_between_rasters_summary.txt"), sep="\t", row.names = FALSE)

if( BinaryTransformation_logical == TRUE) {

if(round(res(present_rasters_bin)[1], 4) == 0.0083) {resolution_name = "30_sec"}
if(round(res(present_rasters_bin)[1], 4) == 0.0417) {resolution_name = "2_5_min"}
if(round(res(present_rasters_bin)[1], 4) == 0.0833) {resolution_name = "5_min"}
if(round(res(present_rasters_bin)[1], 4) == 0.1667) {resolution_name = "10_min"}

                              writeRaster(present_rasters_bin , filename=paste0(species_name, "_", model_name, "_res_", resolution_name, "_binThreshold_", binary_threshold,"_present"), datatype='INT2S', overwrite=TRUE)
                              writeRaster(future_raster_bin , filename=paste0(species_name, "_", model_name, "_res_", resolution_name, "_binThreshold_", binary_threshold,"_future"), datatype='INT2S', overwrite=TRUE)
                                          }
                setwd(master_directory)
                                    }
##########################################################################################

if( BinaryTransformation_logical == TRUE) {
                                list_rasters_bin <- list(data_frame_comparison = counting_layer_df, present=present_rasters_bin, future = future_raster_bin)
                                return(list_rasters_bin)
                                }

return(counting_layer_df)

                                     } # End of function
                                     

##############                          END of function                       ############

#############################################################################################################################
#Mapinguari_sdm_lost_gain_national_parks ####################################################################################
#############################################################################################################################

Mapinguari_sdm_lost_gain_national_parks <- function (national_parks_raster_value = NULL,
                                                            present_raster_value = NULL,
                                                             future_raster_value = NULL,
                                                                raster_dir_value = NULL,
                                                            type_of_raster_value = NULL,
                                                    time_models_to_compare_value = NULL,
                                                        rasters_to_compare_value = "all",
                                                              species_name_value = NULL,
                                                                model_name_value = NULL,
                                                          binary_threshold_value = 0.75,
                                                           path_output_dir_value = NULL) {


# require
require("dismo")
require("raster")
require("biomod2")
# install_version(package ="slam", version = "0.1-32", repos = "http://cran.us.r-project.org")

# input from user

np_master_raster_file <- national_parks_raster_value
present_raster_file <- present_raster_value
future_raster_file <- future_raster_value

raster_dir <- raster_dir_value
type_of_raster <- type_of_raster_value
time_models_to_compare <- time_models_to_compare_value

species_name <- species_name_value
if(is.null(species_name)) {species_name <- NA}

model_name <- model_name_value
if(is.null(model_name)) {model_name <- NA}

var_to_compare <- rasters_to_compare_value

binary_threshold <- binary_threshold_value

output_dir <- path_output_dir_value

master_directory <- getwd()

###################            read rasters             ################

if(class(present_raster_file) == "character") {
                                present_raster_stack_master <- stack(present_raster_file)
                                 }

if(class(future_raster_file) == "character") {
                                future_raster_stack_master <- stack(future_raster_file)
                                 }

if(class(present_raster_file) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                present_raster_stack_master <- present_raster_file
                                 }

if(class(future_raster_file) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                future_raster_stack_master <- present_raster_file
                                 }

if(!is.null(raster_dir_value)) {
                           setwd(raster_dir)
                           list_files_type_of_raster <- list.files(path = ".", pattern=paste0("*.", type_of_raster, ".*"),full.names=T, ignore.case=T)

                           list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.gri$"), list_files_type_of_raster)]
                           if(length(list_files_models_raw_1) == 0) {list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.tif$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_1) == 0) {list_files_models_raw_1 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[1],".*.bil$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_1) == 0) {stop("It appears that the time_models_to_compare are not present in the folder or the raster files are not .gri, .tif or .bil")}
                           present_raster_stack_master <- stack(list_files_models_raw_1)

                           list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.gri$"), list_files_type_of_raster)]
                           if(length(list_files_models_raw_2) == 0) {list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.tif$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_2) == 0) {list_files_models_raw_2 <- list_files_type_of_raster[grep(paste0(time_models_to_compare[2],".*.bil$"), list_files_type_of_raster)]}
                           if(length(list_files_models_raw_2) == 0) {stop("It appears that the time_models_to_compare are not present in the folder or the raster files are not .gri, .tif or .bil")}
                           future_raster_stack_master <- stack(list_files_models_raw_2)

                           model_name <- type_of_raster
                           species_name_raw <- sub("./", "", list_files_models_raw_1)
                           species_name_raw <- unlist(strsplit(species_name_raw, "_"))
                           species_name <- paste0(species_name_raw[1], "_", species_name_raw[2])

                           setwd(master_directory)
                              }

# national parks

if(class(np_master_raster_file) == "character") {
                                np_stack_master <- stack(np_master_raster_file)
                                names(np_stack_master) <- "national_park_bin"
                                 }

if(class(np_master_raster_file) %in% c("RasterStack", "RasterBrick", "RasterLayer")) {
                                np_stack_master <- np_master_raster_file
                                names(np_stack_master) <- "national_park_bin"
                                 }

if(is.null(np_master_raster_file)) { stop("you must provide a national parks raster as a binary: 0 - no national park, 1 - national park") }


###################            subset rasters             ################

if(var_to_compare == "all") {
                         present_stack <- present_raster_stack_master
                         future_stack <- future_raster_stack_master
                         } else {
                         names_present_stack <- names(present_raster_stack_master)
                         present_names_subset <- names_present_stack[grep(var_to_compare, names_present_stack)]
                         names_future_stack <- names(future_raster_stack_master)
                         future_names_subset <- names_future_stack[grep(var_to_compare, names_future_stack)]
                         present_stack <- subset(present_raster_stack_master, present_names_subset)
                          future_stack <- subset(future_raster_stack_master, future_names_subset)
                                }

# resolution name

if(round(res(present_stack)[1], 4) == 0.0083) {resolution_name_present = "30_sec"}
if(round(res(present_stack)[1], 4) == 0.0417) {resolution_name_present = "2_5_min"}
if(round(res(present_stack)[1], 4) == 0.0833) {resolution_name_present = "5_min"}
if(round(res(present_stack)[1], 4) == 0.1667) {resolution_name_present = "10_min"}

if(round(res(future_stack)[1], 4) == 0.0083) {resolution_name_future = "30_sec"}
if(round(res(future_stack)[1], 4) == 0.0417) {resolution_name_future = "2_5_min"}
if(round(res(future_stack)[1], 4) == 0.0833) {resolution_name_future = "5_min"}
if(round(res(future_stack)[1], 4) == 0.1667) {resolution_name_future = "10_min"}

if(round(res(np_stack_master)[1], 4) == 0.0083) {resolution_name_np = "30_sec"}
if(round(res(np_stack_master)[1], 4) == 0.0417) {resolution_name_np = "2_5_min"}
if(round(res(np_stack_master)[1], 4) == 0.0833) {resolution_name_np = "5_min"}
if(round(res(np_stack_master)[1], 4) == 0.1667) {resolution_name_np = "10_min"}


cat("\n------------------------      rasters that will be compared      ------------------------\n")
cat("\n species name:   ", species_name, "    \n")
cat("\n type predictor variables:   ", model_name, "    \n")
cat("\n group 1:   ", names(present_stack), "    \n")
cat("\n group 1 resolution:   ", resolution_name_present, "    \n")
cat("\n group 2:   ", names(future_stack), "    \n")
cat("\n group 2 resolution:   ", resolution_name_future, "    \n")
cat("\n national parks:   ", names(np_stack_master), "    \n")
cat("\n national parks resolution:   ", resolution_name_np, "    \n")
cat("\n-----------------------------------------------------------------------------------------\n")

########################               national park              #########################

# determine the extent of presence rasters

present_raster_extent <- extent(present_stack)

# crop np

np_crop <- crop(np_stack_master, present_raster_extent)
np_resampled<-resample(np_crop, present_stack[[1]], method="ngb")

########################               binary and calculations              #########################

layer_1 <- character()
layer_2 <- character()

total_present_1 <- numeric()
total_present_0 <- numeric()
no_change_0_present <- numeric()
no_change_2_present <- numeric()
absent_protected_1_present <- numeric()
predicted_non_protected_minus_1_present <- numeric()
niche_overlap_Warren_I_present <- numeric()
niche_overlap_Warren_D_present <- numeric()

total_future_1 <- numeric()
total_future_0 <- numeric()
no_change_0_future <- numeric()
no_change_2_future <- numeric()
absent_protected_1_future <- numeric()
predicted_non_protected_minus_1_future <- numeric()
niche_overlap_Warren_I_future <- numeric()
niche_overlap_Warren_D_future <- numeric()

       for(i in 1:nlayers(present_stack)) {

                         one_present_layer <- present_stack[[i]]
                         one_future_layer <- future_stack[[i]]

#                         one_present_layer <- present_stack[[i]]
#                         one_future_layer <- future_stack[[i]]

                          layer_1[i] <- names(one_present_layer)
                          layer_2[i] <- names(one_future_layer)

# present and national parks

                     niche_overlap_Warren_I_present[i] <- nicheOverlap(one_present_layer, np_resampled, stat='I')
                     niche_overlap_Warren_D_present[i] <- nicheOverlap(one_present_layer, np_resampled, stat='D')

                     one_present_layer_bin <- BinaryTransformation(one_present_layer/maxValue(one_present_layer), binary_threshold)
                     names(one_present_layer_bin) <- paste0(layer_1[i], "_bin")

                     total_present_values <- getValues(one_present_layer_bin)
                     total_present_table <- as.data.frame(table(total_present_values))
                     total_present_1[i] <- total_present_table$Freq[total_present_table$total_present_values == 1]
                     total_present_0[i] <- total_present_table$Freq[total_present_table$total_present_values == 0]

                     park_plus_present_raster <- one_present_layer_bin + np_resampled
                     park_plus_present_values <- getValues(park_plus_present_raster)
                     park_plus_present_table <- as.data.frame(table(park_plus_present_values))

                     no_change_0_present[i] <- ifelse(length(park_plus_present_table$Freq[park_plus_present_table$park_plus_present_values == 0]) == 0, 0, park_plus_present_table$Freq[park_plus_present_table$park_plus_present_values == 0])
                     no_change_2_present[i] <- ifelse(length(park_plus_present_table$Freq[park_plus_present_table$park_plus_present_values == 2]) == 0, 0, park_plus_present_table$Freq[park_plus_present_table$park_plus_present_values == 2])

                     park_minus_present_raster <- np_resampled - one_present_layer_bin
                     park_minus_present_values <- getValues(park_minus_present_raster)
                     park_minus_present_table <- as.data.frame(table(park_minus_present_values))

                     absent_protected_1_present[i] <- ifelse(length(park_minus_present_table$Freq[park_minus_present_table$park_minus_present_values == 1]) == 0, 0, park_minus_present_table$Freq[park_minus_present_table$park_minus_present_values == 1])
                     predicted_non_protected_minus_1_present[i] <- ifelse(length(park_minus_present_table$Freq[park_minus_present_table$park_minus_present_values == -1]) == 0, 0, park_minus_present_table$Freq[park_minus_present_table$park_minus_present_values == -1])

# future and national parks

                     niche_overlap_Warren_I_future[i] <- nicheOverlap(one_future_layer, np_resampled, stat='I')
                     niche_overlap_Warren_D_future[i] <- nicheOverlap(one_future_layer, np_resampled, stat='D')

                     one_future_layer_bin <- BinaryTransformation(one_future_layer/maxValue(one_future_layer), binary_threshold)
                     names(one_future_layer_bin) <- paste0(layer_1[i], "_bin")

                     total_future_values <- getValues(one_future_layer_bin)
                     total_future_table <- as.data.frame(table(total_future_values))
                     total_future_1[i] <- total_future_table$Freq[total_future_table$total_future_values == 1]
                     total_future_0[i] <- total_future_table$Freq[total_future_table$total_future_values == 0]

                     park_plus_future_raster <- one_future_layer_bin + np_resampled
                     park_plus_future_values <- getValues(park_plus_future_raster)
                     park_plus_future_table <- as.data.frame(table(park_plus_future_values))

                     no_change_0_future[i] <- ifelse(length(park_plus_future_table$Freq[park_plus_future_table$park_plus_future_values == 0]) == 0, 0, park_plus_future_table$Freq[park_plus_future_table$park_plus_future_values == 0])
                     no_change_2_future[i] <- ifelse(length(park_plus_future_table$Freq[park_plus_future_table$park_plus_future_values == 2]) == 0, 0, park_plus_future_table$Freq[park_plus_future_table$park_plus_future_values == 2])

                     park_minus_future_raster <- np_resampled - one_future_layer_bin
                     park_minus_future_values <- getValues(park_minus_future_raster)
                     park_minus_future_table <- as.data.frame(table(park_minus_future_values))

                     absent_protected_1_future[i] <- ifelse(length(park_minus_future_table$Freq[park_minus_future_table$park_minus_future_values == 1]) == 0, 0, park_minus_future_table$Freq[park_minus_future_table$park_minus_future_values == 1])
                     predicted_non_protected_minus_1_future[i] <- ifelse(length(park_minus_future_table$Freq[park_minus_future_table$park_minus_future_values == -1]) == 0, 0, park_minus_future_table$Freq[park_minus_future_table$park_minus_future_values == -1])

                                              }
rm(i)

########### construct data frames to resuts

# resolution name

if(round(res(present_stack)[1], 4) == 0.0083) {resolution_in_km2 = 0.86 }
if(round(res(present_stack)[1], 4) == 0.0417) {resolution_in_km2 = 21.6225}
if(round(res(present_stack)[1], 4) == 0.0833) {resolution_in_km2 = 86.49}
if(round(res(present_stack)[1], 4) == 0.1667) {resolution_in_km2 = 345.96}

counting_np_layer_df <- data.frame(species_name = species_name, 
                                          model = model_name,
                                        layer_1 = layer_1, 
                                        layer_2 = layer_2, 
                niche_overlap_Warren_I_now_park = niche_overlap_Warren_I_present, 
                niche_overlap_Warren_I_future_park = niche_overlap_Warren_I_future, 
                niche_overlap_Warren_D_now_park = niche_overlap_Warren_D_present,
                niche_overlap_Warren_D_future_park = niche_overlap_Warren_D_future,
       total_area_predicted_for_species_now_km2 = total_present_1*resolution_in_km2,
       total_area_predicted_for_species_future_km2 = total_future_1*resolution_in_km2,
   total_area_non_predicted_for_species_now_km2 = total_present_0*resolution_in_km2,
   total_area_non_predicted_for_species_future_km2 = total_future_0*resolution_in_km2,
                   predicted_absent_outside_parks_now_in_km2 = no_change_0_present*resolution_in_km2, 
                predicted_absent_outside_parks_future_in_km2 = no_change_0_future*resolution_in_km2, 
                predicted_present_inside_parks_now_in_km2 = no_change_2_present*resolution_in_km2,
                predicted_present_inside_parks_future_in_km2 = no_change_2_future*resolution_in_km2,
       predicted_absent_inside_parks_now_in_km2 = absent_protected_1_present*resolution_in_km2, 
       predicted_absent_inside_parks_future_in_km2 = absent_protected_1_future*resolution_in_km2, 
     predicted_present_outside_parks_now_in_km2 = predicted_non_protected_minus_1_present*resolution_in_km2, 
     predicted_present_outside_parks_future_in_km2 = predicted_non_protected_minus_1_future*resolution_in_km2, 
                                        stringsAsFactors = FALSE)
        counting_np_layer_df$binThreshold <- binary_threshold
        counting_np_layer_df$one_cell_is_km2 <- resolution_in_km2

cat("\n------------------------      species area and national park results      ------------------------\n")
print(counting_np_layer_df)
cat("\n---------------------------------------------------------------------------------------------------\n")

###################            create dir             #########################

# dir.create

if(is.null(output_dir) == FALSE) {
                              dir.create(output_dir, showWarnings = F)
                              setwd(output_dir)
                              write.table(counting_np_layer_df, file= paste0(species_name, "_", model_name, "_national_parks_vs_sdm_summary.txt"), sep="\t", row.names = FALSE)
                                          }
                setwd(master_directory)
                return(counting_np_layer_df)
                                          } # End of function


##############                          END of function                       ############
