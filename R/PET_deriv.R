
# Calculating monthly PET
# The following code takes the function in EcoHydRology and applies
# it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision)

# necessary variables

lat_rad <- 0
slope_stack <- 0
aspect_stack <- 0
tmax_year_scenario_stack <- 0
tmin_year_scenario_stack <- 0

for (i in 1:12) {
  evap <- raster::raster(tmax_year_scenario_stack, 1) # why? check original equations
  slope <- raster::values(subset(slope_stack, 1))
  aspect <- raster::values(subset(aspect_stack, 2))
  Tmax <- raster::values(subset(tmax_year_scenario_stack, i))/10
  Tmin <- raster::values(subset(tmin_year_scenario_stack, i))/10
  d <- data.frame(day = (30 * i) - 15, Tmin, Tmax, slope, aspect, lat_rad) # day at the midpoint of each month
  d[is.na(d)] <- 0
  Es_PET <- suppressWarnings(EcoHydroLogy::PET_fromTemp(Jday = d$day, Tmax_C = d$Tmax, Tmin_C = d$Tmin, lat_radians = d$lat_rad, aspect = d$aspect, slope = d$slope) * 1000)
  raster::values(evap) <- Es_PET
  if (i == 1) {
    PET <- suppressMessages(raster::brick(evap))
  }
  if (i > 1) {
    PET <- suppressMessages(raster::addLayer(PET, evap))
  }
}

PET_year_scenario_stack <- suppressMessages(PET * 100)
