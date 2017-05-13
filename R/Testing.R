library('magrittr')

dist <- read.table(file = "./data/FulanusDistribution.txt", h = T)

x <-
EcoRasters2(raster_source = "./global_grids_10_minutes/",
            ext = dist,
            non_fixed_var = c('prec', 'tmin', 'tmax', 'PET', 'AET', 'CWD'),
            fixed_var = 'alt',
            years = c("present", '2050', '2070'),
            scenarios = c('rcp26', 'rcp45', 'rcp85'),
            StartSeason = 3,
            StopSeason = 8)

raster_source = "./global_grids_10_minutes/"
non_fixed_var = c('prec', 'tmin', 'tmax')
fixed_var = 'alt'
years = c("present", '2050', '2070')
scenarios = c('rcp26', 'rcp45', 'rcp85')
baseline = c("present", "baseline")
separator = "_"
