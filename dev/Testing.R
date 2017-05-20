library('magrittr')

dist <- read.table(file = "./data/FulanusDistribution.txt", header = TRUE)

FulanusEcoRasters_month <-
  EcoRasters2(raster_source = "./global_grids_10_minutes/",
    ext = dist,
    non_fixed_var = c('prec', 'tmin', 'tmax'),
    fixed_var = 'alt',
    years = c("present", '2050', '2070'),
    scenarios = c('rcp26', 'rcp45', 'rcp85'),
    phenology = 'month',
    reorder = T)

FulanusEcoRasters_season <-
  EcoRasters2(raster_source = "./global_grids_10_minutes/",
    ext = dist,
    non_fixed_var = c('prec', 'tmin', 'tmax', 'PET', 'AET', 'CWD'),
    fixed_var = 'alt',
    years = c("present", '2050', '2070'),
    scenarios = c('rcp26', 'rcp45', 'rcp85'),
    phenology = 'season',
    StartSeason = 3,
    StopSeason = 8,
    derive = T)

perf <- read.table(file = "./data/FulanusPerformance.txt", header = TRUE)

perf_functions <-
  PerfFUN(formula = performance ~ s(temp, bs = 'cs') + size,
    data = perf,
    type = 'GAMM',
    random = list(id = ~ 1)
  )

Perf_rasters <-
Perf_rasters(raster_source = FulanusEcoRasters,
  Perf_args = list(temp = 'tmax', size = 10),
  separator = '_',
  PerfFUN = perf_functions$predict$model_1)

PhenFUN <- function(x) 1/(1 + exp((26 - x)/2))
