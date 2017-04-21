# This function currently does only environmental variables (non species specific).

# It allows rasters to be inputed from workspace, a folder containing all variables or individual paths for each variable. It can also automatically download rasters from worldclim.

EcoRasters <- function(rasterpath = NULL,

                       ext = raster::extent(-180, 180, -60, 90),
                       sp_coord=NULL,
                       margin=0,

                       clim_var=NULL,
                       geo_var=NULL,
                       scenarios=NULL,

                       name='Mapinguari', # this is just a name that will be on output files, you can write anything, like the species name, so you can identify your files
                       write_raster=FALSE,
                       out_dir=NULL, # if this is null, the files will be written on the workspace

                       download_rasters=FALSE, # downloads the rasters not supplied
                       projection_model='MP', # future projection model in case future rasters are being downloaded. Default is MPI-ESM-LR
                       resolution,

                       group_by='month', # The rasters outputed can either be left separated by 'month', averaged by 'year' or by 'season', in which case the month when the season begins and ends must be indicated in the two arguments bellow. There will be an average for the season and another for the remaining months (non-season).
                       StartSeason,
                       StopSeason,

                       reorder=T # monthly rasters on WorldClim stacks will come in a funny order, due to being arranged by alphabetic, not chronological order. This fixes that, if you are using any stack that is already on the desired order, set this to false.

){

  # required libraries: replace with namespace assignment

  # require('rgdal')
  # require('raster')
  # require('geosphere')
  # require('maptools')
  # require('dismo')
  # require('stringi')
  # require('EcoHydRology')

  # save current directory
  # see alternative to changing directories

  master_directory <- getwd()

  # set workspace to output directory. If it doesn't exist, create it

  if (!is.null(out_dir)) {

    if (dir.exists(out_dir) == F) {
      dir.create(out_dir, showWarnings = F,recursive = T)
    }

    setwd(out_dir)

  }

  # Select extent by species distribution

  if (!is.null(sp_coord)) {

    ext <- raster::extent((min(sp_coord$Lon) - margin), (max(sp_coord$Lon) + margin), (min(sp_coord$Lat)-margin), (max(sp_coord$Lat) + margin))

  }

  # include variables needed to calculate other variables

  if(is.element('CWD',clim_var)){
    if(!is.element('PET',clim_var)){
      clim_var<-c(clim_var,'PET')
    }
    if(!is.element('AET',clim_var)){
      clim_var<-c(clim_var,'AET')
    }
  }

  if(is.element('AET',clim_var)){
    if(!is.element('PET',clim_var)){
      clim_var<-c(clim_var,'PET')
    }
    if(!is.element('prec',clim_var)){
      clim_var<-c(clim_var,'prec')
    }
  }

  if(is.element('PET',clim_var)){
    if(!is.element('tmin',clim_var)){
      clim_var<-c(clim_var,'tmin')
    }
    if(!is.element('tmax',clim_var)){
      clim_var<-c(clim_var,'tmax')
    }
    if(!is.element('slopeaspect',geo_var)){
      geo_var<-c(geo_var,'slopeaspect')
    }
    if(!is.element('latitude',geo_var)){
      geo_var<-c(geo_var,'latitude')
    }
  }

  if(is.element('solarradiation',clim_var)){
    if(!is.element('tmin',clim_var)){
      clim_var<-c(clim_var,'tmin')
    }
    if(!is.element('tmax',clim_var)){
      clim_var<-c(clim_var,'tmax')
    }
    if(!is.element('slopeaspect',geo_var)){
      geo_var<-c(geo_var,'slopeaspect')
    }
    if(!is.element('latitude',geo_var)){
      geo_var<-c(geo_var,'latitude')
    }
  }

  if(is.element('slopeaspect',geo_var)){
    if(!is.element('alt',geo_var)){
      geo_var<-c(geo_var,'alt')
    }
  }

  if(is.element('latitude',geo_var)){
    if(!is.element('alt',geo_var)){
      geo_var<-c(geo_var,'alt')
    }
  }

  if(is.element('longitude',geo_var)){
    if(!is.element('alt',geo_var)){
      geo_var<-c(geo_var,'alt')
    }
  }

  if(any(stri_detect_fixed(geo_var,'soil'))){
    if(!is.element('alt',geo_var)){
      geo_var<-c(geo_var,'alt')
    }
  }

  # Create list with combination of climate variables with scenarios and join it with list of geological variables

  if(!is.null(clim_var) & is.null(geo_var)){

    eg<-expand.grid(clim_var, scenarios)
    clim_comb <- sprintf('%s_%s', eg[,1], eg[,2])
    var_list<-clim_comb
    scenario_list<-paste(eg[,2])
    var_only_list<-paste(eg[,1])

  } else if(!is.null(geo_var) & is.null(clim_var)){

    var_list<-geo_var
    scenario_list<-rep('constant',length(geo_var))
    var_only_list<-geo_var

  } else {

    eg<-expand.grid(clim_var, scenarios)
    clim_comb <- sprintf('%s_%s', eg[,1], eg[,2])
    var_list<-c(geo_var,clim_comb)
    scenario_list<-c(rep('constant',length(geo_var)), paste(eg[,2]))
    var_only_list<-c(geo_var, paste(eg[,1]))
  }

  unique_climate_scenarios<-unique(scenarios)

  # Identify if the user has provided a path to a folder or a raster in the workspace for any variable

  for(i in 1:length(var_list)){

    if(exists(var_list[i])){

      eval(parse(text=paste('

if(is.character(',var_list[i],')){

        ',var_list[i],'_path<-',var_list[i],'

} else if(class(',var_list[i],')==\'RasterLayer\' | class(',var_list[i],')==\'RasterStack\') {

        ',var_list[i],'_raster<-',var_list[i],'
}

',sep='')))

    }

  }

# If rasterpath is supplied, set rasterpath for each variable

if(!is.null(rasterpath)){

  # Paste an extra '/', in case the user has not put one at the end of rasterpath

  rasterpath<-paste(rasterpath,'/',sep='')

  for(i in 1:length(var_list)){

    eval(parse(text=paste('

if(is.element(\'',var_list[i],'\',var_list) & !exists(\'',var_list[i],'_path\')){

',var_list[i],'_path <- dir(rasterpath, pattern=\'^',var_list[i],'_*\',full.names=T)

}

',sep='')))

  }

}

# Fetch files from directory

for(i in 1:length(var_list)){

  eval(parse(text=paste('

if(!exists(\'',var_list[i],'_raster\') & exists(\'',var_list[i],'_path\')){

',var_list[i],'_file <- list.files(path = ',var_list[i],'_path, pattern=\'*.bil$\',full.names=T, ignore.case=T)

if(length(',var_list[i],'_file) == 0) {',var_list[i],'_file <- list.files(path = ',var_list[i],'_path, pattern=\'*.tif$\',full.names=T, ignore.case=T)}
if(length(',var_list[i],'_file) == 0) {',var_list[i],'_file <- list.files(path = ',var_list[i],'_path, pattern=\'*.gri$\',full.names=T, ignore.case=T)}

cat(\'\n\')
cat(\'------------- Raster file list ------------\n\')
print(',var_list[i],'_file)
cat(\'\n\')

',var_list[i],'_raster <- suppressMessages(stack(',var_list[i],'_file))

}

',sep='')))

}

# Download missing rasters

if(download_rasters==T){

  for(i in 1:length(var_list)){

    if(any(stri_detect_fixed(var_list[i],'bio')) |
       any(stri_detect_fixed(var_list[i],'min')) |
       any(stri_detect_fixed(var_list[i],'max')) |
       any(stri_detect_fixed(var_list[i],'prec')) |
       any(stri_detect_fixed(var_list[i],'alt')) |
       any(stri_detect_fixed(var_list[i],'tmean'))
    ){

      # define variable

      if(any(stri_detect_fixed(var_list[i],'bio'))){
        var<-'bio'
      } else if(any(stri_detect_fixed(var_list[i],'min'))){
        var<-'tmin'
      } else if(any(stri_detect_fixed(var_list[i],'max'))){
        var<-'tmax'
      } else if(any(stri_detect_fixed(var_list[i],'prec'))){
        var<-'prec'
      } else if(any(stri_detect_fixed(var_list[i],'alt'))){
        var<-'alt'
      } else if(any(stri_detect_fixed(var_list[i],'tmean'))){
        var<-'tmean'
      }

      # define year

      if(any(stri_detect_fixed(var_list[i],'50'))){
        year<-50
      } else if(any(stri_detect_fixed(var_list[i],'70'))){
        year<-70
      }

      # define rcp

      if(any(stri_detect_fixed(var_list[i],'26'))){
        rcp<-26
      } else if(any(stri_detect_fixed(var_list[i],'45'))){
        rcp<-45
      } else if(any(stri_detect_fixed(var_list[i],'60'))){
        rcp<-60
      } else if(any(stri_detect_fixed(var_list[i],'85'))){
        rcp<-85
      }

      # download present rasters

      if(!exists('year')){

        eval(parse(text=paste('

if(!exists(\'',var_list[i],'_path\') & !exists(\'',var_list[i],'_raster\') & download_rasters==T){

',var_list[i],'_raster<-suppressMessages(crop(getData(\'worldclim\', var=var, res=resolution),ext))

} else if(length(',var_list[i],'_path)==0 & download_rasters==F){

 stop(\'Cannot find ',var_list[i],' raster\')
 }

 ',sep='')))

        # download future rasters

      } else {

        eval(parse(text=paste('

if(!exists(\'',var_list[i],'_path\') & !exists(\'',var_list[i],'_raster\') & download_rasters==T){

 ',var_list[i],'_raster<-suppressMessages(crop(getData(\'CMIP5\', var=var, res=resolution, rcp=rcp, year=year, model=projection_model),ext))

} else if(length(',var_list[i],'_path)==0 & download_rasters==F){

 stop(\'Cannot find ',var_list[i],' raster\')

}

',sep='')))

      }

    suppressWarnings(rm(var,year,rcp))

    }

  }

# Download soil rasters

if(any(stri_detect_fixed(var_list,'soil'))){

  soilvars<-var_list[which(stri_detect_fixed(var_list,'soil'))]

  for(i in 1:length(soilvars)){

    layer<-stri_sub(soilvars[i],from=5,to=stri_length(soilvars[i]))

    out_file<-paste(out_dir,layer,'.tif',sep='')

    download.file(paste('http://webservices.isric.org/geoserver/ows?service=WCS&version=2.0.1&request=GetCoverage&CoverageId=geonode:',layer,'&subset=Long(',extent[1],',',extent[2],')&subset=Lat(',extent[3],',',extent[4],')',sep=''),destfile=out_file)

    if(resolution!='250m'){

      soilrasters<-suppressMessages(crop(resample(raster(out_file),alt_raster),ext))

    } else {

      soilrasters<-suppressMessages(crop(raster(out_file),ext))

    }

    file.remove(out_file)

  }

}

}

################################     CROP RASTERS     #################################

for(i in 1:length(var_list)){

  if(exists(paste(var_list[i],'_raster',sep=''))){

    # check if it is necessary to crop raster
    eval(parse(text=paste('

if(',var_list[i],'_raster@extent!=ext) {
',var_list[i],'_crop <- crop (',var_list[i],'_raster, ext)
} else {
',var_list[i],'_crop <-',var_list[i],'_raster
}

',sep='')))

  }

}

################################     PROCESS RASTERS     #################################

for(i in 1:length(unique(scenario_list))){

  eval(parse(text=paste('raster_stack_',unique(scenario_list)[i],'<-stack()

',sep='')))

}

for(i in 1:length(var_list)){

  eval(parse(text=paste('

if(exists(\'',var_list[i],'_crop\')){
if(length(names(',var_list[i],'_crop))==12){

',var_list[i],'_stack_raw <- suppressMessages(stack(',var_list[i],'_crop))

if(reorder==T){

',var_list[i],'_stack <- subset(',var_list[i],'_stack_raw, c(1,5,6,7,8,9,10,11,12,2,3,4))

}

} else {

',var_list[i],'_stack <- stack(',var_list[i],'_crop)
names(',var_list[i],'_stack)<-\'',var_list[i],'\'
raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_stack)

}

} else if(var_list[i]==\'slopeaspect\'){

slopeaspect_crop <- suppressMessages(stack(terrain(alt_crop, opt=c(\'slope\', \'aspect\'), unit=\'radians\', neighbors=8)))

',var_list[i],'_stack <- stack(',var_list[i],'_crop)
names(',var_list[i],'_stack)<-c(\'slope\',\'aspect\' )
raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_stack)

}

',sep='')))

}

if(is.element('latitude', var_list)){
  lat_rad <- coordinates(alt_stack)[, 2] * pi/180
  latitude_raster<-alt_stack
  latitude_stack<-latitude_raster
  values(latitude_raster)<-lat_rad
}

if(is.element('longitude', var_list)){
  lon_rad <- coordinates(alt_stack)[, 1] * pi/180
  longitude_raster<-alt_stack
  longitude_stack<-longitude_raster
  values(longitude_raster)<-lon_rad

}

for(i in 1:length(unique_climate_scenarios)){

  eval(parse(text=paste('

if(is.element(\'PET\',clim_var)){

# Calculating monthly PET
# The following code takes the function in EcoHydRology and applies
# it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision)

for (i in 1:12) {
    evap <- raster(tmax_',unique_climate_scenarios[[i]],'_stack, 1)
    slope <- values(subset(slopeaspect_stack, 1))
    aspect <- values(subset(slopeaspect_stack, 2))
    Tmax <- values(subset(tmax_',unique_climate_scenarios[[i]],'_stack, i))/10
    Tmin <- values(subset(tmin_',unique_climate_scenarios[[i]],'_stack, i))/10
    d <- data.frame(day = (30 * i) - 15, Tmin, Tmax, slope, aspect, lat_rad) # day at the midpoint of each month
    d[is.na(d)] <- 0
    Es_PET <- suppressWarnings(PET_fromTemp(Jday = d$day, Tmax_C = d$Tmax, Tmin_C = d$Tmin, lat_radians = d$lat_rad, aspect = d$aspect, slope = d$slope) * 1000)
    values(evap) <- Es_PET
    if (i == 1) {
        PET <- suppressMessages(brick(evap))
    }
    if (i > 1) {
        PET <- suppressMessages(addLayer(PET, evap))
    }
}

PET_',unique_climate_scenarios[[i]],'_stack <- suppressMessages(PET * 100)

}

if(is.element(\'AET\',clim_var)){

# Estimating AET using a simple bucket model # Duncan Golicher code:
# AET is Actual evapotranspiration and always lower than PET potential evapotranspiration
# and can be much lower when the soil profile is well below field capacity.

Bucket <- suppressMessages(raster(PET_',unique_climate_scenarios[[i]],'_stack/100, 1))

for (n in 1:2) {
    for (i in 1:359) {
        mn <- 1 + i%/%30                                # %/% indicates integer division
        NewAET <- suppressMessages(raster(PET_',unique_climate_scenarios[[i]],'_stack/100, 1))
        NewBucket <- values(Bucket)
        rain <- values(subset(prec_',unique_climate_scenarios[[i]],'_stack, mn))/30
        alpha <- (NewBucket - 200)/300
        evap <- suppressMessages(values(subset(PET_',unique_climate_scenarios[[i]],'_stack/100, mn)) * alpha * 0.8)   #     A fudge factor for stomatal control.
        NewBucket <- NewBucket + (rain) - evap
        NewBucket[NewBucket > 500] <- 500
        NewBucket[NewBucket < 200] <- 200
        values(Bucket) <- NewBucket
        values(NewAET) <- evap * (NewBucket > 200)
        if (n > 1 && (i%%30) - 15 == 0) {     ## i%%30 will run 1 to 359 and determine position in 30 e.g., 1 is 1 and 61 is 1
            if (mn == 1) {
                AET <- suppressMessages(brick(NewAET))
            }
            if (mn > 1) {
                AET <- suppressMessages(addLayer(AET, NewAET))
            }
        }
    }
}

AET_',unique_climate_scenarios[[i]],'_stack <- suppressMessages(AET * 100)

}

if(is.element(\'CWD\',clim_var)){

CWD_',unique_climate_scenarios[[i]],'_stack<-PET_',unique_climate_scenarios[[i]],'_stack-AET_',unique_climate_scenarios[[i]],'_stack

}

if(is.element(\'solarradiation\',clim_var)){

# Calculating monthly Solar Radiation
# The following code takes the function in EcoHydRology and applies
# it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision) .

if(!exists(\'solarradiation_raster\')){

for (k in 1:12) {
    solar_r <- raster(tmax_',unique_climate_scenarios[[i]],'_stack, 1)
    slope <- values(subset(slopeaspect_stack, 1))
    aspect <- values(subset(slopeaspect_stack, 2))
    Tmax <- values(subset(tmax_',unique_climate_scenarios[[i]],'_stack, k))/10
    Tmin <- values(subset(tmin_',unique_climate_scenarios[[i]],'_stack, k))/10
    d <- data.frame(day = (30 * k) - 15, Tmin, Tmax, lat_rad, slope, aspect) # day at the midpoint of each month
    d[is.na(d)] <- 0
    Es_Solar <- Solar(lat = d$lat_rad, Jday = d$day, Tx = d$Tmax, Tn = d$Tmin, albedo=0.2, forest=0, aspect = d$aspect, slope = d$slope, printWarn=F) / 10
    values(solar_r) <- Es_Solar
    if (k == 1) {
        solar_out <- brick(solar_r)
    }
    if (k > 1) {
        solar_out <- addLayer(solar_out, solar_r)
    }
}

}

}

if(is.element(\'RH\',clim_var)){

actual_vapor_pressure_rasters_in_milibars<-list()
maximum_temperature_rasters_in_celsius<-list()
saturation_vapor_pressure_rasters_in_milibars<-list()
relative_humidity_rasters_in_decimal<-list()

for(i in 1:12){

 actual_vapor_pressure_rasters_in_milibars[[i]]<-vaporpressure_',unique_climate_scenarios[[i]],'_x_1000[[i]]/100

 maximum_temperature_rasters_in_celsius[[i]]<-tmax_',unique_climate_scenarios[[i]],'_stack[[i]]/10

 saturation_vapor_pressure_rasters_in_milibars[[i]]<-6.11*10^(7.5*maximum_temperature_rasters_in_celsius[[i]]/(237.7+maximum_temperature_rasters_in_celsius[[i]]))

 relative_humidity_rasters_in_decimal[[i]]<-actual_vapor_pressure_rasters_in_milibars[[i]]/saturation_vapor_pressure_rasters_in_milibars[[i]]

 relative_humidity_rasters_in_decimal[[i]][relative_humidity_rasters_in_decimal[[i]] > 1] <- 1

}

RH_',unique_climate_scenarios[[i]],'_stack <- relative_humidity_rasters_in_decimal_stack * 10000

}

',sep='')))

}

# Make list of units

unit_list<-list()
for(i in 1:length(var_only_list)){
  if(is.element('alt',var_only_list)){
    unit_list[i]<-'grid units: meters'
  } else if(is.element('windspeed',var_only_list)){
    unit_list[i]<-'grid units: m s-1 * 1000'
  } else if(is.element('RH',var_only_list)){
    unit_list[i]<-'grid units: relative humidity in_decimal * 10000'
  } else if(is.element('PET',var_only_list)){
    unit_list[i]<-'grid units: Potential EvapoTranspiration (in meters) based on the Priestley-Taylor equation (1972) * 100'
  } else if(is.element('AET',var_only_list)){
    unit_list[i]<-'grid units: Actual evapotranspiration derived from PET and precipitation using the -simple bucket model- * 100'
  } else if(is.element('CWD',var_only_list)){
    unit_list[i]<-'grid units: Climatic Water Deficit * 100'
  } else if(is.element('solarradiation',var_only_list)){
    unit_list[i]<-'grid units: Solar radiation at the ground surface [kJ m-2 d-1]'
  } else {
    unit_list[i]<-'grid units: unknown'
  }
}

for(i in 1:length(var_list)){

  eval(parse(text=paste('

if(length(names(',var_list[i],'_stack))==12){

names(',var_list[i],'_stack) <- c(\'',var_list[i],'_01\',
                                  \'',var_list[i],'_02\',
                                  \'',var_list[i],'_03\',
                                  \'',var_list[i],'_04\',
                                  \'',var_list[i],'_05\',
                                  \'',var_list[i],'_06\',
                                  \'',var_list[i],'_07\',
                                  \'',var_list[i],'_08\',
                                  \'',var_list[i],'_09\',
                                  \'',var_list[i],'_10\',
                                  \'',var_list[i],'_11\',
                                  \'',var_list[i],'_12\'
                                  )

if(group_by==\'month\')
{

raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_stack)

} else if(group_by==\'year\')
{

',var_list[i],'_year<-suppressMessages(mean(',var_list[i],'_stack))
names(',var_list[i],'_year)<- \'',var_list[i],'_year\'
raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_year)

} else if(group_by==\'season\')
{

if(StartSeason<=StopSeason){

',var_list[i],'_season<-suppressMessages(mean(',var_list[i],'_stack[[StartSeason:StopSeason]]))
',var_list[i],'_non_season<-suppressMessages(mean(',var_list[i],'_stack[[c(StopSeason:12,1:StartSeason)]]))

} else if(StartSeason>StopSeason){

',var_list[i],'_season<-suppressMessages(mean(',var_list[i],'_stack[[c(StartSeason:12,1:StopSeason)]]))
',var_list[i],'_non_season<-suppressMessages(mean(',var_list[i],'_stack[[StartSeason:StopSeason]]))

}

names(',var_list[i],'_non_season)<-\'',var_list[i],'_non_season\'
names(',var_list[i],'_season)<-\'',var_list[i],'_season\'

raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_non_season)
raster_stack_',scenario_list[i],'<-stack(raster_stack_',scenario_list[i],',',var_list[i],'_season)

}

}

# resolution name

if(round(res(',var_list[i],'_stack)[1], 4) == 0.0083) {resolution_name = \'30_sec\'} else
if(round(res(',var_list[i],'_stack)[1], 4) == 0.0417) {resolution_name = \'2_5_min\'} else
if(round(res(',var_list[i],'_stack)[1], 4) == 0.0833) {resolution_name = \'5_min\'} else
if(round(res(',var_list[i],'_stack)[1], 4) == 0.1667) {resolution_name = \'10_min\'} else
                                if(exists(\'resolution\')){resolution_name = resolution} else
                                                      {resolution_name = \'unknown resolution\'}


if(length(names(',var_list[i],'_stack))==12){

if(group_by==\'month\'){

if(write_raster==TRUE){
  writeRaster(',var_list[i],'_stack , filename=paste(\'',var_list[i],'_stack_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), datatype=\'INT2S\', overwrite=TRUE)
}
# output grid

projection_raster_stack <- capture.output(crs(',var_list[i],'_stack))

cat(\'------------- Raster file properties ------------\n\')
cat(\'grid names: \', names(',var_list[i],'_stack), \'\n\')
cat(\'',unit_list[i],'\')
cat(\'resolution: \', resolution_name, \'\n\')
cat(\'maximum values: \', maxValue(',var_list[i],'_stack),\'\n\')
cat(\'minimum values: \', minValue(',var_list[i],'_stack),\'\n\')
cat(\'file_name_cropped_written: \', paste(\'',var_list[i],'_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), \'\n\')
cat(\'max_longitude:\', ext[1], \'\n\',\'min_longitude:\', ext[2], \'\n\', \'max_latitude:\', ext[3], \'\n\',\'min_latitude:\',  ext[4], \'\n\')
cat(projection_raster_stack, \'\n\')
cat(\'----------------------------------------\n\')

} else if(group_by==\'year\'){

if(write_raster==TRUE){
  writeRaster(',var_list[i],'_year , filename=paste(\'',var_list[i],'_year_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), datatype=\'INT2S\', overwrite=TRUE)
}
# output grid

projection_raster_stack <- capture.output(crs(',var_list[i],'_year))

cat(\'------------- Raster file properties ------------\n\')
cat(\'grid names: \', names(',var_list[i],'_year), \'\n\')
cat(\'',unit_list[i],'\')
cat(\'resolution: \', resolution_name, \'\n\')
cat(\'maximum values: \', maxValue(',var_list[i],'_year),\'\n\')
cat(\'minimum values: \', minValue(',var_list[i],'_year),\'\n\')
cat(\'file_name_cropped_written: \', paste(\'',var_list[i],'_year_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), \'\n\')
cat(\'max_longitude:\', ext[1], \'\n\',\'min_longitude:\', ext[2], \'\n\', \'max_latitude:\', ext[3], \'\n\',\'min_latitude:\',  ext[4], \'\n\')
cat(projection_raster_stack, \'\n\')
cat(\'----------------------------------------\n\')

} else if(group_by==\'season\'){

if(write_raster==TRUE){
  writeRaster(',var_list[i],'_season , filename=paste(\'',var_list[i],'_season_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), datatype=\'INT2S\', overwrite=TRUE)
  writeRaster(',var_list[i],'_non_season , filename=paste(\'',var_list[i],'_non_season_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), datatype=\'INT2S\', overwrite=TRUE)
}

# output grid

projection_raster_stack <- capture.output(crs(',var_list[i],'_season))

cat(\'------------- Raster file properties ------------\n\')
cat(\'grid names: \', names(',var_list[i],'_season), \'\n\')
cat(\'',unit_list[i],'\')
cat(\'resolution: \', resolution_name, \'\n\')
cat(\'maximum values: \', maxValue(',var_list[i],'_season),\'\n\')
cat(\'minimum values: \', minValue(',var_list[i],'_season),\'\n\')
cat(\'file_name_cropped_written: \', paste(\'',var_list[i],'_season_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), \'\n\')
cat(\'max_longitude:\', ext[1], \'\n\',\'min_longitude:\', ext[2], \'\n\', \'max_latitude:\', ext[3], \'\n\',\'min_latitude:\',  ext[4], \'\n\')
cat(projection_raster_stack, \'\n\')
cat(\'----------------------------------------\n\')

# output grid

projection_raster_stack <- capture.output(crs(',var_list[i],'_non_season))

cat(\'------------- Raster file properties ------------\n\')
cat(\'grid names: \', names(',var_list[i],'_non_season), \'\n\')
cat(\'',unit_list[i],'\')
cat(\'resolution: \', resolution_name, \'\n\')
cat(\'maximum values: \', maxValue(',var_list[i],'_non_season),\'\n\')
cat(\'minimum values: \', minValue(',var_list[i],'_non_season),\'\n\')
cat(\'file_name_cropped_written: \', paste(\'',var_list[i],'_non_season_\', name, \'_res_\', resolution_name, \'_\' ,group_by ,sep=\'\'), \'\n\')
cat(\'max_longitude:\', ext[1], \'\n\',\'min_longitude:\', ext[2], \'\n\', \'max_latitude:\', ext[3], \'\n\',\'min_latitude:\',  ext[4], \'\n\')
cat(projection_raster_stack, \'\n\')
cat(\'----------------------------------------\n\')

}

} else {

if(write_raster==TRUE){
  writeRaster(',var_list[i],'_stack , filename=paste(\'',var_list[i],'_stack_\', name, \'_res_\', resolution_name, \'_\' ,sep=\'\'), datatype=\'INT2S\', overwrite=TRUE)
}
# output grid

projection_raster_stack <- capture.output(crs(',var_list[i],'_stack))

cat(\'------------- Raster file properties ------------\n\')
cat(\'grid names: \', names(',var_list[i],'_stack), \'\n\')
cat(\'',unit_list[i],'\')
cat(\'resolution: \', resolution_name, \'\n\')
cat(\'maximum values: \', maxValue(',var_list[i],'_stack),\'\n\')
cat(\'minimum values: \', minValue(',var_list[i],'_stack),\'\n\')
cat(\'file_name_cropped_written: \', paste(\'',var_list[i],'_\', name, \'_res_\', resolution_name, \'_\' ,sep=\'\'), \'\n\')
cat(\'max_longitude:\', ext[1], \'\n\',\'min_longitude:\', ext[2], \'\n\', \'max_latitude:\', ext[3], \'\n\',\'min_latitude:\',  ext[4], \'\n\')
cat(projection_raster_stack, \'\n\')
cat(\'----------------------------------------\n\')

}

',sep='')))

}


final_raster_list<-list()

for(i in 1:length(unique(scenario_list))){

  eval(parse(text=paste('

if(length(raster_stack_',unique(scenario_list)[i],')>0){
 suppressWarnings(final_raster_list[i]<-raster_stack_',unique(scenario_list)[i],')
}

',sep='')))

}

names(final_raster_list)<-unique(scenario_list)

setwd(master_directory)

return(final_raster_list)

}


testvar<-EcoRasters(
  geo_var='alt',
  clim_var=c('CWD'),
  scenarios=c('present','2050rcp26'),
  ext=extent(-70,-60,-10,0),
  group_by='season',
  download_rasters=T,
  resolution=10,
  StartSeason=11,
  StopSeason=3
)
