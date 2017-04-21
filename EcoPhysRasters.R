function(rasterpath,

         hahrmethod='Senoid',

         RichCoef=NULL,
         Tupr=NA,
         Tlwr=NA,
         hcap=NA,
         hr_res=3,

         Breeding=F,
         StartBreeding=1,
         StopBreeding=12,
         StartBreeding_frac=1,
         StopBreeding_frac=1 ,

         diel=NULL,

         scenarios=c('all'),
         variables=c('all'),

         sp_coord=NULL,

         PerfFUN=NA,
         size,
         acclag=NA,
         EWLPerf=F,
         EWLCoef,
         dehydrationtime=1,
         EWLdry=T,
         EWLshade=T,


         margin=0,
         Lon_max=180,
         Lon_min=-180,
         Lat_max=90,
         Lat_min=-60,
         res,
         write_rasters=F){


  # require('raster')
  # require('rlist')
  # require('mgcv')

  ##########################################################################################
  ###################            Load and crop rasters             #########################
  ##########################################################################################

  #

  # Crop extent

  if (is.null(sp_coord)) {
    ext_species <- extent((Lon_min-margin), (Lon_max+margin), (Lat_min-margin), (Lat_max+margin))
  } else {
    ext_species <- extent((min(sp_coord$Lon)-margin), (max(sp_coord$Lon)+margin), (min(sp_coord$Lat)-margin), (max(sp_coord$Lat)+margin))
  }

  if(isTRUE(res=='30s')){

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Radiation',variables)){

      altitude_30s_file <- list.files(path =  paste(rasterpath,"/alt_30s_bil",sep="") , pattern="*.bil$",full.names=T, ignore.case=T)

      altitude_raster <- stack(altitude_30s_file)

      alt_crop <- crop (altitude_raster, ext_species)
      alt_stack <- stack(alt_crop)


    }
  }

  if(isTRUE(res=='2.5m')){

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Radiation',variables)){

      altitude_30s_file <- list.files(path =  paste(rasterpath,"/alt_2-5m_bil",sep="") , pattern="*.bil$",full.names=T, ignore.case=T)

      altitude_raster <- stack(altitude_30s_file)

      alt_crop <- crop (altitude_raster, ext_species)
      alt_stack <- stack(alt_crop)

    }
  }

  if(isTRUE(res=='5m')){

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Radiation',variables)){

      altitude_30s_file <- list.files(path =  paste(rasterpath,"/alt_5m_bil",sep="") , pattern="*.bil$",full.names=T, ignore.case=T)

      altitude_raster <- stack(altitude_30s_file)

      alt_crop <- crop (altitude_raster, ext_species)
      alt_stack <- stack(alt_crop)


    }
  }

  if(isTRUE(res=='10m')){

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Radiation',variables)){

      altitude_30s_file <- list.files(path =  paste(rasterpath,"/alt_10m_bil",sep="") , pattern="*.bil$",full.names=T, ignore.case=T)

      altitude_raster <- stack(altitude_30s_file)

      alt_crop <- crop (altitude_raster, ext_species)
      alt_stack <- stack(alt_crop)


    }
  }


  scenario_names<-c()
  all_crop<-list()

  #####Present---1975######

  if ( isTRUE(scenarios=='all') | is.element('present',scenarios)) {

    ##########################################################################################
    ###################                30 seconds grids              #########################
    ##########################################################################################

    if(isTRUE(res=='30s')){

      if(!is.null(diel)){
        sunlight_30_sec_file <- list.files(path = paste(rasterpath,"/sun_30s",sep=""), pattern="*.grd$",full.names=T, ignore.case=T)

        sunlight_raster <- stack(sunlight_30_sec_file)

        sunlight_crop <- crop (sunlight_raster , ext_species)

        sunlight_crop <- subset(sunlight_crop, c(1,5,6,7,8,9,10,11,12,2,3,4))


        names(sunlight_crop) <- c("daylength_jan", "daylength_feb", "daylength_mar", "daylength_apr", "daylength_may", "daylength_jun", "daylength_jul", "daylength_aug", "daylength_sep", "daylength_oct", "daylength_nov", "daylength_dec")


      }
      global_grids_present<-c()

      if(is.element('EWL',variables) | EWLPerf == T){
        rh_path=paste(rasterpath,"/rh_30s_bil",sep="")
        rh_present_30s_file <- list.files(path = rh_path, pattern="*.grd$",full.names=T, ignore.case=T)
        rh_present_30s_raster <- stack(rh_present_30s_file)
        rh_present_30s <- subset(rh_present_30s_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        rh_crop_present <- crop (rh_present_30s , ext_species)
        ext_species<-rh_crop_present@extent

        global_grids_present <- c(global_grids_present,rh_crop_present)

        rhindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
        prec_path=paste(rasterpath,"/prec_30s_bil",sep="")
        precipitation_present_30s_file <- list.files(path = prec_path, pattern="*.bil$",full.names=T, ignore.case=T)
        precipitation_present_30s_raster <- stack(precipitation_present_30s_file)
        precipitation_present_30s <- subset(precipitation_present_30s_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        precipitation_crop_present <- crop (precipitation_present_30s , ext_species)
        ext_species<-precipitation_crop_present@extent

        global_grids_present <- c(global_grids_present,precipitation_crop_present)

        precindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables) | !is.null(PerfFUN)| !is.null(PerfFUN)){
        tmax_path=paste(rasterpath,"/tmax_30s_bil",sep="")
        tmax_present_30s_file <- list.files(path = tmax_path, pattern="*.bil$",full.names=T, ignore.case=T)
        tmax_present_30s_file_raster <- stack(tmax_present_30s_file)
        tmax_present_30s <- subset(tmax_present_30s_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmax_crop_present <- crop (tmax_present_30s, ext_species)
        ext_species<-tmax_crop_present@extent

        global_grids_present <- c(global_grids_present,tmax_crop_present)

        tmaxindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
        tmin_path=paste(rasterpath,"/tmin_30s_bil",sep="")
        tmin_present_30s_file <- list.files(path = tmin_path, pattern="*.bil$",full.names=T, ignore.case=T)
        tmin_present_30s_file_raster <- stack(tmin_present_30s_file)
        tmin_present_30s <- subset(tmin_present_30s_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmin_crop_present <- crop (tmin_present_30s, ext_species)
        ext_species<-tmin_crop_present@extent

        global_grids_present <- c(global_grids_present,tmin_crop_present)

        tminindex<-length(global_grids_present)

      }

    }

    ##########################################################################################
    ###################               2.5 minutes grids              #########################
    ##########################################################################################

    if(isTRUE(res=='2.5m')){

      if(!is.null(diel)){
        sunlight_30_sec_file <- list.files(path = paste(rasterpath,"/sun_2-5m",sep=""), pattern="*.grd$",full.names=T, ignore.case=T)

        sunlight_raster <- stack(sunlight_30_sec_file)

        sunlight_crop <- crop (sunlight_raster , ext_species)

        sunlight_crop <- subset(sunlight_crop, c(1,5,6,7,8,9,10,11,12,2,3,4))

        names(sunlight_crop) <- c("daylength_jan", "daylength_feb", "daylength_mar", "daylength_apr", "daylength_may", "daylength_jun", "daylength_jul", "daylength_aug", "daylength_sep", "daylength_oct", "daylength_nov", "daylength_dec")


      }

      global_grids_present<-c()

      if(is.element('EWL',variables)| EWLPerf==T){
        rh_path=paste(rasterpath,"/rh_2-5m_bil",sep="")
        rh_present_2_5_min_file <- list.files(path = rh_path, pattern="*.grd$",full.names=T, ignore.case=T)
        rh_present_2_5_min_raster <- stack(rh_present_2_5_min_file)
        rh_present_2_5_min <- subset(rh_present_2_5_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        rh_crop_present <- crop (rh_present_2_5_min , ext_species)
        ext_species<-rh_crop_present@extent

        global_grids_present <- c(global_grids_present,rh_crop_present)

        rhindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
        precipitation_present_2_5_min_file <- list.files(path = paste(rasterpath,"/prec_2-5m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        precipitation_present_2_5_min_raster <- stack(precipitation_present_2_5_min_file)
        precipitation_present_2_5_min <- subset(precipitation_present_2_5_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        precipitation_crop_present <- crop (precipitation_present_2_5_min , ext_species)
        ext_species<-precipitation_crop_present@extent

        global_grids_present <- c(global_grids_present,precipitation_crop_present)

        precindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
        tmax_present_2_5_min_file <- list.files(path = paste(rasterpath,"/tmax_2-5m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        tmax_present_2_5_min_file_raster <- stack(tmax_present_2_5_min_file)
        tmax_present_2_5_min <- subset(tmax_present_2_5_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmax_crop_present <- crop (tmax_present_2_5_min, ext_species)
        ext_species<-tmax_crop_present@extent

        global_grids_present <- c(global_grids_present,tmax_crop_present)

        tmaxindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
        tmin_present_2_5_min_file <- list.files(path = paste(rasterpath,"/tmin_2-5m_bil",sep="")	, pattern="*.bil$",full.names=T, ignore.case=T)
        tmin_present_2_5_min_file_raster <- stack(tmin_present_2_5_min_file)
        tmin_present_2_5_min <- subset(tmin_present_2_5_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmin_crop_present <- crop (tmin_present_2_5_min, ext_species)
        ext_species<-tmin_crop_present@extent

        global_grids_present <- c(global_grids_present,tmin_crop_present)

        tminindex<-length(global_grids_present)

      }

    }

    ##########################################################################################
    ###################                 5 minutes grids              #########################
    ##########################################################################################

    if(isTRUE(res=='5m')){

      if(!is.null(diel)){
        sunlight_30_sec_file <- list.files(path = paste(rasterpath,"/sun_5m",sep=""), pattern="*.grd$",full.names=T, ignore.case=T)

        sunlight_raster <- stack(sunlight_30_sec_file)

        sunlight_crop <- crop (sunlight_raster , ext_species)

        sunlight_crop <- subset(sunlight_crop, c(1,5,6,7,8,9,10,11,12,2,3,4))

        names(sunlight_crop) <- c("daylength_jan", "daylength_feb", "daylength_mar", "daylength_apr", "daylength_may", "daylength_jun", "daylength_jul", "daylength_aug", "daylength_sep", "daylength_oct", "daylength_nov", "daylength_dec")

      }

      global_grids_present<-c()

      if(is.element('EWL',variables)| EWLPerf==T){
        rh_path=paste(rasterpath,"/rh_5m_bil",sep="")
        rh_present_5_min_file <- list.files(path = rh_path, pattern="*.grd$",full.names=T, ignore.case=T)
        rh_present_5_min_raster <- stack(rh_present_5_min_file)
        rh_present_5_min <- subset(rh_present_5_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        rh_crop_present <- crop (rh_present_5_min , ext_species)
        ext_species<-rh_crop_present@extent

        global_grids_present <- c(global_grids_present,rh_crop_present)

        rhindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
        precipitation_present_5_min_file <- list.files(path = paste(rasterpath,"/prec_5m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        precipitation_present_5_min_raster <- stack(precipitation_present_5_min_file)
        precipitation_present_5_min <- subset(precipitation_present_5_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        precipitation_crop_present <- crop (precipitation_present_5_min , ext_species)
        ext_species<-precipitation_crop_present@extent

        global_grids_present <- c(global_grids_present,precipitation_crop_present)

        precindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
        tmax_present_5_min_file <- list.files(path = paste(rasterpath,"/tmax_5m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        tmax_present_5_min_file_raster <- stack(tmax_present_5_min_file)
        tmax_present_5_min <- subset(tmax_present_5_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmax_crop_present <- crop (tmax_present_5_min, ext_species)
        ext_species<-tmax_crop_present@extent

        global_grids_present <- c(global_grids_present,tmax_crop_present)

        tmaxindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
        tmin_present_5_min_file <- list.files(path = paste(rasterpath,"/tmin_5m_bil",sep="")	, pattern="*.bil$",full.names=T, ignore.case=T)
        tmin_present_5_min_file_raster <- stack(tmin_present_5_min_file)
        tmin_present_5_min <- subset(tmin_present_5_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmin_crop_present <- crop (tmin_present_5_min, ext_species)
        ext_species<-tmin_crop_present@extent

        global_grids_present <- c(global_grids_present,tmin_crop_present)

        tminindex<-length(global_grids_present)

      }

    }

    ##########################################################################################
    ###################                 10 minutes grids             #########################
    ##########################################################################################

    if(isTRUE(res=='10m')){

      if(!is.null(diel)){
        sunlight_30_sec_file <- list.files(path = paste(rasterpath,"/sun_10m",sep=""), pattern="*.grd$",full.names=T, ignore.case=T)

        sunlight_raster <- stack(sunlight_30_sec_file)

        sunlight_crop <- crop (sunlight_raster , ext_species)

        sunlight_crop <- subset(sunlight_crop, c(1,5,6,7,8,9,10,11,12,2,3,4))

        names(sunlight_crop) <- c("daylength_jan", "daylength_feb", "daylength_mar", "daylength_apr", "daylength_may", "daylength_jun", "daylength_jul", "daylength_aug", "daylength_sep", "daylength_oct", "daylength_nov", "daylength_dec")

      }


      global_grids_present<-c()

      if(is.element('EWL',variables)| EWLPerf==T){
        rh_path=paste(rasterpath,"/rh_10m_bil",sep="")
        rh_present_10_min_file <- list.files(path = rh_path, pattern="*.grd$",full.names=T, ignore.case=T)
        rh_present_10_min_raster <- stack(rh_present_10_min_file)
        rh_present_10_min <- subset(rh_present_10_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        rh_crop_present <- crop (rh_present_10_min , ext_species)
        ext_species<-rh_crop_present@extent

        global_grids_present <- c(global_grids_present,rh_crop_present)

        rhindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('Precipitation',variables) | is.element('ET',variables)){
        precipitation_present_10_min_file <- list.files(path = paste(rasterpath,"/prec_10m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        precipitation_present_10_min_raster <- stack(precipitation_present_10_min_file)
        precipitation_present_10_min <- subset(precipitation_present_10_min_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        precipitation_crop_present <- crop (precipitation_present_10_min , ext_species)
        ext_species<-precipitation_crop_present@extent

        global_grids_present <- c(global_grids_present,precipitation_crop_present)

        precindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
        tmax_present_10_min_file <- list.files(path = paste(rasterpath,"/tmax_10m_bil",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
        tmax_present_10_min_file_raster <- stack(tmax_present_10_min_file)
        tmax_present_10_min <- subset(tmax_present_10_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmax_crop_present <- crop (tmax_present_10_min, ext_species)
        ext_species<-tmax_crop_present@extent

        global_grids_present <- c(global_grids_present,tmax_crop_present)

        tmaxindex<-length(global_grids_present)

      }

      if(is.element('all',variables) | is.element('Radiation',variables) |  is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
        tmin_present_10_min_file <- list.files(path = paste(rasterpath,"/tmin_10m_bil",sep="")	, pattern="*.bil$",full.names=T, ignore.case=T)
        tmin_present_10_min_file_raster <- stack(tmin_present_10_min_file)
        tmin_present_10_min <- subset(tmin_present_10_min_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

        tmin_crop_present <- crop (tmin_present_10_min, ext_species)
        ext_species<-tmin_crop_present@extent

        global_grids_present <- c(global_grids_present,tmin_crop_present)

        tminindex<-length(global_grids_present)

      }

    }

    all_crop[[length(all_crop)+1]]<-global_grids_present
    scenario_names<-c(scenario_names,'present')

  }

  #####Future---2050---rcp 26 ######

  if ( isTRUE(scenarios=='all') | is.element('2050rcp26',scenarios)) {

    global_grids_2050_rcp26<-c()

    if(is.element('EWL',variables)| EWLPerf==T){

      rh_2050_rcp26_file <- list.files(path = paste(rasterpath,"/mp26rh50",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2050_rcp26_raster <- stack(rh_2050_rcp26_file)
      rh_2050_rcp26 <- subset(rh_2050_rcp26_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2050_rcp26 <- crop (rh_2050_rcp26 , ext_species)
      ext_species<-rh_crop_2050_rcp26@extent

      global_grids_2050_rcp26 <- c(global_grids_2050_rcp26,rh_crop_2050_rcp26)

      rhindex<-length(global_grids_2050_rcp26)

    }


    if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
      precipitation_2050_rcp26_file <- list.files(path = paste(rasterpath,"/mp26pr50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2050_rcp26_raster <- stack(precipitation_2050_rcp26_file)
      precipitation_2050_rcp26 <- subset(precipitation_2050_rcp26_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2050_rcp26 <- crop (precipitation_2050_rcp26 , ext_species)
      ext_species<-precipitation_crop_2050_rcp26@extent

      global_grids_2050_rcp26 <- c(global_grids_2050_rcp26,precipitation_crop_2050_rcp26)

      precindex<-length(global_grids_2050_rcp26)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
      tmax_2050_rcp26_file <- list.files(path = paste(rasterpath,"/mp26tx50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2050_rcp26_file_raster <- stack(tmax_2050_rcp26_file)
      tmax_2050_rcp26 <- subset(tmax_2050_rcp26_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2050_rcp26 <- crop (tmax_2050_rcp26, ext_species)
      ext_species<-tmax_crop_2050_rcp26@extent

      global_grids_2050_rcp26 <- c(global_grids_2050_rcp26,tmax_crop_2050_rcp26)

      tmaxindex<-length(global_grids_2050_rcp26)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2050_rcp26_file <- list.files(path = paste(rasterpath,"/mp26tn50",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2050_rcp26_file_raster <- stack(tmin_2050_rcp26_file)
      tmin_2050_rcp26 <- subset(tmin_2050_rcp26_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2050_rcp26 <- crop (tmin_2050_rcp26, ext_species)
      ext_species<-tmin_crop_2050_rcp26@extent

      global_grids_2050_rcp26 <- c(global_grids_2050_rcp26,tmin_crop_2050_rcp26)

      tminindex<-length(global_grids_2050_rcp26)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2050_rcp26
    scenario_names<-c(scenario_names,'2050rcp26')

  }

  #####Future---2050---rcp 45 ######

  if ( isTRUE(scenarios=='all') | is.element('2050rcp45',scenarios)) {

    global_grids_2050_rcp45<-c()

    if(is.element('EWL',variables)| EWLPerf==T){

      rh_2050_rcp45_file <- list.files(path = paste(rasterpath,"/mp45rh50",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2050_rcp45_raster <- stack(rh_2050_rcp45_file)
      rh_2050_rcp45 <- subset(rh_2050_rcp45_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2050_rcp45 <- crop (rh_2050_rcp45 , ext_species)
      ext_species<-rh_crop_2050_rcp45@extent

      global_grids_2050_rcp45 <- c(global_grids_2050_rcp45,rh_crop_2050_rcp45)

      rhindex<-length(global_grids_2050_rcp45)

    }

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
      precipitation_2050_rcp45_file <- list.files(path = paste(rasterpath,"/mp45pr50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2050_rcp45_raster <- stack(precipitation_2050_rcp45_file)
      precipitation_2050_rcp45 <- subset(precipitation_2050_rcp45_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2050_rcp45 <- crop (precipitation_2050_rcp45 , ext_species)
      ext_species<-precipitation_crop_2050_rcp45@extent

      global_grids_2050_rcp45 <- c(global_grids_2050_rcp45,precipitation_crop_2050_rcp45)

      precindex<-length(global_grids_2050_rcp45)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
      tmax_2050_rcp45_file <- list.files(path = paste(rasterpath,"/mp45tx50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2050_rcp45_file_raster <- stack(tmax_2050_rcp45_file)
      tmax_2050_rcp45 <- subset(tmax_2050_rcp45_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2050_rcp45 <- crop (tmax_2050_rcp45, ext_species)
      ext_species<-tmax_crop_2050_rcp45@extent

      global_grids_2050_rcp45 <- c(global_grids_2050_rcp45,tmax_crop_2050_rcp45)

      tmaxindex<-length(global_grids_2050_rcp45)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2050_rcp45_file <- list.files(path = paste(rasterpath,"/mp45tn50",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2050_rcp45_file_raster <- stack(tmin_2050_rcp45_file)
      tmin_2050_rcp45 <- subset(tmin_2050_rcp45_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2050_rcp45 <- crop (tmin_2050_rcp45, ext_species)
      ext_species<-tmin_crop_2050_rcp45@extent

      global_grids_2050_rcp45 <- c(global_grids_2050_rcp45,tmin_crop_2050_rcp45)

      tminindex<-length(global_grids_2050_rcp45)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2050_rcp45
    scenario_names<-c(scenario_names,'2050rcp45')

  }

  #####Future---2050---rcp 85 ######

  if ( isTRUE(scenarios=='all') | is.element('2050rcp85',scenarios)) {

    global_grids_2050_rcp85<-c()

    if(is.element('EWL',variables)| EWLPerf==T){

      rh_2050_rcp85_file <- list.files(path = paste(rasterpath,"/mp85rh50",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2050_rcp85_raster <- stack(rh_2050_rcp85_file)
      rh_2050_rcp85 <- subset(rh_2050_rcp85_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2050_rcp85 <- crop (rh_2050_rcp85 , ext_species)
      ext_species<-rh_crop_2050_rcp85@extent

      global_grids_2050_rcp85 <- c(global_grids_2050_rcp85,rh_crop_2050_rcp85)

      rhindex<-length(global_grids_2050_rcp85)

    }

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
      precipitation_2050_rcp85_file <- list.files(path = paste(rasterpath,"/mp85pr50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2050_rcp85_raster <- stack(precipitation_2050_rcp85_file)
      precipitation_2050_rcp85 <- subset(precipitation_2050_rcp85_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2050_rcp85 <- crop (precipitation_2050_rcp85 , ext_species)
      ext_species<-precipitation_crop_2050_rcp85@extent

      global_grids_2050_rcp85 <- c(global_grids_2050_rcp85,precipitation_crop_2050_rcp85)

      precindex<-length(global_grids_2050_rcp85)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
      tmax_2050_rcp85_file <- list.files(path = paste(rasterpath,"/mp85tx50",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2050_rcp85_file_raster <- stack(tmax_2050_rcp85_file)
      tmax_2050_rcp85 <- subset(tmax_2050_rcp85_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2050_rcp85 <- crop (tmax_2050_rcp85, ext_species)
      ext_species<-tmax_crop_2050_rcp85@extent

      global_grids_2050_rcp85 <- c(global_grids_2050_rcp85,tmax_crop_2050_rcp85)

      tmaxindex<-length(global_grids_2050_rcp85)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2050_rcp85_file <- list.files(path = paste(rasterpath,"/mp85tn50",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2050_rcp85_file_raster <- stack(tmin_2050_rcp85_file)
      tmin_2050_rcp85 <- subset(tmin_2050_rcp85_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2050_rcp85 <- crop (tmin_2050_rcp85, ext_species)
      ext_species<-tmin_crop_2050_rcp85@extent

      global_grids_2050_rcp85 <- c(global_grids_2050_rcp85,tmin_crop_2050_rcp85)

      tminindex<-length(global_grids_2050_rcp85)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2050_rcp85
    scenario_names<-c(scenario_names,'2050rcp85')

  }

  #####Future---2070---rcp 26 ######

  if ( isTRUE(scenarios=='all') | is.element('2070rcp26',scenarios)) {

    global_grids_2070_rcp26<-c()

    if(is.element('EWL',variables)| EWLPerf==T){

      rh_2070_rcp26_file <- list.files(path = paste(rasterpath,"/mp26rh70",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2070_rcp26_raster <- stack(rh_2070_rcp26_file)
      rh_2070_rcp26 <- subset(rh_2070_rcp26_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2070_rcp26 <- crop (rh_2070_rcp26 , ext_species)
      ext_species<-rh_crop_2070_rcp26@extent

      global_grids_2070_rcp26 <- c(global_grids_2070_rcp26,rh_crop_2070_rcp26)

      rhindex<-length(global_grids_2070_rcp26)

    }

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
      precipitation_2070_rcp26_file <- list.files(path = paste(rasterpath,"/mp26pr70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2070_rcp26_raster <- stack(precipitation_2070_rcp26_file)
      precipitation_2070_rcp26 <- subset(precipitation_2070_rcp26_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2070_rcp26 <- crop (precipitation_2070_rcp26 , ext_species)
      ext_species<-precipitation_crop_2070_rcp26@extent

      global_grids_2070_rcp26 <- c(global_grids_2070_rcp26,precipitation_crop_2070_rcp26)

      precindex<-length(global_grids_2070_rcp26)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables)| !is.null(PerfFUN)){
      tmax_2070_rcp26_file <- list.files(path = paste(rasterpath,"/mp26tx70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2070_rcp26_file_raster <- stack(tmax_2070_rcp26_file)
      tmax_2070_rcp26 <- subset(tmax_2070_rcp26_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2070_rcp26 <- crop (tmax_2070_rcp26, ext_species)
      ext_species<-tmax_crop_2070_rcp26@extent

      global_grids_2070_rcp26 <- c(global_grids_2070_rcp26,tmax_crop_2070_rcp26)

      tmaxindex<-length(global_grids_2070_rcp26)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2070_rcp26_file <- list.files(path = paste(rasterpath,"/mp26tn70",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2070_rcp26_file_raster <- stack(tmin_2070_rcp26_file)
      tmin_2070_rcp26 <- subset(tmin_2070_rcp26_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2070_rcp26 <- crop (tmin_2070_rcp26, ext_species)
      ext_species<-tmin_crop_2070_rcp26@extent

      global_grids_2070_rcp26 <- c(global_grids_2070_rcp26,tmin_crop_2070_rcp26)

      tminindex<-length(global_grids_2070_rcp26)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2070_rcp26
    scenario_names<-c(scenario_names,'2070rcp26')

  }

  #####Future---2070---rcp 45 ######

  if ( isTRUE(scenarios=='all') | is.element('2070rcp45',scenarios)) {

    global_grids_2070_rcp45<-c()

    if(is.element('EWL',variables)| EWLPerf==T){

      rh_2070_rcp45_file <- list.files(path = paste(rasterpath,"/mp45rh70",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2070_rcp45_raster <- stack(rh_2070_rcp45_file)
      rh_2070_rcp45 <- subset(rh_2070_rcp45_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2070_rcp45 <- crop (rh_2070_rcp45 , ext_species)
      ext_species<-rh_crop_2070_rcp45@extent

      global_grids_2070_rcp45 <- c(global_grids_2070_rcp45,rh_crop_2070_rcp45)

      rhindex<-length(global_grids_2070_rcp45)

    }

    if(is.element('all',variables) | is.element('ET',variables) | is.element('Precipitation',variables)){
      precipitation_2070_rcp45_file <- list.files(path = paste(rasterpath,"/mp45pr70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2070_rcp45_raster <- stack(precipitation_2070_rcp45_file)
      precipitation_2070_rcp45 <- subset(precipitation_2070_rcp45_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2070_rcp45 <- crop (precipitation_2070_rcp45 , ext_species)
      ext_species<-precipitation_crop_2070_rcp45@extent

      global_grids_2070_rcp45 <- c(global_grids_2070_rcp45,precipitation_crop_2070_rcp45)

      precindex<-length(global_grids_2070_rcp45)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables) | !is.null(PerfFUN)){
      tmax_2070_rcp45_file <- list.files(path = paste(rasterpath,"/mp45tx70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2070_rcp45_file_raster <- stack(tmax_2070_rcp45_file)
      tmax_2070_rcp45 <- subset(tmax_2070_rcp45_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2070_rcp45 <- crop (tmax_2070_rcp45, ext_species)
      ext_species<-tmax_crop_2070_rcp45@extent

      global_grids_2070_rcp45 <- c(global_grids_2070_rcp45,tmax_crop_2070_rcp45)

      tmaxindex<-length(global_grids_2070_rcp45)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2070_rcp45_file <- list.files(path = paste(rasterpath,"/mp45tn70",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2070_rcp45_file_raster <- stack(tmin_2070_rcp45_file)
      tmin_2070_rcp45 <- subset(tmin_2070_rcp45_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2070_rcp45 <- crop (tmin_2070_rcp45, ext_species)
      ext_species<-tmin_crop_2070_rcp45@extent

      global_grids_2070_rcp45 <- c(global_grids_2070_rcp45,tmin_crop_2070_rcp45)

      tminindex<-length(global_grids_2070_rcp45)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2070_rcp45
    scenario_names<-c(scenario_names,'2070rcp45')

  }

  #####Future---2070---rcp 85 ######

  if ( isTRUE(scenarios=='all') | is.element('2070rcp85',scenarios)) {

    global_grids_2070_rcp85<-c()

    if(is.element('EWL',variables) | EWLPerf==T){

      rh_2070_rcp85_file <- list.files(path = paste(rasterpath,"/mp85rh70",sep=""), pattern="*.bil$",full.names=T, ignore.case=T)
      rh_2070_rcp85_raster <- stack(rh_2070_rcp85_file)
      rh_2070_rcp85 <- subset(rh_2070_rcp85_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      rh_crop_2070_rcp85 <- crop (rh_2070_rcp85 , ext_species)
      ext_species<-rh_crop_2070_rcp85@extent

      global_grids_2070_rcp85 <- c(global_grids_2070_rcp85,rh_crop_2070_rcp85)

      rhindex<-length(global_grids_2070_rcp85)

    }

    if(is.element('all',variables) | is.element('Precipitation',variables) | is.element('ET',variables)){
      precipitation_2070_rcp85_file <- list.files(path = paste(rasterpath,"/mp85pr70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      precipitation_2070_rcp85_raster <- stack(precipitation_2070_rcp85_file)
      precipitation_2070_rcp85 <- subset(precipitation_2070_rcp85_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      precipitation_crop_2070_rcp85 <- crop (precipitation_2070_rcp85 , ext_species)
      ext_species<-precipitation_crop_2070_rcp85@extent

      global_grids_2070_rcp85 <- c(global_grids_2070_rcp85,precipitation_crop_2070_rcp85)

      precindex<-length(global_grids_2070_rcp85)

    }

    if(is.element('all',variables) | is.element('EWL',variables) | is.element('ha',variables) | is.element('hr',variables) | is.element('ET',variables) | is.element('Radiation',variables) | !is.null(PerfFUN)){
      tmax_2070_rcp85_file <- list.files(path = paste(rasterpath,"/mp85tx70",sep=""), pattern="*.tif$",full.names=T, ignore.case=T)
      tmax_2070_rcp85_file_raster <- stack(tmax_2070_rcp85_file)
      tmax_2070_rcp85 <- subset(tmax_2070_rcp85_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmax_crop_2070_rcp85 <- crop (tmax_2070_rcp85, ext_species)
      ext_species<-tmax_crop_2070_rcp85@extent

      global_grids_2070_rcp85 <- c(global_grids_2070_rcp85,tmax_crop_2070_rcp85)

      tmaxindex<-length(global_grids_2070_rcp85)

    }

    if(is.element('all',variables) | is.element('Radiation',variables) | is.element('ET',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_2070_rcp85_file <- list.files(path = paste(rasterpath,"/mp85tn70",sep="")	, pattern="*.tif$",full.names=T, ignore.case=T)
      tmin_2070_rcp85_file_raster <- stack(tmin_2070_rcp85_file)
      tmin_2070_rcp85 <- subset(tmin_2070_rcp85_file_raster, c(1,5,6,7,8,9,10,11,12,2,3,4))

      tmin_crop_2070_rcp85 <- crop (tmin_2070_rcp85, ext_species)
      ext_species<-tmin_crop_2070_rcp85@extent

      global_grids_2070_rcp85 <- c(global_grids_2070_rcp85,tmin_crop_2070_rcp85)

      tminindex<-length(global_grids_2070_rcp85)

    }

    all_crop[[length(all_crop)+1]]<-global_grids_2070_rcp85
    scenario_names<-c(scenario_names,'2070rcp85')

  }

  #EcoPhys

  ##########################################################################################
  ##########################     From raster to matrices    ################################
  ##########################################################################################

  if(is.element('all',variables) | is.element('Performance',variables) | is.element('Precipitation',variables)  | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid' | is.element('EWL',variables) | EWLPerf==T){

    if(!is.null(diel)){

      #### for stack 12 bands for sunlight

      sunlight_seq_year_list <-list()

      for (j in 1:12) {
        sun_bin<-as.matrix(sunlight_raster[[j]])
        (sun_bin[sun_bin==-32768] <- NA) #assign -999 to missing
        sunlight_seq_year_list[[j]] <- sun_bin
      }

      ####for stack 12 bands for sunlight -- logical

      sunlight_matrix_logical <- list() #create an empty list

      for(j in 1:12){    # 1st to 12th month
        temp <- sunlight_seq_year_list[[j]] >= 12 #more or equal to 12 hours of sunlight is consider summer
        sunlight_matrix_logical[[j]] <- temp
        rm(temp)
      }

    }


    matrix_list<-list()

    pb <- txtProgressBar(min = 0, max = length(all_crop), style = 3)

    for(i in 1:length(all_crop)){

      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, i)

      matrix_var_list<-list()

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


      if(is.element('all',variables) | is.element('Precipitation',variables)){

        ####for stack 12 bands for prec

        prec_seq_year_list <-list()

        for (j in 1:12) {
          prec_bin<-as.matrix(all_crop[[i]][[precindex]][[j]])
          (prec_bin[prec_bin==-32768] <- NA) #assign -999 to missing
          prec_seq_year_list[[j]] <- prec_bin

        }

        matrix_var_list[[length(matrix_var_list) + 1]]<-prec_seq_year_list

      }

      if(is.element('all',variables) | is.element('Performance',variables) | is.element('EWL',variables) | is.element('ha',variables)  & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){

        ####for stack 12 bands for tmax

        tmax_seq_year_list <-list()

        for (j in 1:12) {
          tmax_bin<-as.matrix(all_crop[[i]][[tmaxindex]][[j]])
          (tmax_bin[tmax_bin==-32768] <- NA) #assign -999 to missing
          tmax_seq_year_list[[j]] <- tmax_bin
        }

        matrix_var_list[[length(matrix_var_list) + 1]]<-tmax_seq_year_list

      }

      if(is.element('all',variables) | is.element('ha',variables)  & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){

        ####for stack 12 bands for tmin

        tmin_seq_year_list <-list()

        for (j in 1:12) {
          tmin_bin<-as.matrix(all_crop[[i]][[tminindex]][[j]])
          (tmin_bin[tmin_bin==-32768] <- NA) #assign -999 to missing
          tmin_seq_year_list[[j]] <- tmin_bin
        }

        matrix_var_list[[length(matrix_var_list) + 1]]<-tmin_seq_year_list

      }

      matrix_list[[i]]<-matrix_var_list

    }

    close(pb)

    names(matrix_list)<-scenario_names
  }

  ##########################################################################################
  ##########################      Create EcoPhysRasters     ################################
  ##########################################################################################

  EcoPhysRasters<-list()

  pb <- txtProgressBar(min = 0, max = length(all_crop), style = 3)

  for(q in 1:length(all_crop)){

    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, q)


    varlist<-list()
    varnames<-c()

    if(is.element('EWL',variables) | EWLPerf==T){
      rh_matrix<-matrix_list[[q]][[rhindex]]
    }

    if(is.element('all',variables) | is.element('Performance',variables) | is.element('EWL',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmax_matrix<-matrix_list[[q]][[tmaxindex]]
    }

    if(is.element('all',variables) | is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){
      tmin_matrix<-matrix_list[[q]][[tminindex]]
    }

    if(is.element('all',variables) | is.element('Precipitation',variables)){

      prec_matrix<-matrix_list[[q]][[precindex]]

      if(Breeding==T) {

        if(StartBreeding > 0 && StopBreeding < 13) {


          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            print ( c(StartBreeding:StopBreeding) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c((StartBreeding+1):(StopBreeding-1)) )))

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            print ( c(StartBreeding:12, 1) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c(((StartBreeding+1):12)) )))

          } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
            print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
            print ( c(12, 1:StopBreeding) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c((1:(StopBreeding-1))) )))

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))

          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            print (c( StartBreeding:StopBreeding ) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]]

          } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) )))

          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            WinPrec_matrix <- StartBreeding_frac*prec_matrix[[StartBreeding]] + StopBreeding_frac*prec_matrix[[StopBreeding]] + as.matrix(Reduce("+",list.subset(prec_matrix, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  )))

          } else {stop (print("Breeding months are NOT OK"))}
        }
        ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))


        # get sum matrices

        year_prec_matrix_mat <-Reduce('+',prec_matrix) # all year rain

        WinPrec_matrix[ WinPrec_matrix < 0] <- 0
        SumPrec_matrix <- year_prec_matrix_mat - WinPrec_matrix
        SumPrec_matrix[SumPrec_matrix < 0] <- 0

        ## Define rasters

        prec_year <-stack(raster(year_prec_matrix_mat,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(prec_year) <- 'total_prec'

        prec_breeding <-stack(raster(WinPrec_matrix,
                                     xmn = ext_species[1],
                                     xmx = ext_species[2],
                                     ymn = ext_species[3],
                                     ymx = ext_species[4],
                                     crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(prec_breeding) <- 'Prec_during_Breeding'

        prec_non_breeding <-stack(raster(SumPrec_matrix,
                                         xmn = ext_species[1],
                                         xmx = ext_species[2],
                                         ymn = ext_species[3],
                                         ymx = ext_species[4],
                                         crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(prec_non_breeding) <- 'Prec_during_NON_Breeding'

        varlist[[length(varlist)+1]]<-prec_year
        varnames[[length(varnames)+1]]<-'total_prec'

        varlist[[length(varlist)+1]]<-prec_breeding
        varnames[[length(varnames)+1]]<-'Prec_during_Breeding'

        varlist[[length(varlist)+1]]<-prec_non_breeding
        varnames[[length(varnames)+1]]<-'Prec_during_NON_Breeding'

      } else {

        year_prec_matrix_mat <-Reduce('+',prec_matrix) # all year rain

        prec_year <-stack(raster(year_prec_matrix_mat,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(prec_year) <- 'total_prec'

        varlist[[length(varlist)+1]]<-prec_year
        varnames[[length(varnames)+1]]<-'total_prec'

      }

    }

    if(is.element('EWL',variables) | EWLPerf==T){

      Asym=EWLCoef$ewl_Asym
      K1=EWLCoef$ewl_K1
      M=EWLCoef$ewl_M
      Infl1=EWLCoef$ewl_Infl1
      K2=EWLCoef$ewl_K2
      Infl2=EWLCoef$ewl_Infl2
      Tslope=EWLCoef$T_slope
      Tintercept=EWLCoef$T_intercept
      Kdry=EWLCoef$Kdry
      Kshd=EWLCoef$Kshd

      ####RH_mat_list

      TdeltaFUN<-function(temp_value){

        TD<-temp_value-(Tslope*temp_value+Tintercept)

        return(TD)

      }

      Tdelta_mat_list<-list()
      for(m in 1:12){
        Tdelta_mat<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))
        for(r in 1:nrow(tmax_matrix[[m]])){
          for(c in 1:ncol(tmax_matrix[[m]])){
            Tdelta_mat[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(x),TdeltaFUN(temp_value = x), NA))

          }
        }
        Tdelta_mat_list[[m]] <- matrix(Tdelta_mat,nrow = nrow(tmax_matrix[[1]]),ncol = ncol(tmax_matrix[[1]]))
      }

      EWLFUN<-function(Tdelta,RH,dry,shade){

        EWL<-Asym/(1+M*exp(-K1*(Tdelta-Infl1)-K2*(RH-Infl2)+Kdry*dry+Kshd*shade)^(1/M))

        return(EWL)

      }

      if(EWLdry==T){
        dry_value=1
      } else {
        dry_value=0
      }

      if(EWLshade==T){
        shade_value=1
      } else {
        shade_value=0
      }


      EWL_mat_list<-list()
      for(m in 1:12){
        EWL_mat<-matrix(data = NA, nrow = nrow(Tdelta_mat_list[[m]]), ncol = ncol(Tdelta_mat_list[[m]]))
        for(r in 1:nrow(Tdelta_mat_list[[m]])){
          for(c in 1:ncol(Tdelta_mat_list[[m]])){
            EWL_mat[r,c] <- sapply (Tdelta_mat_list[[m]][r,c]/10,function(x) ifelse(!is.na(x),EWLFUN(Tdelta = x, RH=rh_matrix[[m]][r,c], dry=dry_value, shade=shade_value), NA))

          }
        }
        EWL_mat_list[[m]] <- matrix(EWL_mat,nrow = nrow(Tdelta_mat_list[[1]]),ncol = ncol(Tdelta_mat_list[[1]]))

      }


      if(Breeding==T){

        if(StartBreeding > 0 && StopBreeding < 13) {

          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            print ( c(StartBreeding:StopBreeding) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            print ( c(StartBreeding:12, 1) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c(((StartBreeding+1):12)) )))
            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
            print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
            print ( c(12, 1:StopBreeding) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c((1:(StopBreeding-1))) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            print (c( StartBreeding:StopBreeding ) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]]

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            EWL_breeding_mat <- StartBreeding_frac*EWL_mat_list[[StartBreeding]] + StopBreeding_frac*EWL_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(EWL_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2


          } else {stop (print("Breeding months are NOT OK"))}
        }
        ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

        # get sum matrices

        year_EWL_mat <-Reduce('+',EWL_mat_list) # all year rain

        EWL_breeding_mat[ EWL_breeding_mat < 0] <- 0
        EWL_non_breeding_mat <- year_EWL_mat - EWL_breeding_mat
        EWL_non_breeding_mat[EWL_non_breeding_mat < 0] <- 0

        ## Define rasters

        EWL_year <-stack(raster(year_EWL_mat/12,
                                xmn = ext_species[1],
                                xmx = ext_species[2],
                                ymn = ext_species[3],
                                ymx = ext_species[4],
                                crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(EWL_year) <- 'total_EWL'

        EWL_breeding <-stack(raster(EWL_breeding_mat/nmonths,
                                    xmn = ext_species[1],
                                    xmx = ext_species[2],
                                    ymn = ext_species[3],
                                    ymx = ext_species[4],
                                    crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(EWL_breeding) <- 'EWL_during_Breeding'

        EWL_non_breeding <-stack(raster(EWL_non_breeding_mat/(12-nmonths),
                                        xmn = ext_species[1],
                                        xmx = ext_species[2],
                                        ymn = ext_species[3],
                                        ymx = ext_species[4],
                                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(EWL_non_breeding) <- 'EWL_during_NON_Breeding'

        varlist[[length(varlist)+1]]<-EWL_year
        varnames[[length(varnames)+1]]<-'total_EWL'

        varlist[[length(varlist)+1]]<-EWL_breeding
        varnames[[length(varnames)+1]]<-'EWL_during_Breeding'

        varlist[[length(varlist)+1]]<-EWL_non_breeding
        varnames[[length(varnames)+1]]<-'EWL_during_NON_Breeding'

      } else {

        year_EWL_mat <-Reduce('+',EWL_mat_list) # all year rain

        EWL_year <-stack(raster(year_EWL_mat/12,
                                xmn = ext_species[1],
                                xmx = ext_species[2],
                                ymn = ext_species[3],
                                ymx = ext_species[4],
                                crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(EWL_year) <- 'total_EWL'

        varlist[[length(varlist)+1]]<-EWL_year
        varnames[[length(varnames)+1]]<-'total_EWL'

      }

    }

    ############# Performance ###########

    if(is.element('all',variables) | is.element('Performance',variables)){

      if(EWLPerf==T){

        if(EWLdry==T){
          dry_value=1
        } else {
          dry_value=0
        }

        if(EWLshade==T){
          shade_value=1
        } else {
          shade_value=0
        }

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
        }


      } else {

        if(is.na(acclag)){

          perf_mat_list<-list()
          for(m in 1:12){
            Perf_mat<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))
            for(r in 1:nrow(tmax_matrix[[m]])){
              for(c in 1:ncol(tmax_matrix[[m]])){
                Perf_mat[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(x),PerfFUN(temp_value = x, size_value=size), NA))

              }
            }
            perf_mat_list[[m]] <- matrix(Perf_mat,nrow = nrow(tmax_matrix[[1]]),ncol = ncol(tmax_matrix[[1]]))
          }

        } else {

          perf_mat_list<-list()


          for(m in 1:12){

            Perf_mat<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))
            Perf_mat1<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))
            Perf_mat2<-matrix(data = NA, nrow = nrow(tmax_matrix[[m]]), ncol = ncol(tmax_matrix[[m]]))

            for(r in 1:nrow(tmax_matrix[[m]])){
              for(c in 1:ncol(tmax_matrix[[m]])){

                Perf_mat[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value = tmax_matrix[[m]][r,c]/10), NA))

                if (m>2){

                  Perf_mat1[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[m-1]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value=tmax_matrix[[m-1]][r,c]/10), NA))

                  Perf_mat2[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[m-2]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value=tmax_matrix[[m-2]][r,c]/10), NA))

                }

                if(m==2){

                  Perf_mat1[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[m-1]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value=tmax_matrix[[m-1]][r,c]/10), NA))

                  Perf_mat2[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[12]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value=tmax_matrix[[12]][r,c]/10), NA))

                }

                if(m==1){

                  Perf_mat1[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[12]][r,c]/10),PerfFUN(temp_value  = x, size_value=size , acctemp_value=tmax_matrix[[12]][r,c]/10), NA))

                  Perf_mat2[r,c] <- sapply (tmax_matrix[[m]][r,c]/10,function(x) ifelse(!is.na(tmax_matrix[[m]][r,c]/10) & !is.na(tmax_matrix[[11]][r,c]/10),PerfFUN(temp_value = x, size_value=size , acctemp_value=tmax_matrix[[11]][r,c]/10), NA))

                }
              }
            }

            if(acclag <= 4){
              perf_mat_list[[m]]<- ((4-acclag)/4)*Perf_mat + (acclag/4)*Perf_mat1
            }
            if(acclag > 4){
              perf_mat_list[[m]]<- ((8-acclag)/4)*Perf_mat1 + ((acclag-4)/4)*Perf_mat2
            }
            if(acclag > 8){
              stop("I'm sorry, Dave. I'm afraid I can't do that.")
            }
          }

        }

      }

      if(Breeding==T){

        if(StartBreeding > 0 && StopBreeding < 13) {

          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            print ( c(StartBreeding:StopBreeding) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))
            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            print ( c(StartBreeding:12, 1) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c(((StartBreeding+1):12)) )))
            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
            print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
            print ( c(12, 1:StopBreeding) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c((1:(StopBreeding-1))) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            print (c( StartBreeding:StopBreeding ) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]]

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2

          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
            perf_breeding_mat <- StartBreeding_frac*perf_mat_list[[StartBreeding]] + StopBreeding_frac*perf_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(perf_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  )))

            nmonths<- StartBreeding_frac + StopBreeding_frac + length(c(StartBreeding:12,1:StopBreeding)) -2


          } else {stop (print("Breeding months are NOT OK"))}
        }
        ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

        # get sum matrices

        year_perf_mat <-Reduce('+',perf_mat_list) # all year rain

        perf_breeding_mat[ perf_breeding_mat < 0] <- 0
        perf_non_breeding_mat <- year_perf_mat - perf_breeding_mat
        perf_non_breeding_mat[perf_non_breeding_mat < 0] <- 0

        ## Define rasters

        perf_year <-stack(raster(year_perf_mat/12,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(perf_year) <- 'total_perf'

        perf_breeding <-stack(raster(perf_breeding_mat/nmonths,
                                     xmn = ext_species[1],
                                     xmx = ext_species[2],
                                     ymn = ext_species[3],
                                     ymx = ext_species[4],
                                     crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(perf_breeding) <- 'Perf_during_Breeding'

        perf_non_breeding <-stack(raster(perf_non_breeding_mat/(12-nmonths),
                                         xmn = ext_species[1],
                                         xmx = ext_species[2],
                                         ymn = ext_species[3],
                                         ymx = ext_species[4],
                                         crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(perf_non_breeding) <- 'Perf_during_NON_Breeding'

        varlist[[length(varlist)+1]]<-perf_year
        varnames[[length(varnames)+1]]<-'total_perf'

        varlist[[length(varlist)+1]]<-perf_breeding
        varnames[[length(varnames)+1]]<-'Perf_during_Breeding'

        varlist[[length(varlist)+1]]<-perf_non_breeding
        varnames[[length(varnames)+1]]<-'Perf_during_NON_Breeding'

      } else {

        year_perf_mat <-Reduce('+',perf_mat_list) # all year rain

        perf_year <-stack(raster(year_perf_mat/12,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(perf_year) <- 'total_perf'

        varlist[[length(varlist)+1]]<-perf_year
        varnames[[length(varnames)+1]]<-'total_perf'

      }

    }

    ##########################################################################################
    #######                 PET (Potential Evapotranspiration)              ##################
    ##########################################################################################

    if(is.element('all',variables) | is.element('ET',variables)){

      require("EcoHydRology")

      prec_stack <- stack(all_crop[[q]][[precindex]])
      tmax_stack <- stack(all_crop[[q]][[tmaxindex]])
      tmin_stack <- stack(all_crop[[q]][[tminindex]])

      #### from get slope and aspect

      species_slope_aspect <- terrain(alt_stack, opt=c('slope', 'aspect'), unit='radians', neighbors=8)

      #### from degrees of latitude of each cell to radians

      lat_rad <- coordinates(tmax_stack)[, 2] * pi/180

      # Calculating monthly PET
      # The following code takes the function in EcoHydRology and applies
      # it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision) .

      for (l in 1:12) {
        evap <- raster(tmax_stack, 1)
        slope <- values(subset(species_slope_aspect, 1))
        aspect <- values(subset(species_slope_aspect, 2))
        Tmax <- values(subset(tmax_stack, l))/10
        Tmin <- values(subset(tmin_stack, l))/10
        d <- data.frame(day = (30 * l) - 15, Tmin, Tmax, slope, aspect, lat_rad) # day at the midpoint of each month
        d[is.na(d)] <- 0
        Es_PET <- PET_fromTemp(Jday = d$day, Tmax_C = d$Tmax, Tmin_C = d$Tmin, lat_radians = d$lat_rad, aspect = d$aspect, slope = d$slope) * 1000
        values(evap) <- Es_PET
        if (l == 1) {
          PET <<- brick(evap)
        }
        if (l > 1) {
          PET <<- addLayer(PET, evap)
        }
      }

      # name PET layers

      PET_months <- c("PET_Jan", "PET_Feb", "PET_Mar", "PET_Apr", "PET_May", "PET_Jun", "PET_Jul", "PET_Aug", "PET_Sep", "PET_Oct", "PET_Nov", "PET_Dec")
      PET_10 <- PET * 10
      names(PET_10) <- PET_months

      ##########################################################################################
      ########                 AET (Actual evapotranspiration)             #####################
      ##########################################################################################

      # Estimating AET using a simple bucket model # Duncan Golicher code:
      # AET is Actual evapotranspiration and always lower than PET potential evapotranspiration
      # and can be much lower when the soil profile is well below field capacity.

      Bucket <- raster(PET, 1)

      for (n in 1:2) {
        for (p in 1:359) {
          mn <- 1 + p%/%30                                # %/% indicates integer division
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
          if (n > 1 && (p%%30) - 15 == 0) {     ## p%%30 will run 1 to 359 and determine position in 30 e.g., 1 is 1 and 61 is 1
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

      AET_months <- c("AET_Jan", "AET_Feb", "AET_Mar", "AET_Apr", "AET_May", "AET_Jun", "AET_Jul", "AET_Aug", "AET_Sep", "AET_Oct", "AET_Nov", "AET_Dec")
      AET_100 <- AET * 100
      names(AET_100) <- AET_months

      # add total and mean AET

      if(Breeding==T){

        if(StartBreeding > 0 && StopBreeding < 13) {

          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding) + calc(subset(PET_10, c( (StartBreeding+1)[(StartBreeding+1)!=0] : (StopBreeding-1)[(StopBreeding-1)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding) + calc(subset(PET_10, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0])), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding) + calc(subset(PET_10, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding) + calc(subset(PET_10, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            PET_breeding <- StartBreeding_frac*subset(PET_10, StartBreeding) + StopBreeding_frac*subset(PET_10, StopBreeding) + calc(subset(PET_10, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else {stop ("Breeding months are NOT OK")}

          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding) + calc(subset(AET_100, c( (StartBreeding+1)[(StartBreeding+1)!=0] : (StopBreeding-1)[(StopBreeding-1)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding) + calc(subset(AET_100, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0])), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding) + calc(subset(AET_100, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding) + calc(subset(AET_100, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            AET_breeding <- StartBreeding_frac*subset(AET_100, StartBreeding) + StopBreeding_frac*subset(AET_100, StopBreeding) + calc(subset(AET_100, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else {stop ("Breeding months are NOT OK")}
        }
        ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

        PET_year_x<-calc(PET_10, fun=sum)
        AET_year_x<-calc(AET_100, fun=sum)

        PET_non_breeding <- PET_year_x - PET_breeding
        AET_non_breeding <- AET_year_x - AET_breeding

        PET_breeding <-stack(PET_breeding ,
                             xmn = ext_species[1],
                             xmx = ext_species[2],
                             ymn = ext_species[3],
                             ymx = ext_species[4],
                             crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(PET_breeding) <- "PET_breeding"

        PET_non_breeding <-stack(PET_non_breeding ,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(PET_non_breeding) <- "PET_non_breeding"

        AET_breeding <-stack(AET_breeding/10 ,
                             xmn = ext_species[1],
                             xmx = ext_species[2],
                             ymn = ext_species[3],
                             ymx = ext_species[4],
                             crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(AET_breeding) <- "AET_breeding"

        AET_non_breeding <-stack(AET_non_breeding/10 ,
                                 xmn = ext_species[1],
                                 xmx = ext_species[2],
                                 ymn = ext_species[3],
                                 ymx = ext_species[4],
                                 crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(AET_non_breeding) <- "PET_non_breeding"


        varlist[[length(varlist)+1]]<-PET_breeding
        varnames[[length(varnames)+1]]<-'PET_breeding'

        varlist[[length(varlist)+1]]<-PET_non_breeding
        varnames[[length(varnames)+1]]<-'PET_non_breeding'

        varlist[[length(varlist)+1]]<-AET_breeding
        varnames[[length(varnames)+1]]<-'AET_breeding'

        varlist[[length(varlist)+1]]<-AET_non_breeding
        varnames[[length(varnames)+1]]<-'AET_non_breeding'

      }


      PET_year <-stack(calc(PET_10, fun=sum) ,
                       xmn = ext_species[1],
                       xmx = ext_species[2],
                       ymn = ext_species[3],
                       ymx = ext_species[4],
                       crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
      names(PET_year) <- "total_PET_year"

      varlist[[length(varlist)+1]]<-PET_year
      varnames[[length(varnames)+1]]<-'total_PET_year'


      AET_year <-stack(calc(AET_100, fun=sum)/10 ,
                       xmn = ext_species[1],
                       xmx = ext_species[2],
                       ymn = ext_species[3],
                       ymx = ext_species[4],
                       crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
      names(AET_year) <- "total_AET_year"

      varlist[[length(varlist)+1]]<-AET_year
      varnames[[length(varnames)+1]]<-'total_AET_year'

    }

    ##########################################################################################
    ###########################         Solar Radiation          #############################
    ##########################################################################################

    if(is.element('all',variables) | is.element('Radiation',variables)){

      require(EcoHydRology)

      tmax_stack <- stack(all_crop[[q]][[tmaxindex]])
      tmin_stack <- stack(all_crop[[q]][[tminindex]])

      #### from get slope and aspect

      species_slope_aspect <- terrain(alt_stack, opt=c('slope', 'aspect'), unit='radians', neighbors=8)

      #### from degrees of latitude of each cell to radians

      lat_rad <- coordinates(tmax_stack)[, 2] * pi/180

      # Calculating monthly Solar Radiation
      # The following code takes the function in EcoHydRology and applies
      # it to a day at the midpoint of each month (assuming 30 day months for simplicity as there is no need for false precision) .

      for (i in 1:12) {
        solar_r <- raster(tmax_stack, 1)
        slope <- values(subset(species_slope_aspect, 1))
        aspect <- values(subset(species_slope_aspect, 2))
        Tmax <- values(subset(tmax_stack, i))/10
        Tmin <- values(subset(tmin_stack, i))/10
        d <- data.frame(day = (30 * i) - 15, Tmin, Tmax, lat_rad, slope, aspect) # day at the midpoint of each month
        d[is.na(d)] <- 0
        Es_Solar <- Solar(lat = d$lat_rad, Jday = d$day, Tx = d$Tmax, Tn = d$Tmin, albedo=0.2, forest=0, aspect = d$aspect, slope = d$slope, printWarn=F) / 10
        values(solar_r) <- Es_Solar
        if (i == 1) {
          solar_out <<- brick(solar_r)
        }
        if (i > 1) {
          solar_out <<- addLayer(solar_out, solar_r)
        }
      }

      # name solar layers

      solar_months <- c("solar_Jan", "solar_Feb", "solar_Mar", "solar_Apr", "solar_May", "solar_Jun", "solar_Jul", "solar_Aug", "solar_Sep", "solar_Oct", "solar_Nov", "solar_Dec")
      names(solar_out) <- solar_months

      # add total solar

      solar_year <- calc(solar_out, fun=sum)

      solar_year_by_10 <-stack(solar_year/10,
                               xmn = ext_species[1],
                               xmx = ext_species[2],
                               ymn = ext_species[3],
                               ymx = ext_species[4],
                               crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
      names(solar_year_by_10) <- "total_solar_year"

      varlist[[length(varlist)+1]]<-solar_year_by_10
      varnames[[length(varnames)+1]]<-"total_solar_year"

      if(Breeding==T){

        if(StartBreeding > 0 && StopBreeding < 13) {
          if (StartBreeding-StopBreeding < -1) {
            print ("StartBreeding-StopBreeding < -1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding) + calc(subset(solar_out, c( (StartBreeding+1)[(StartBreeding+1)!=0] : (StopBreeding-1)[(StopBreeding-1)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding) + calc(subset(solar_out, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0])), fun=sum)
          } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding) + calc(subset(solar_out, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == -1) {
            print ("StartBreeding-StopBreeding == -1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding == 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding) + calc(subset(solar_out, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] )), fun=sum)
          } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
            print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
            solar_out_breeding <- StartBreeding_frac*subset(solar_out, StartBreeding) + StopBreeding_frac*subset(solar_out, StopBreeding) + calc(subset(solar_out, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
          } else {stop ("Breeding months are NOT OK")}
        }
        ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

        solar_out_non_breeding <- solar_year - solar_out_breeding

        solar_out_breeding <-stack(solar_out_breeding,
                                   xmn = ext_species[1],
                                   xmx = ext_species[2],
                                   ymn = ext_species[3],
                                   ymx = ext_species[4],
                                   crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(solar_out_breeding) <- 'solar_radiation_breeding'

        ssolar_out_non_breeding <-stack(solar_out_non_breeding,
                                        xmn = ext_species[1],
                                        xmx = ext_species[2],
                                        ymn = ext_species[3],
                                        ymx = ext_species[4],
                                        crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
        names(solar_year_by_10) <- 'solar_radiation_non_breeding'

        varlist[[length(varlist)+1]]<-solar_out_breeding
        varnames[[length(varnames)+1]]<-'solar_radiation_breeding'

        varlist[[length(varlist)+1]]<-solar_out_non_breeding
        varnames[[length(varnames)+1]]<-'solar_radiation_non_breeding'

      }

    }

    if(is.element('all',variables) | is.element('ha',variables) | is.element('hr',variables)){

      if(is.element('all',variables) & hahrmethod=='Richards'| is.element('ha',variables) & hahrmethod=='Richards' | is.element('hr',variables) & hahrmethod=='Richards'){

        tmax_stack <- stack(all_crop[[q]][[tmaxindex]])/10

        #### HERE is my modification of the EcoPhysRaster for ha

        if(is.element('all',variables) | is.element('hr',variables)){
          hr_Asym<-RichCoef$hr_Asym
          hr_K<-RichCoef$hr_K
          hr_M<-RichCoef$hr_M
          hr_Infl<-RichCoef$hr_Infl
          Tupr<-RichCoef$Tupr

          month_hr_list <- list()

          for (j in 1:12) {
            # This line has the old equation I used in fitting h_r functions

            month_hr_list [[j]] <- hr_Asym / ((1 + hr_K * exp(( -hr_Infl * (tmax_stack[[j]] - Tupr))))^(1/hr_M))

          }

          ## hr rasters montly and year

          h_r_montly_raster <- stack(month_hr_list)
          h_r_year_x <- Reduce('+',month_hr_list)


          h_r_year <-stack(h_r_year_x/12,
                           xmn = ext_species[1],
                           xmx = ext_species[2],
                           ymn = ext_species[3],
                           ymx = ext_species[4],
                           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
          names(h_r_year) <- 'h_r_total'

          varlist[[length(varlist)+1]]<-h_r_year
          varnames[[length(varnames)+1]]<-'total_hr'

        }


        #### HERE is my modification of the EcoPhysRaster for ha

        if(is.element('all',variables) | is.element('ha',variables)){
          ha_Asym<-RichCoef$ha_Asym
          ha_K<-RichCoef$ha_K
          ha_M<-RichCoef$ha_M
          ha_Infl<-RichCoef$ha_Infl
          Tupr<-RichCoef$Tupr

          month_ha_list <- list()

          for (j in 1:12) {
            # This line has the old equation I used in fitting h_a functions
            month_ha_list [[j]] <- ha_Asym / ((1 + ha_K * exp(( -ha_Infl * (tmax_stack[[j]] - Tupr))))^(1/ha_M))

          }

          ## ha rasters montly and year

          h_a_montly_raster <- stack(month_ha_list)
          h_a_year_x <- Reduce('+',month_ha_list)

          h_a_year <-stack(h_a_year_x/12,
                           xmn = ext_species[1],
                           xmx = ext_species[2],
                           ymn = ext_species[3],
                           ymx = ext_species[4],
                           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
          names(h_a_year) <- 'h_a_total'

          varlist[[length(varlist)+1]]<-h_a_year
          varnames[[length(varnames)+1]]<-'total_ha'

        }

        if(Breeding==T){
          ##########################################################################################
          ###########################         Breeding fractions          ##########################
          ##########################################################################################

          if(StartBreeding < StopBreeding) {

            nmonths<-StopBreeding-StartBreeding

          } else {

            nmonths<-13-StartBreeding+StopBreeding

          }

          if(StartBreeding > 0 && StopBreeding < 13) {

            ####

            if(is.element('all',variables) | is.element('ha',variables)){

              if (StartBreeding-StopBreeding < -1) {
                print ("StartBreeding-StopBreeding < -1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( (StartBreeding+1)[(StartBreeding+1)!=0] : (StopBreeding-1)[(StopBreeding-1)!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0])), fun=sum)
              } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
                print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
                print ( c(12, 1:StopBreeding) )
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( (1:(StopBreeding-1))[((StartBreeding+1):12)!=0])), fun=sum)

              } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding == -1) {
                print ("StartBreeding-StopBreeding == -1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding)
              } else if (StartBreeding-StopBreeding == 1  && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                ha_breeding <- StartBreeding_frac*subset(h_a_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_a_montly_raster, StopBreeding) + calc(subset(h_a_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
              } else {stop ("Breeding months are NOT OK")}

              ha_non_breeding <- h_a_year_x - ha_breeding

              ha_breeding <-stack(ha_breeding/nmonths,
                                  xmn = ext_species[1],
                                  xmx = ext_species[2],
                                  ymn = ext_species[3],
                                  ymx = ext_species[4],
                                  crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
              names(ha_breeding) <- 'h_a_during_Breeding'

              ha_non_breeding <-stack(ha_non_breeding/(12-nmonths),
                                      xmn = ext_species[1],
                                      xmx = ext_species[2],
                                      ymn = ext_species[3],
                                      ymx = ext_species[4],
                                      crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
              names(ha_non_breeding) <- 'h_a_during_NON_Breeding'

              varlist[[length(varlist)+1]]<-ha_breeding
              varnames[[length(varnames)+1]]<-'ha_breeding'

              varlist[[length(varlist)+1]]<-ha_non_breeding
              varnames[[length(varnames)+1]]<-'ha_non_breeding'

            }

            if(is.element('all',variables) | is.element('hr',variables)){

              if (StartBreeding-StopBreeding < -1) {
                print ("StartBreeding-StopBreeding < -1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( (StartBreeding+1)[(StartBreeding+1)!=0] : (StopBreeding-1)[(StopBreeding-1)!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0])), fun=sum)
              } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
                print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
                print ( c(12, 1:StopBreeding) )
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( (1:(StopBreeding-1))[((StartBreeding+1):12)!=0])), fun=sum)
              }else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding == -1) {
                print ("StartBreeding-StopBreeding == -1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding)
              } else if (StartBreeding-StopBreeding == 1  && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] )), fun=sum)
              } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                hr_breeding <- StartBreeding_frac*subset(h_r_montly_raster, StartBreeding) + StopBreeding_frac*subset(h_r_montly_raster, StopBreeding) + calc(subset(h_r_montly_raster, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0] )), fun=sum)
              } else {stop ("Breeding months are NOT OK")}

              hr_non_breeding <- h_r_year_x - hr_breeding

              hr_breeding <-stack(hr_breeding/nmonths,
                                  xmn = ext_species[1],
                                  xmx = ext_species[2],
                                  ymn = ext_species[3],
                                  ymx = ext_species[4],
                                  crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
              names(hr_breeding) <- 'h_r_during_Breeding'

              hr_non_breeding <-stack(hr_non_breeding/(12-nmonths),
                                      xmn = ext_species[1],
                                      xmx = ext_species[2],
                                      ymn = ext_species[3],
                                      ymx = ext_species[4],
                                      crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
              names(hr_non_breeding) <- 'h_r_during_NON_Breeding'

              varlist[[length(varlist)+1]]<-hr_breeding
              varnames[[length(varnames)+1]]<-'hr_breeding'

              varlist[[length(varlist)+1]]<-hr_non_breeding
              varnames[[length(varnames)+1]]<-'hr_non_breeding'

            }

          }
          ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

        }

      }

      if(is.element('all',variables) & hahrmethod=='Senoid'| is.element('ha',variables) & hahrmethod=='Senoid' | is.element('hr',variables) & hahrmethod=='Senoid'){

        ForestOffset<-0 #Placeholder, to be implemented

        dimensions <-c(nrow(tmax_matrix[[1]]), ncol(tmax_matrix[[1]]))
        dimensions[1] #nrows
        dimensions[2] #ncols

        mat_sumTair_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1]))# 12 matrices 1 per month

        if(is.element('all',variables) | is.element('hr',variables)){

          hrcap_value<-hcap

          mat_sumUCT_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1])) # calculates the # time units that tair was > than UCT

          h_r_Barry_mat_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1]))


          h_r_year_mat <- matrix(0, ncol=dimensions[2], nrow=dimensions[1])

          for (x in 1:12) { # make numeric NA matrices
            mode(mat_sumTair_list[[x]]) <- "double"
            mode(mat_sumUCT_list[[x]]) <- "double"

            mode(h_r_Barry_mat_list[[x]]) <- "double"

            mode(h_r_year_mat[[x]]) <- "double"

          }

        }

        if(is.element('all',variables) | is.element('ha',variables)){

          hacap_value<-hcap

          mat_sumLCT_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1])) # calculates the # time units that tair was < than LCT

          h_a_Barry_mat_list <- lapply(1:12, function(x) matrix(0, ncol=dimensions[2], nrow=dimensions[1]))


          h_a_year_mat <- matrix(0, ncol=dimensions[2], nrow=dimensions[1])

          for (x in 1:12) { # make numeric NA matrices
            mode(mat_sumTair_list[[x]]) <- "double"
            mode(mat_sumLCT_list[[x]]) <- "double"

            mode(h_a_Barry_mat_list[[x]]) <- "double"

            mode(h_a_year_mat[[x]]) <- "double"

          }

        }

        t_air<-list()

        for(m in 1:12) { # open m loop Number -- 1
          for(h in 1:24){ # open h
            for(s in 1:hr_res){ # open i

              t_air[[m]] <- ((ForestOffset + tmax_matrix[[m]])-(ForestOffset + tmin_matrix[[m]])) /
                2*sin(pi/12*(h+s/hr_res)-3*pi/4) +
                ((ForestOffset + tmax_matrix[[m]])+(ForestOffset + tmin_matrix[[m]])) /
                2

              mat_sumTair_list[[m]] <- mat_sumTair_list[[m]] + t_air[[m]]

              if(is.element('all',variables) | is.element('hr',variables)){
                mat_sumUCT_calculator <-  rapply(t_air, function(x) ifelse(x>Tupr*10, 1/hr_res,0), how="replace")
                mat_sumUCT_list[[m]] <- mat_sumUCT_list[[m]] + mat_sumUCT_calculator[[m]]
              }

              if(is.element('all',variables) | is.element('ha',variables)){
                mat_sumLCT_calculator <-  rapply(t_air, function(x) ifelse(x<Tlwr*10, 1/hr_res,0), how="replace")
                mat_sumLCT_list[[m]] <- mat_sumLCT_list[[m]] + mat_sumLCT_calculator[[m]]
              }
            } # close i
          } # close h
          ## sum h_a and h_r
          if(is.element('all',variables) | is.element('ha',variables)){
            hacap <- hacap_value
            ifelse (is.na(hacap_value) == T, h_a_Barry_mat_list[[m]] <- 24 -mat_sumLCT_list[[m]],
                    ifelse (mat_sumLCT_list[[m]]  < hacap, h_a_Barry_mat_list[[m]]  <- h_a_Barry_mat_list[[m]] + hacap,
                            h_a_Barry_mat_list[[m]]  <- h_a_Barry_mat_list[[m]] + 24-mat_sumLCT_list[[m]]))
          }

          if(is.element('all',variables) | is.element('hr',variables)){
            hrcap <- hrcap_value
            ifelse (is.na(hrcap_value) == T, h_r_Barry_mat_list[[m]] <- mat_sumUCT_list[[m]],
                    ifelse(mat_sumUCT_list[[m]]  > hrcap, h_r_Barry_mat_list[[m]]  <- h_r_Barry_mat_list[[m]] + hrcap,
                           h_r_Barry_mat_list[[m]]  <- h_r_Barry_mat_list[[m]] + mat_sumUCT_list[[m]]))
          }
        }

        if(Breeding==T){

          if(StartBreeding < StopBreeding) {

            nmonths<-StopBreeding-StartBreeding

          } else {

            nmonths<-12-StartBreeding+StopBreeding

          }


          if(is.element('all',variables) | is.element('hr',variables)){
            if(StartBreeding > 0 && StopBreeding < 13) {


              if (StartBreeding-StopBreeding < -1) {
                print ("StartBreeding-StopBreeding < -1")
                print ( c(StartBreeding:StopBreeding) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))

              } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                print ( c(StartBreeding:12, 1) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c(((StartBreeding+1):12)) )))

              } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
                print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
                print ( c(12, 1:StopBreeding) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c((1:(StopBreeding-1))) )))

              } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))

              } else if (StartBreeding-StopBreeding == -1) {
                print ("StartBreeding-StopBreeding == -1")
                print (c( StartBreeding:StopBreeding ) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]]

              } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) )))

              } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                h_r_breeding_mat <- StartBreeding_frac*h_r_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_r_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_r_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  )))

              } else {stop (print("Breeding months are NOT OK"))}
            }
            ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

            ######## Sum of total h_r

            h_r_year_mat <- Reduce('+',h_r_Barry_mat_list)


            h_r_breeding_mat[ h_r_breeding_mat < 0] <- 0
            h_r_non_breeding_mat <-h_r_year_mat - h_r_breeding_mat
            h_r_non_breeding_mat[h_r_non_breeding_mat < 0] <- 0

            ## Define rasters

            h_r_year <-stack(raster(h_r_year_mat/12,
                                    xmn = ext_species[1],
                                    xmx = ext_species[2],
                                    ymn = ext_species[3],
                                    ymx = ext_species[4],
                                    crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(h_r_year) <- 'total_h_r'

            hr_breeding <-stack(raster(h_r_breeding_mat/nmonths,
                                       xmn = ext_species[1],
                                       xmx = ext_species[2],
                                       ymn = ext_species[3],
                                       ymx = ext_species[4],
                                       crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(hr_breeding) <- 'h_r_during_Breeding'

            hr_non_breeding <-stack(raster(h_r_non_breeding_mat/(12-nmonths),
                                           xmn = ext_species[1],
                                           xmx = ext_species[2],
                                           ymn = ext_species[3],
                                           ymx = ext_species[4],
                                           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(hr_non_breeding) <- 'h_r_during_NON_Breeding'

            varlist[[length(varlist)+1]]<-h_r_year
            varnames[[length(varnames)+1]]<-'total_h_r'

            varlist[[length(varlist)+1]]<-hr_breeding
            varnames[[length(varnames)+1]]<-'hr_breeding'

            varlist[[length(varlist)+1]]<-hr_non_breeding
            varnames[[length(varnames)+1]]<-'hr_non_breeding'
          }


          if(is.element('all',variables) | is.element('ha',variables)){
            if(StartBreeding > 0 && StopBreeding < 13) {


              if (StartBreeding-StopBreeding < -1) {
                print ("StartBreeding-StopBreeding < -1")
                print ( c(StartBreeding:StopBreeding) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c((StartBreeding+1):(StopBreeding-1)) )))

              } else if (StartBreeding-StopBreeding > 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding == 1")
                print ( c(StartBreeding:12, 1) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c(((StartBreeding+1):12)) )))

              } else if (StartBreeding-StopBreeding > 1 && StartBreeding == 12) {
                print ("StartBreeding-StopBreeding > 1 && StartBreeding == 12")
                print ( c(12, 1:StopBreeding) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c((1:(StopBreeding-1))) )))

              } else if (StartBreeding-StopBreeding > 1 && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding > 1 && StopBreeding > 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0]) )))

              } else if (StartBreeding-StopBreeding == -1) {
                print ("StartBreeding-StopBreeding == -1")
                print (c( StartBreeding:StopBreeding ) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]]

              } else if (StartBreeding-StopBreeding == 1 && StopBreeding == 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding == 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0], 1) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0]) )))

              } else if (StartBreeding-StopBreeding == 1  && StopBreeding > 1) {
                print ("StartBreeding-StopBreeding == 1  && StopBreeding > 1")
                print (c( (StartBreeding:12)[(StartBreeding:12)!=0] , (1:StopBreeding)[(1:StopBreeding)!=0]) )
                h_a_breeding_mat <- StartBreeding_frac*h_a_Barry_mat_list[[StartBreeding]] + StopBreeding_frac*h_a_Barry_mat_list[[StopBreeding]] + as.matrix(Reduce("+",list.subset(h_a_Barry_mat_list, c( ((StartBreeding+1):12)[((StartBreeding+1):12)!=0] , (1:(StopBreeding-1))[(1:(StopBreeding-1))!=0])  )))

              } else {stop (print("Breeding months are NOT OK"))}
            }
            ifelse (StartBreeding>12 | StopBreeding>12, print("Breeding months are NOT OK"),print("Breeding months are OK"))

            ######## Sum of total h_a

            h_a_year_mat <- Reduce('+',h_a_Barry_mat_list)

            h_a_breeding_mat[ h_a_breeding_mat < 0] <- 0
            h_a_non_breeding_mat <-h_a_year_mat - h_a_breeding_mat
            h_a_non_breeding_mat[h_a_non_breeding_mat < 0] <- 0

            ## Define rasters

            h_a_year <-stack(raster(h_a_year_mat/12,
                                    xmn = ext_species[1],
                                    xmx = ext_species[2],
                                    ymn = ext_species[3],
                                    ymx = ext_species[4],
                                    crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(h_a_year) <- 'total_h_a'

            ha_breeding <-stack(raster(h_a_breeding_mat/nmonths,
                                       xmn = ext_species[1],
                                       xmx = ext_species[2],
                                       ymn = ext_species[3],
                                       ymx = ext_species[4],
                                       crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(ha_breeding) <- 'h_a_during_Breeding'

            ha_non_breeding <-stack(raster(h_a_non_breeding_mat/(12-nmonths),
                                           xmn = ext_species[1],
                                           xmx = ext_species[2],
                                           ymn = ext_species[3],
                                           ymx = ext_species[4],
                                           crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(ha_non_breeding) <- 'h_a_during_NON_Breeding'


            varlist[[length(varlist)+1]]<-h_a_year
            varnames[[length(varnames)+1]]<-'total_h_a'

            varlist[[length(varlist)+1]]<-ha_breeding
            varnames[[length(varnames)+1]]<-'ha_breeding'

            varlist[[length(varlist)+1]]<-ha_non_breeding
            varnames[[length(varnames)+1]]<-'ha_non_breeding'
          }

        } else {

          if(is.element('all',variables) | is.element('hr',variables)){
            h_r_year_mat <- Reduce('+',h_r_Barry_mat_list)

            h_r_year <-stack(raster(h_r_year_mat/12,
                                    xmn = ext_species[1],
                                    xmx = ext_species[2],
                                    ymn = ext_species[3],
                                    ymx = ext_species[4],
                                    crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(h_r_year) <- 'total_h_r'

            varlist[[length(varlist)+1]]<-h_r_year
            varnames[[length(varnames)+1]]<-'total_h_r'
          }


          if(is.element('all',variables) | is.element('ha',variables)){
            h_a_year_mat <- Reduce('+',h_a_Barry_mat_list)

            h_a_year <-stack(raster(h_a_year_mat/12,
                                    xmn = ext_species[1],
                                    xmx = ext_species[2],
                                    ymn = ext_species[3],
                                    ymx = ext_species[4],
                                    crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
            names(h_a_year) <- 'total_h_a'

            varlist[[length(varlist)+1]]<-h_a_year
            varnames[[length(varnames)+1]]<-'total_h_a'

          }


        }
      }

    }

    EcoPhysRasters[[q]]<-stack(varlist)
    names(EcoPhysRasters[[q]])<-varnames

  }

  close(pb)

  names(EcoPhysRasters)<-scenario_names

  #option to write rasters

  if(write_rasters==T){
    for(u in 1:length(EcoPhysRasters)){
      for(o in 1:length(names(EcoPhysRasters[[u]]))){
        writeRaster(EcoPhysRasters[[u]][[o]] , filename=paste(names(EcoPhysRasters[[u]])[o],names(EcoPhysRasters)[u],sep='_'), datatype='INT2S', overwrite=TRUE)
      }
    }
  }

  plot(EcoPhysRasters[[1]])
  return(EcoPhysRasters)

}
