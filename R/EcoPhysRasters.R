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

  # insert EcoRasters call necessary for each Physiological variable

  #EcoPhys

  ##########################################################################################
  ##########################      Create EcoPhysRasters     ################################
  ##########################################################################################

  EcoPhysRasters<-list()

  pb <- txtProgressBar(min = 0, max = length(all_crop), style = 3)

  for(q in 1:length(all_crop)){

    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, q)

    ############# Performance ###########

    if(is.element('all',variables) | is.element('Performance',variables)){

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
