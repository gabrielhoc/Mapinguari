# HATmodel

HATmodel<-function(data,
				   summarize='mean',
				   thrs,
				   thrs2=NULL,
				   hcap=NULL,
				   diel='diurnal',
				   out_dir=NULL,
				   method='mle'){

require(geosphere)

master_directory <- getwd()
			   	
if(!is.null(out_dir)){
 
if(dir.exists(out_dir)==F){
dir.create(out_dir, showWarnings = F,recursive=T)
}

setwd(out_dir)

}
				   	
				   }

#############################################################################################################################
#   Process HOBO Data    ####################################################################################################
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

names_HOBOdf_raw <- names(HOBOdf_raw)
names_HOBOdf_raw <- gsub("Temp_", "temp_", names_HOBOdf_raw)
names_HOBOdf_raw <- gsub("T_air", "t_air", names_HOBOdf_raw)
names_HOBOdf_raw <- gsub("Tair", "t_air", names_HOBOdf_raw)
names_HOBOdf_raw <- gsub("tair", "t_air", names_HOBOdf_raw)

names(HOBOdf_raw) <- names_HOBOdf_raw

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

day_counter <- 0

cat("days..")

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

day_counter <- day_counter + 1

cat(".", day_counter,".")

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

######  if  dir.create to output 

if(is.null(path_output_dir) == FALSE) {
                              dir.create(path_output_dir, showWarnings = F)
                              setwd(path_output_dir)

write.table(ha_hr_FlexParamCurve_df, file = paste(name_species,"_", local_names, "_","sample_", size_sample, "_", t_air_var_selected, "_Richards_FlexParamCurve_coefficients.txt", sep = ""), sep="\t")

if (!"[1] \"try-error\"" %in% out_fitModel_nls_ha)     {
writeLines(summary_fitModel_nls_ha,con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_ha_Richards_FlexParamCurve_summary.txt", sep=""))
}

if (!"[1] \"try-error\"" %in% out_fitModel_nls_hr)     {
writeLines(summary_fitModel_nls_hr,con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_hr_Richards_FlexParamCurve_summary.txt", sep=""))
}

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

                                    }

# return selected layers

return(coeflist)

                   }      # end of function

##############                          END of function                       ############


#############################################################################################################################
#RichHOBO_bbmle_JCS  ########################################################################################################
#############################################################################################################################

setwd("~/Desktop/Luisa_data/Ameiva_ameiva")

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

# dir.create

if(is.null(path_output_dir) == FALSE) {
                              dir.create(path_output_dir, showWarnings = F)
                              setwd(path_output_dir)
                              writeLines(capture.output(summary),con=paste(name_species, "_", local_names, "_","sample_", size_sample, "_", t_air_var_selected,"_hr_Richards_bbmle_summary.txt", sep=""))
# return to master directory
                               setwd(master_directory)
                                    }

# return selected coefficients

return(coeflist)

                        }      # end of function

##############                          END of function                       ############


