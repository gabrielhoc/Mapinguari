###################################################################################################################
###################################################################################################################
###################################################################################################################
########################    Mapinguari0.7 - The Correct Richard Equation Awakens    ###############################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#Fazer:

# Novo PerfGAMM
# Script sdm


###################################################################################################################
#cleanpoints#######################################################################################################
###################################################################################################################

cleanpoints <- function(spcoord,
                        altpath,
                        km_merge = 2
                        )   {      

require('raster')
require('geosphere')
require('maptools')
require('dismo')
data(wrld_simpl)

# input from user

species_data_raw<-spcoord
alt_data_raster <- altpath
y <- (km_merge)/100 # 1 km ~ 0.01 degrees (also 0.05 degrees ~ 5 km)

###########    preprocessing split file of species distributions to vectors    ###########

# read alt raster

alt.data <- list.files(path = alt_data_raster, pattern="*.bil$",full.names=T, ignore.case=T)
alt_data <- raster(alt.data[1])

n_entries <-nrow(spcoord)
Z_2<-spcoord

# from list to vectors

Z_2_Lon_Lat <- subset(spcoord, select = c(Lon, Lat)) # sapply(Z_2_Lon_Lat,data.class) to get class

# get hr_res to for resolution to less than an hour 

Z_2_area_not_round <- (areaPolygon(Z_2_Lon_Lat))/1000000 #area in square Km round 
Z_2_area <- round(Z_2_area_not_round, 0) #area in square Km round to integer
Z_2_area_hr_res <- 10-(round(log10(ifelse (Z_2_area+1>10000000,10000000, Z_2_area+1)),0)+1) #this function assing an interger from 2 to 9 (for really small area 1 km2) based on the area in km2. However, it forces to 2 for extremely large distributions > 10 000 000 km2

# process to reduce redundant localities

coordinates(Z_2) <- ~Lon+Lat #set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
crs(Z_2) <- crs(wrld_simpl) #Get or set the coordinate reference system (CRS) of a Raster* object.
Z_2_r <- raster(Z_2)
res(Z_2_r) <- y # y = 0.05 then 5 km ~0.05 degrees resolution
Z_2_r_e <- extend(Z_2_r, extent(Z_2_r)+1)
Z_2_r_e_acsel <- gridSample(Z_2, Z_2_r_e, n=1)
Z_2_r_e_acsel_df <- data.frame(Z_2_r_e_acsel)

# exclude localities in the ocean

georef<- Z_2_r_e_acsel_df
georef_alt<- cbind(georef, alt = extract(alt_data, georef, method = "bilinear"))
Z_2_r_e_acsel_df_cord_alt <- georef_alt[complete.cases(georef_alt$alt),]
Z_2_r_e_acsel_df_alt <- Z_2_r_e_acsel_df_cord_alt[,-c(1,2)]
Z_2_r_e_acsel_df <- Z_2_r_e_acsel_df_cord_alt[,-3]

## for summary

n_entries_clean <- nrow(Z_2_r_e_acsel_df)

# get and write_summary

names_species_split_df_end <- c(n_entries,n_entries_clean)
names(names_species_split_df_end) <- c("n_entries_raw", "n_entries_clean")
names_species_split_df_end$km_resolution_merging <- y*100

# if plot is requested

plot(Z_2_r_e_acsel_df$Lon, Z_2_r_e_acsel_df$Lat, pch=1, cex=0.75)
plot(wrld_simpl, add=T, border='blue', lwd=0.7)

# return list

print(names_species_split_df_end)
return(Z_2_r_e_acsel_df)

}


#############################################################################################################################
#RichHOBO#####################################################################################################################
#############################################################################################################################
RichHOBO<-function(HOBOdf,
				   method,
				   Tlwr,
				   Tupr,
				   hcap=NULL,
				   diel=NULL,
				   variables=c('ha','hr')){

require('bbmle')
	# dependencies: stats4

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

####   split by day

HOBOdf_day <- split(HOBOdf, HOBOdf$day_month_year)

#### from list to data frames

HOBOdf_day_list <- lapply(seq_along(HOBOdf_day), function(x) as.data.frame(HOBOdf_day[[x]]))

########################           loop for each day                ######################


day_of_year <- strptime(HOBOdf$day_month_year, "%d_%m_%Y")$yday+1

richards_data_point_day_list <- list()

for ( i in 1:length(HOBOdf_day_list)) {

thisday <- HOBOdf_day_list[[i]]

if(!is.null(diel)){
## sunrise and sunset hour calculator

suncalc<-function(day_of_year, Lat, Lon){

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
timezone = -4*(abs(Lon)%%15)*sign(Lon)

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

sunrise_sunset_hour_station  <- suncalc (day_of_year =day_of_year, Lat=thisday$Lat[1], Lon=thisday$Lon[1])

sunrise_round <- round(sunrise_sunset_hour_station[[1]],0)
sunset_round <- round(sunrise_sunset_hour_station[[2]],0)

#######################          time brackets for diel behavior: diurnal or nocturnal

      if( diel == 'diurnal') {
                         thisdaysubset <- subset (thisday, hour >= sunrise_round & hour <= sunset_round)
                         thisday <- na.omit (thisdaysubset)

                                }
      if( diel == 'nocturnal') {
                         thisdaysubset  <- subset (thisday, hour <= sunrise_round | hour >= sunset_round)
                         thisday <- na.omit (thisdaysubset)

                                  }                                    
}

#calculating HOBO interval

total_secs_in_minutes <- (thisday$second[2] - thisday$second[1])/60
total_min_in_minutes_1 <- (thisday$minute[2] - thisday$minute[1])

if(total_min_in_minutes_1 < 0) {
             total_min_in_minutes <- (thisday$minute[3] - thisday$minute[2])
                               } else {
             total_min_in_minutes <- total_min_in_minutes_1 }


total_seconds_and_minutes_interval <- total_secs_in_minutes+total_min_in_minutes

if(total_seconds_and_minutes_interval == 0 ) {
                         frac_in_minutes_interval <- (thisday$hour[2] - thisday$hour[1]) * 60
                                             } else {
                                  frac_in_minutes_interval <- total_seconds_and_minutes_interval
                                             }

##########################            determine day of the year

date <- paste(unique(HOBOdf$month), "/", unique(HOBOdf$day), "/", unique(HOBOdf$year), sep ="")

if(is.element('hr',variables) ){
hr <- (sum(thisday$binaryhr)/60) *frac_in_minutes_interval 
}

if(is.element('ha',variables) ){
ha <- (sum(thisday$binaryha)/60) *frac_in_minutes_interval 
}

t_air<- max(thisday$t_air)

# mergin all

if(is.element('hr',variables) & is.element('ha',variables) ){
all_bind <- data.frame(date[i],ha,hr,t_air)
} else if(is.element('hr',variables) ){
all_bind <- data.frame(date[i],hr,t_air)
} else if(is.element('ha',variables) ){
all_bind <- data.frame(date[i],ha,t_air)
}

richards_data_point_day_list[[i]] <-all_bind

}

##################### end of loop for each day

# from list to data frame

hahrdf<-do.call(rbind.data.frame, richards_data_point_day_list)

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


Asym / ((1 + K * exp(( -Infl * (t_air - Tupr))))^(1/M))

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
title<-paste(HOBOdf$species[1], "_hr_Richards_curve", sep = "")

Tdelta<-hahrdf$t_air-Tupr

plot(hahrdf$hr~Tdelta, data=hahrdf, main=title,xlab='T_air-Tupr')

hrfunction<-function(t_air,Tupr){
	(hrcoef[1])/((1+hrcoef[2]*exp(-hrcoef[4]*(t_air-Tupr)))^(1/hrcoef[3]))
	}

range_t_air<-c(min(hahrdf$t_air):max(hahrdf$t_air))	

lines(y=hrfunction(range_t_air,Tupr), x=range_t_air-Tupr, col="red", lwd=2)                                                        
}

if(is.element('ha',success) ){
title<-paste(HOBOdf$species[1], "_ha_Richards_curve", sep = "")

Tdelta<-hahrdf$t_air-Tupr

plot(hahrdf$ha~Tdelta, data=hahrdf, main=title,xlab='T_air-Tupr')

hafunction<-function(t_air,Tupr){
	(hacoef[1])/((1+hacoef[2]*exp(-hacoef[4]*(t_air-Tupr)))^(1/hacoef[3]))	}

range_t_air<-c(min(hahrdf$t_air):max(hahrdf$t_air))	
	
lines(y=hafunction(range_t_air,Tupr), x=range_t_air-Tupr, col="red", lwd=2)
}
#

if(is.element('hr',success) & is.element('ha',success) ){
coeflist<-list(hr_Asym=hrcoef[1],hr_K=hrcoef[2],hr_M=hrcoef[3],hr_Infl=hrcoef[4],ha_Asym=hacoef[1],ha_K=hacoef[2],ha_M=hacoef[3],ha_Infl=hacoef[4],Tupr=hacoef[5])
} else if(is.element('hr',success) ){
coeflist<-list(hr_Asym=hrcoef[1],hr_K=hrcoef[2],hr_M=hrcoef[3],hr_Infl=hrcoef[4],Tupr=hrcoef[5])
} else if(is.element('ha',success) ){
coeflist<-list(ha_Asym=hacoef[1],ha_K=hacoef[2],ha_M=hacoef[3],ha_Infl=hacoef[4],Tupr=hacoef[5])
}

print(summary)
print(printatend)
return(coeflist)

}
}
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
                                     output_directory) {

require('ggplot2')
require('grid')
require('mgcv')


# input data 

list_input_performance_raw <- performance_data
dependent_variable_names <- dependent_variable
smooth_variable_names <- smooth_predictors
linear_predictors_names <- linear_predictors
quadratic_predictors_names <- quadratic_predictors

criterion <- criterion
knots_vector <- knots_vector

plot_variable_on_x <- plot_variable_on_x
plot_variable_on_y <- plot_variable_on_y
plot_variable_on_z <- plot_variable_on_z

output_directory <- output_directory

master_directory <- getwd()

# create and open output dir

dir.create(output_directory, showWarnings = F)
setwd(output_directory)

# get species names

species_name <- unique(list_input_performance_raw$species)

# function of not into

"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0 

# subset_groups: 

performance_reference_1 <- list_input_performance_raw[ , which(names(list_input_performance_raw) %in% c("id", dependent_variable_names,smooth_variable_names,
                                                                             linear_predictors_names,quadratic_predictors_names))]
performance_reference <- performance_reference_1[complete.cases(performance_reference_1),]

get_numbers_individuals <- unique(as.character(performance_reference$id))
get_numbers_PER <- nrow(performance_reference)
cat(paste("Total individuals included:", " ", length(get_numbers_individuals), "\n", sep=""))
cat(paste("Total observations:", " ", get_numbers_PER, "\n", sep=""))

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


cat("processing formula: ", user_string_formula, "\n")

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


pdf(paste(species_name,'species_best_fit_3D_gam.pdf')) # only plot objects can be made pdfs
plot(plot_dispersion_species)
plot(best_gamm_model_AIC$gam, pages =1,residuals=TRUE,pch=19, seWithMean = TRUE, shade=TRUE,shade.col="gray") # 
title(paste("Best GAMM model AIC  -- ", species_name))
vis.gam(best_gamm_model_AIC$gam,
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
title(paste("Best GAMM model AIC  -- ", species_name))

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

temp_test_no_order <- as.data.frame(c(seq(-10,50, by = 0.1), -10, 50)) 
temp_test <- as.data.frame(temp_test_no_order[order(temp_test_no_order[,1]), ])
names(temp_test) <-'temp'

# get means of selected predictors

performance_input_select <- subset(performance_input, select = names(performance_input) %!in% c("id","temp", "performance", "order"))
performance_input_means <- as.data.frame(t(colMeans(performance_input_select)), stringsAsFactors = F)
data_test <- merge(temp_test, performance_input_means, all.x = TRUE)

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



## write summary for best models

writeLines(capture.output( {
                          cat("------------      TPC GAMM on given k_final     ------------\n\n")
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
                          print (best_gamm_model_BIC)
                          cat("\n\n")
                          cat("------------      best_gamm_model_AIC -- adjusted R squared")
                          cat("\n\n")
                          print (summary(best_gamm_model_AIC$gam)$r.sq) #adjusted R squared
                          cat("\n")
                          cat("------------      best_gamm_model_AIC      ------------")
                          cat("\n")
                          print (summary(best_gamm_model_AIC$gam))
                          cat("\n\n")
                          print (best_gamm_model_AIC)
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

 }
                                      ),con=paste(species_name, '_performance_calc_k_AUTO_raw_gamm_fit.txt', sep=""))

# set back to master directory

setwd(master_directory)

if(TPCFUN==T){
TPCFUN <- function (temp_value, size_value) {
                                       formula_gam <- best_model$gam
                                       pred_data <- data.frame(temp=temp_value, size = size_value)
                                       P <- as.vector(predict.gam(formula_gam,pred_data))
                                       if(P>0) {
                                                return(P)
                                               } else { return(0) }
                                               }

return(TPCFUN)

} else {

# return model

return(best_model)
}
}

#############################################################################################################################
#EcoPhysRasters##############################################################################################################
#############################################################################################################################

EcoPhysRasters<-function(rasterpath,
						 
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


require('raster')
require('rlist')
require('mgcv')

##########################################################################################
###################            Load and crop rasters             #########################
##########################################################################################

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
#Mapinguari####################################################################################################################
#############################################################################################################################

#Pseudoabscences

Mapinguari<-function(spcoord,rasterlist, repPA=2, nPA = 1000, PAmin = 50, PAmax = 200, 
gam_k_mgcv=7,
gam_k_gam=7,
DataSplit_percent=80,
NbRunEval_runs=2,
threshold_EnsembleModel=0.5,
write_rasters=F) {


#Mapinguari
require("dismo")
	# dependencies: raster, sp
require("maps")
require("mapdata")	
require("maptools")
require("countrycode")
require("biomod2")
	# dependencies: paralell, reshape, ggplot2

Lat<-spcoord$Lat
Lon<-spcoord$Lon
raster_list<-rasterlist
scenario_names<-names(raster_list)

##########################################################################################
## Ecophysiology Predictors: Write final subset master stacks and get rid of NAN layers  #
##########################################################################################

raster_list_no_na <- list()

for ( i in 1:length(raster_list)) {
                    raster_list_no_na [[i]] <- do.call(stack, Filter(function(e){!all(is.na(values(e)))},unstack(raster_list[[i]])))
                                  }


##########################################################################################
###################                 BIOMOD MODELING                    ###################
##########################################################################################

species_sdm_ecophysiology_rasters<-list()

for(q in 1:length(raster_list_no_na)){

species_sdm_ecophysiology_rasters[[q]] <- raster_list_no_na[[q]]
	
}

# setting explanatory variables

myExpl <- species_sdm_ecophysiology_rasters[[1]]

######   Preparing response variable if presences only or presences and absences   #######

#### create this key function

'%!in%' <- function(x,y)!('%in%'(x,y))

###

                                           myRespXY <- data.frame(Lon = Lon, Lat = Lat)
                                             myResp <- rep(1,length(Lon))  # number of input coordinates
                                  species_presences <- myRespXY
                                         
# NOTICE: %in% and its oposite %!in%


#######################        Biomod Model Options                 ######################

 myRespName <- "Temporary_Mapinguari_calculations"

myBiomodOptions_GLM_GAM_mgcv<- BIOMOD_ModelingOptions(GLM = list( type='simple',
                                                             interaction.level = 0,
                                                             myFormula = NULL,
                                                             test = 'BIC',
                                                             family = 'binomial',
                                                             control = glm.control(epsilon = 1e-08,
                                                                                      maxit = 1000,
                                                                                      trace = FALSE)),

                                               GAM = list( algo = 'GAM_mgcv',
                                                     type = 's_smoother',
                                                            k = gam_k_mgcv,
                                            interaction.level = 0,
                                                    myFormula = NULL,
                                                       family = binomial(link = 'logit'),
                                                       method = 'GCV.Cp',
                                                    optimizer = c('outer','newton'),
                                                       select = FALSE,
                                                        knots = NULL,
                                                      paraPen = NULL,
                                                      control = NULL),

                                           )



myBiomodOptions_GLM_GAM_gam <- BIOMOD_ModelingOptions(GLM = list( type='simple',
                                                             interaction.level = 0,
                                                             myFormula = NULL,
                                                             test = 'BIC',
                                                             family = 'binomial',
                                                             control = glm.control(epsilon = 1e-08,
                                                                                      maxit = 1000,
                                                                                      trace = FALSE)),

                                                   GAM = list( algo = 'GAM_gam',
                                                     type = 's_smoother',
                                                            k = gam_k_gam,
                                            interaction.level = 0,
                                                    myFormula = NULL,
                                                       family = binomial(link = 'logit'),
                                                       method = 'GCV.Cp',
                                                    optimizer = c('outer','newton'),
                                                       select = FALSE,
                                                        knots = NULL,
                                                      paraPen = NULL,
                                                      control = NULL),

                                          )

#######################        Biomod Data Formating                ######################

# Required
# myRespXY
# myResp
# !!!!!!!!!!!!!! requires that myResp is a numeric vector NO AN INTERGER

	myBiomodData <-BIOMOD_FormatingData(resp.var=as.numeric(myResp),
	                                    expl.var=myExpl,
	                                    resp.xy = myRespXY,
	                                    resp.name = myRespName,
	                                    PA.nb.rep = repPA, 
	                                    PA.nb.absences = nPA, # 10*total presences pseudoabsences
	                                    PA.strategy = 'disk',
	                                    PA.dist.min = PAmin*1000, # keep to 50 km min
	                                    PA.dist.max = PAmax*1000, # reduce to 200 km max
	                                    PA.sre.quant = 0.025,
	                                    PA.table = NULL,
	                                    na.rm = TRUE)

print(myBiomodData)

#######################        BIOMOD Modeling                ######################

## !!! NOTICE two gam algorithms this overcomes the limitation seem on mgcv

	myBiomodModelOut_all_mgcv <- try(BIOMOD_Modeling(myBiomodData,
	                                    models = c('GLM','GAM'),
	                                    models.options = myBiomodOptions_GLM_GAM_mgcv,
	                                    NbRunEval=NbRunEval_runs,
	                                    DataSplit=DataSplit_percent,
	                                    models.eval.meth = c('ROC','TSS'), 
	                                    do.full.models=FALSE,
	                                    modeling.id= paste(myRespName, "GLM_GAM_mgcv", sep = "_")))

out_all_mgcv <-capture.output(myBiomodModelOut_all_mgcv)

if ('Failed Models :  none' %!in% out_all_mgcv)     { 
            myBiomodModelOut_all_gam <- try(BIOMOD_Modeling(myBiomodData,
	                                    models = c('GLM','GAM'),
	                                    models.options = myBiomodOptions_GLM_GAM_gam,
	                                    NbRunEval=NbRunEval_runs,
	                                    DataSplit=DataSplit_percent,
	                                    models.eval.meth = c('ROC','TSS'), 
	                                    do.full.models=FALSE,
	                                    modeling.id= paste(myRespName, "GLM_GAM_gam", sep = "_")))

            myBiomodModelOut_all <- myBiomodModelOut_all_gam

                                    } else { 

            myBiomodModelOut_all <- myBiomodModelOut_all_mgcv

                                    }

print(myBiomodModelOut_all)

######################     select models that match algorithm        ######################

GLM_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_GLM", myBiomodModelOut_all@models.computed)]
GAM_models <- myBiomodModelOut_all@models.computed[grep(pattern = "*_GAM", myBiomodModelOut_all@models.computed)]

#######################        Biomod Ensemble Model                 ######################

myBiomodEM_GLM <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = GLM_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = TRUE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = TRUE,
                                  committee.averaging = TRUE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))

myBiomodEM_GAM <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = GAM_models,
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = TRUE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = TRUE,
                                  committee.averaging = TRUE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))


myBiomodEM_all <- try(BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut_all,
                                        chosen.models = 'all',
                                                em.by ='all',
                                          eval.metric = c('ROC'),
                        eval.metric.quality.threshold = threshold_EnsembleModel,
                                            prob.mean = TRUE,
                                              prob.cv = TRUE,
                                              prob.ci = FALSE,
                                        prob.ci.alpha = 0.05,
                                          prob.median = TRUE,
                                  committee.averaging = TRUE,
                                     prob.mean.weight = TRUE,
                               prob.mean.weight.decay = 'proportional' ))


#######################                 Select models                  ######################

myGLMs <- BIOMOD_LoadModels(myBiomodModelOut_all, models=c('GLM'))
myGAMs <- BIOMOD_LoadModels(myBiomodModelOut_all, models=c('GAM'))


# get get formal

# GLM

get_formal_model(get(myGLMs))

# GAM_gam special case

gam_output_capture <- capture.output(myBiomodModelOut_all)

if (any(grepl("GLM_GAM_gam", gam_output_capture) == TRUE)) {
                                                                  print("The system used the gam package")
                                                                  } else { get_formal_model(get(myGAMs)) }

#######################                 Load Models                 ######################

myBiomodProj_GLM <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = GLM_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')

myBiomodProj_GAM <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = GAM_models,
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')

myBiomodProj_all <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut_all,
                         new.env = myExpl,
                         proj.name = 'current_1975',
                         selected.models = 'all',
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.grd')

##########################################################################################
###################               end of biomod modeling               ###################
##########################################################################################

### write summaries

output<-list()

summary<-list()

summary[[1]]<-myBiomodData
summary[[2]]<-myBiomodModelOut_all

output[[1]]<-summary

myBiomodData

# glm capture output

glm_output<-list()

glm_output[[1]]<-get_formal_model(get(myGLMs))
oo <- get_formal_model(get(myGLMs))	
glm_output[[2]]<-summary(oo)
glm_output[[3]]<-data.frame(BIC=BIC(oo),AIC=AIC(oo))

output[[2]]<-glm_output

# gam capture output

gam_output_capture <- capture.output(myBiomodModelOut_all)

gam_output<-list()

if (any(grepl("GLM_GAM_gam", gam_output_capture) == TRUE)) {
                                                                  print("The system used the gam package")
                                                                  } else { 
gam_output[[1]]<-get_formal_model(get(myGAMs))
rr <- get_formal_model(get(myGAMs))	
gam_output[[2]]<-summary(rr)
gam_output[[3]]<-data.frame(BIC=BIC(rr),AIC=AIC(rr))
                                                                  }

output[[3]]<-gam_output

### get and write all models evaluation

evaluations<-list()

evaluations[[1]]<-get_evaluations(myBiomodModelOut_all)
evaluations[[2]]<-get_evaluations(myBiomodEM_all)
evaluations[[3]]<-get_evaluations(myBiomodEM_GLM)
evaluations[[4]]<-get_evaluations(myBiomodEM_GAM)

names(evaluations)<-c('Model','Ensemble','GLM','GAM')

output[[4]]<-evaluations

##########################################################################################
###################               Plotting and projections               #################
##########################################################################################

species_coordinates_plots <- species_presences   # this will need reevaluation if true absences
names(species_coordinates_plots) <- c("Lon", "Lat")
coordinates(species_coordinates_plots) <- ~ Lon + Lat

projections<-list()

for(q in 1:length(species_sdm_ecophysiology_rasters)){

myBiomod_Ensem_GLM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GLM,
                                        new.env = species_sdm_ecophysiology_rasters[[q]],
                                        proj.name = 'Ensemble_GLM',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'))

myBiomod_Ensem_GAM <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_GAM,
                                        new.env = species_sdm_ecophysiology_rasters[[q]],
                                        proj.name = 'Ensemble_GAM',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'))

myBiomod_Ensem_all <- try(BIOMOD_EnsembleForecasting(
                                        EM.output = myBiomodEM_all,
                                        new.env = species_sdm_ecophysiology_rasters[[q]],
                                        proj.name = 'Ensemble_all',
                                        selected.models = 'all',
                                        binary.meth = 'ROC',
                                        compress = 'xz',
                                        clamping.mask = F,
                                        output.format = '.grd'))

pred<-list()
pred[[1]]<-pred_Ensem_GLM_meanByROC <- try(get_predictions(myBiomod_Ensem_GLM)[[1]])/1000
pred[[2]]<-pred_Ensem_GAM_meanByROC <- try(get_predictions(myBiomod_Ensem_GAM)[[1]])/1000
pred[[3]]<-pred_Ensem_all_meanByROC <- try(get_predictions(myBiomod_Ensem_all)[[1]])/1000

names(pred)<-c('GLM','GAM','Ensemble')

projections[[q]]<-pred

}

unlink(paste(getwd(),"/Temporary.Mapinguari.calculations",sep=""),recursive=T)

names(projections)<-names(rasterlist)

output[[5]]<-projections
names(output)<-c('summary','glm_output','gam_output','evaluations','projections')

if(write_rasters==T){
for(u in 1:length(output[[5]])){
 for(o in 1:length(output[[5]][[u]])){
writeRaster(output[[5]][[u]][[o]] , filename=paste('projection',names(output[[5]][[u]])[o],names(output[[5]])[u],sep='_'), overwrite=TRUE)


}
}
}

return(output)
}

plot.Mapinguari<-function(projection_list, mapres, borders=T, spcoord,col=rainbow(100)[1:70] , zlim=c(0,1), axes = T, xlab = NA, ylab = NA,...){	
	
require("maps")
require("mapdata")	
require("maptools")
require("countrycode")

##########################################################################################
################     Get  Country ISO3 Names Code in Species Raster    ###################
##########################################################################################

species_coordinates<-spcoord

# input from user

if(borders==T){
raster_man <- projection_list[[1]][[1]]
number_random_coordinates <- 20000

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

print(country_final_list)
}

##########################################################################################
############################     Read political boundaries    ############################
##########################################################################################

if(borders==T){
species_country_iso3_codes <- country_final_list$country_ISO3
level_resolution <- mapres
species_extent <- extent(raster_man)

# get list of countries of interes at boundary level only

contry_maps_GADM_list <- list ()

for (i in 1:length(species_country_iso3_codes)) {
                                                   country_GADM <- raster::getData('GADM', country=species_country_iso3_codes[i], level=level_resolution,download=T)
                                    contry_maps_GADM_list [[i]] <- crop (country_GADM, species_extent)
                                                }

list_country_border_polygons<-contry_maps_GADM_list
}
	
for(q in 1:length(projection_list))	{
plot(if("[1] \"try-error\"" %in% capture.output(projection_list[[q]][[1]])[3]) { NA } else { projection_list[[q]][[3]]}, col=col , zlim=zlim, axes = axes, xlab = xlab, ylab = ylab, main=paste('all',names(projection_list)[q],sep=" "))
if(borders==T){
for (i in 1:length(list_country_border_polygons)) {
plot (list_country_border_polygons[[i]],add=T, border="black",lwd=0.7)
}
}
                                                   
points(species_coordinates,pch=21,cex=0.9, bg="black",col="white")

devAskNewPage(ask=T)

plot(if("[1] \"try-error\"" %in% capture.output(projection_list[[q]][[1]])[3]) { NA } else { projection_list[[q]][[1]]}, col=col , zlim=zlim, axes = axes, xlab = xlab, ylab = ylab, main=paste('GLM',names(projection_list)[q],sep=" "), ...)
if(borders==T){
for (i in 1:length(list_country_border_polygons)) {
plot (list_country_border_polygons[[i]],add=T, border="black",lwd=0.7)
}
}

points(species_coordinates,pch=21,cex=0.9, bg="black",col="white")

devAskNewPage(ask=T)

plot(if("[1] \"try-error\"" %in% capture.output(projection_list[[q]][[2]])[3]) { NA } else { projection_list[[q]][[2]]},
                             col=col , zlim=zlim, axes = axes, xlab = xlab, ylab = ylab, main=paste('GAM',names(projection_list)[q],sep=" "), ...)
if(borders==T){                             
for (i in 1:length(list_country_border_polygons)) {
plot (list_country_border_polygons[[i]],add=T, border="black",lwd=0.7)
} 
}
                                                 
points(species_coordinates,pch=21,cex=0.9, bg="black",col="white")
}
}

