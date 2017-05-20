######################################################################
#### First Example: a model with ha and hr by the Senoid method ######
######################################################################

# Change work directory
setwd('~/Desktop/Mapinguari_v0_5')

# Load distribution file
FulanusDistribution<-read.table('FulanusDistribution.txt',h=T)

# Clean distribution file
FulanusDistribution_clean<-cleanpoints(FulanusDistribution,altpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes/alt_10m_bil",km_merge = 2)

# Create variable rasters (I will run for present and rcp 45 only)
EPRasters<-EcoPhysRasters(rasterpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes",
			   			  hahrmethod='Senoid',
			   			  Tlwr=25,
						  Tupr=37,					 
			   			  sp_coord=FulanusDistribution_clean,
			   			  margin=2,
			   			  res = '10m',			   			 
			   			  scenarios=c('present', '2050rcp45', '2070rcp45'),
			   			  variables=c('ha', 'hr'),
			   			  write_rasters=T)
			   			  
# Look at the pretty maps

# Test for colinearity			   			  
colinearity.test(inputvar=EPRasters[[1]],method='VIF',threshold_vif=10)

# The colinearity test says everything is ok, so lets proceed to the SDM
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections
plot.Mapinguari(FulanusSDM[[5]],mapres=1,spcoord=FulanusDistribution_clean)	

################################################################################################
#### Second Example: a model with ha and hr by the Richards method and also Precipitation ######
################################################################################################

# Change work directory
setwd('~/Documents/Ciência/Doutorado/Mapinguari/Mapinguari_v0_6')

# Load distribution file
FulanusDistribution<-read.table('FulanusDistribution.txt',h=T)

# Clean distribution file
FulanusDistribution_clean<-cleanpoints(FulanusDistribution,altpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes/alt_10m_bil",km_merge = 2)

# Load the HOBO data

FulanusHOBO<-read.table('FulanusHOBO.txt',h=T)

# Create the Richards model for ha and hr from HOBO data

RichCoef_Fulanus<-RichHOBO(FulanusHOBO,
						   method='mean',
						   Tlwr=25,
						   Tupr=37,
						   diel='diurnal')
						   
# Create variable rasters (this time I will run for present and rcp 26 only)
EPRasters<-EcoPhysRasters(rasterpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes",
			   			  hahrmethod='Richards',
			   			  RichCoef=	RichCoef_Fulanus,	 
			   			  sp_coord=FulanusDistribution_clean,
			   			  margin=2,
			   			  res = '10m',			   			 
			   			  scenarios=c('present', '2050rcp26', '2070rcp26'),
			   			  variables=c('ha','hr','Precipitation'),
			   			  write_rasters=T)
			   			  
# Look at the pretty maps

# Test for colinearity			   			  
colinearity.test(inputvar=EPRasters[[1]],method='VIF',threshold_vif=10)

# The colinearity test says ha has a colinearity problem! Let's remove it from the list

names(EPRasters[[1]]) # ha is the third variable

#So let's create a new list with all variables, excep the third one, for each scenario (present, 2050rcp26 and 2070rcp26)

EPRasters_noncolinear<-list()
EPRasters_noncolinear[[1]]<-EPRasters[[1]][[-3]] #present
EPRasters_noncolinear[[2]]<-EPRasters[[2]][[-3]] #2050rcp26
EPRasters_noncolinear[[3]]<-EPRasters[[3]][[-3]] #2070rcp26

# Take a look at the new list

EPRasters_noncolinear

# Now we proceed to the SDM
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters_noncolinear,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections (This time I will do only country borders)
plot.Mapinguari(FulanusSDM[[5]],mapres=0,spcoord=FulanusDistribution_clean)	

#########################################################################################
#### Third Example: a model with ha and hr by the Richards method and Performance #######
#########################################################################################

# Change work directory
setwd('~/Desktop/Mapinguari_v0_4_1')

# Load distribution file
FulanusDistribution<-read.table('FulanusDistribution.txt',h=T)

# Clean distribution file
FulanusDistribution_clean<-cleanpoints(FulanusDistribution,altpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes/alt_10m_bil",km_merge = 2)

# Load the HOBO data

FulanusHOBO<-read.table('FulanusHOBO.txt',h=T)

# Create the Richards model for ha and hr from HOBO data

RichCoef_Fulanus<-RichHOBO(FulanusHOBO,
						   method='mean',
						   Tlwr=25,
						   Tupr=37,
						   diel='diurnal')

#Load the Performance data
FulanusPerf<-read.table("FulanusPerformance.txt",h=T)

#Calculates the Thermal Performance Curve
FulanusPerfFUN<-PerfGAMM2(performance_data=FulanusPerf, 
                                     dependent_variable='performance',
                                     smooth_predictors='temp',
                                     linear_predictors='size',
                                     quadratic_predictors = NULL,
                                     criterion = "BIC",
                                     knots_vector = 7,
                                     plot_variable_on_x='temp',
                                     plot_variable_on_y='performance',
                                     plot_variable_on_z='size',
                                     output_directory='~Desktop',
                                     TPCFUN=T)

#Creates an object with the thermal performance function
FulanusPerfFUN<-FulanusPerfGAMM[[1]]
				   
# Create variable rasters (This time I will do all scenarios, but let's use a smaller area)
EPRasters<-EcoPhysRasters(rasterpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes",
			   			  hahrmethod='Richards',
			   			  RichCoef=	RichCoef_Fulanus,
			   			  PerfFUN=FulanusPerfFUN,
			   			  size=3.8,						 
			   			  Lon_max=40,
			   			  Lon_min=25,
			   			  Lat_max=10,
			   			  Lat_min=-5,
			   			  res = '10m',			   			 
			   			  scenarios=c('present'),
			   			  variables=c('ha','hr','Performance'),
			   			  write_rasters=T)
			   			  
# Look at the pretty maps

# Test for colinearity			   			  
colinearity.test(inputvar=EPRasters[[1]],method='VIF',threshold_vif=10)

# Now we proceed to the SDM
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections
plot.Mapinguari(FulanusSDM[[5]],mapres=1,spcoord=FulanusDistribution_clean)	

################################################################################################################################
#### Forth Example: a model with ha and hr by the Richards method, evaporative water loss and Performance with hydration #######
################################################################################################################################

# Change work directory
setwd('~/Desktop/Mapinguari_v0_4_1')

# Load distribution file
FulanusDistribution<-read.table('FulanusDistribution.txt',h=T)

# Clean distribution file
FulanusDistribution_clean<-cleanpoints(FulanusDistribution,altpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes/alt_10m_bil",km_merge = 2)

# Load the HOBO data

FulanusHOBO<-read.table('FulanusHOBO.txt',h=T)

# Create the Richards model for ha and hr from HOBO data

RichCoef_Fulanus<-RichHOBO(FulanusHOBO,
						   method='mean',
						   Tlwr=25,
						   Tupr=37,
						   diel='nocturnal')

#Load the Performance data
FulanusPerf<-read.table("FulanusPerformance.txt",h=T)

#Calculates the Thermal Performance Curve
FulanusPerfGAMM<-PerfGAMM(FulanusPerf, hydration =T)

#Creates an object with the thermal performance function
FulanusPerfFUN<-FulanusPerfGAMM[[1]]

# Load Agar model data  
FulanusAgarModels<-read.table('FulanusAgarModels.txt',h=T)

#Run the EWL Richard model
EWLCoef_Fulanus<-RichWater(EWLdf=FulanusAgarModels)
			   
# Create variable rasters (This time I will run for present only), and will create the EWL surface for sunny and dry microhabitats, and the performance after 2 hours of dissecation
EPRasters<-EcoPhysRasters(rasterpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes",
			   			  hahrmethod='Richards',
			   			  RichCoef=	RichCoef_Fulanus,
			   			  PerfFUN=FulanusPerfFUN,
			   			  EWLPerf=T,
			   			  EWLCoef=EWLCoef_Fulanus,
			   			  EWLdry=T,
			   			  EWLshade=F,
			   			  dehydrationtime=2,
			   			  size=3.8,						 
			   			  Lon_max=40,
			   			  Lon_min=25,
			   			  Lat_max=10,
			   			  Lat_min=-5,
			   			  res = '10m',			   			 
			   			  scenarios=c('present'),
			   			  variables=c('ha', 'hr', 'Performance','EWL'),
			   			  write_rasters=T)
			   			  
# Look at the pretty maps

# Test for colinearity			   			  
colinearity.test(inputvar=EPRasters[[1]],method='VIF',threshold_vif=10)

# The colinearity test says ha and hr have a colinearity problem! Let's remove it from the list

names(EPRasters[[1]]) # hr is the third variable and ha is the forth one

#So let's create a new list with all variables, excep the third one, for each scenario (present)

EPRasters_noncolinear<-list()
EPRasters_noncolinear[[1]]<-EPRasters[[1]][[-c(3,4)]] #present

# Take a look at the new list

EPRasters_noncolinear

# Now we proceed to the SDM
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters_noncolinear,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections (This time I will do only country borders)
plot.Mapinguari(FulanusSDM[[5]],mapres=0,spcoord=FulanusDistribution_clean)	

##############################################################################################################################
#### Fifth Example: a model with ha and hr by Richards method, Solar Radiation and Evapotranspiration (With examples of operations with variables) #######
##############################################################################################################################

# Change work directory
setwd('~/Desktop/Mapinguari_v0_4_1')

# Load distribution file
FulanusDistribution<-read.table('FulanusDistribution.txt',h=T)

# Clean distribution file
FulanusDistribution_clean<-cleanpoints(FulanusDistribution,altpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes/alt_10m_bil",km_merge = 2)

# Load the HOBO data

FulanusHOBO<-read.table('FulanusHOBO.txt',h=T)

# Create the Richards model for ha and hr from HOBO data

RichCoef_Fulanus<-RichHOBO(FulanusHOBO,
						   method='mean',
						   Tlwr=25,
						   Tupr=37,
						   diel='nocturnal')
			   
# Create variable rasters (This time I will run for present only), and will create the EWL surface for sunny and dry microhabitats, and the performance after 2 hours of dissecation
EPRasters<-EcoPhysRasters(rasterpath="~/Desktop/Mapinguari_v0_4_1/global_grids_10_minutes",
			   			  hahrmethod='Richards',
			   			  RichCoef=	RichCoef_Fulanus,			   			  			 
			   			  Lon_max=40,
			   			  Lon_min=25,
			   			  Lat_max=10,
			   			  Lat_min=-5,
			   			  res = '10m',			   			 
			   			  scenarios=c('present','2050rcp85','2070rcp85'),
			   			  variables=c('ha', 'hr', 'ET','Radiation'),
			   			  write_rasters=T)
			   			  
# Look at the pretty maps

# Test for colinearity			   			  
colinearity.test(inputvar=EPRasters[[1]],method='VIF',threshold_vif=10)

# It seems hr and PET have colinearity issues, but instead of removing them, let's use them in operations to create meaningful variables. Let's create ha-hr, that is 'real' hours of restriction and PET-AET that is climatic water deficit

names(EPRasters[[1]]) 

#present
hahr_present<-EPRasters[[1]][[5]]-EPRasters[[1]][[4]]
names(hahr_present)<-'hahr'
CWD_present<-EPRasters[[1]][[1]]-EPRasters[[1]][[2]]
names(CWD_present)<-'CWD'

#present
hahr_2050rcp85<-EPRasters[[2]][[5]]-EPRasters[[1]][[4]]
names(hahr_2050rcp85)<-'hahr'
CWD_2050rcp85<-EPRasters[[2]][[1]]-EPRasters[[1]][[2]]
names(CWD_2050rcp85)<-'CWD'

#present
hahr_2070rcp85<-EPRasters[[3]][[5]]-EPRasters[[1]][[4]]
names(hahr_2070rcp85)<-'hahr'
CWD_2070rcp85<-EPRasters[[3]][[1]]-EPRasters[[1]][[2]]
names(CWD_2070rcp85)<-'CWD'

#So let's recreate a new list adding the variables we created, removing the variables we used to create them and the solar radiation variable from the previous list

EPRasters_new<-list()
EPRasters_new[[1]]<-stack(EPRasters[[1]][[3]],hahr_present,CWD_present) 
EPRasters_new[[2]]<-stack(EPRasters[[2]][[3]],hahr_2050rcp85,CWD_2050rcp85) 
EPRasters_new[[3]]<-stack(EPRasters[[3]][[3]],hahr_2070rcp85,CWD_2070rcp85) 
names(EPRasters_new)<-c('present','2050rcp85','2070rcp85')

# Take a look at the new list

EPRasters_new

#now let's test it for colinearity

colinearity.test(inputvar=EPRasters_new[[1]],method='VIF',threshold_vif=10)

#No colinearity!!!

# Now we proceed to the SDM
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters_new,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections (This time I will do only country borders)
plot.Mapinguari(FulanusSDM[[5]],mapres=0,spcoord=FulanusDistribution_clean)	

##############################################################################################################################
#### Sixth Example: Recreating the first model by loading saved surfaces
##############################################################################################################################

#find files
all_rasters <- list.files(path = '~/Desktop/Mapinguari_v0_4_1', pattern="*.grd$",full.names=T, ignore.case=T)

#check the order
all_rasters

#There are 6 files on my folder: 

#[1] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_a_2050rcp45.grd"
#[2] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_a_2070rcp45.grd"
#[3] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_a_present.grd"  
#[4] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_r_2050rcp45.grd"
#[5] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_r_2070rcp45.grd"
#[6] "/Users/gabriel/Desktop/Mapinguari_v0_4_1/total_h_r_present.grd" 

#read and append them to objects
ha_2050rcp45 <- raster(all_rasters[1])
ha_2070rcp45 <- raster(all_rasters[2])
ha_present <- raster(all_rasters[3])
hr_2050rcp45 <- raster(all_rasters[4])
hr_2070rcp45 <- raster(all_rasters[5])
hr_present <- raster(all_rasters[6])

#Recreate list

EPRasters<-list()
EPRasters[[1]]<-stack(ha_present,hr_present)
EPRasters[[2]]<-stack(ha_2050rcp45,hr_2050rcp45)
EPRasters[[3]]<-stack(ha_2070rcp45,hr_2070rcp45)
names(EPRasters)<-c('present','2050rcp45','2070rcp45')
			   			  
FulanusSDM<-Mapinguari(FulanusDistribution_clean,
		 rasterlist=EPRasters,
		 repPA=5,
		 nPA = 1000, 
		 PAmin = 50, 
		 PAmax = 200,
		 write_rasters=T)

# Plot the projections (This time I will do only country borders)
plot.Mapinguari(FulanusSDM[[5]],mapres=0,spcoord=FulanusDistribution_clean)	
