########################################################################
# This is the new block of functions for Performance ###################
############### Sinervo's new performance functions ####################
#TPC fo Pleurodema Thaul
PleurodemaTPCAdapt<-
  function(temp,acc,Lat){  # latitude is either varying with the raster (acclimation x source pop interaction

    # or fixed for the value of Caulle or Atacama, no acclimation with latitude but

    # acclimation of one site, OR

    # just use TPC from Caulle or Atacama site, ignoring interactions and acclimation
    P= 10.373+(0.1309334*Lat)
    +0.0375491*temp+0.0164633*acc
    +0.0011552*((acc-19.2129)*(acc-19.2129))
    -0.015106*((temp-19.3072)*(temp-19.3072))
    +0.0031662*((temp-19.3072)*(acc-19.2129))
    +0.0055606*(temp-19.3072)*(Lat+37.5482)
    +0.0083017*(acc-19.2129)*(Lat+37.5482)+
      -0.000451*((acc-19.2129)*(acc-19.2129))*(Lat+37.5482)
    -0.0004*((temp-19.3072)*(temp-19.3072))*(Lat+37.5482)
    +0.0003385*((temp-19.3072)*(acc-19.2129))*(Lat+37.5482)

    if(P>0)
      return(P)
    if(P<=0)
      return(0)
  }
# Once you have the TPC function computed either during breeding or across the year,

# then you can simply zero out the interaction terms by setting Lat not equal to the raster Lat but to the

#latitude of either Caulle or Atacma, this is thus a fixed TPC for either curve,

#Gabriel wrote this into the mapinguari so it should run right away



# E.g., SDM fits as before

# Here is a simple species distribution model using the rasters generated

library(dismo)
library(maps)

P_thaul = read.csv("pthaul_dist.csv")
coordinates(P_thaul) = ~ Lon + Lat

source("Mapinguari_v0_7.R")

#list of rasters
#h_a_breed_raster
#h_r_breed_raster
#h_a_year_raster
#h_r_year_raster
#Perfbreed_raster
#Perfyear_raster
#Perfaccbreed_raster
#Perfacc_year_raster
#DeltaPerfbreed_raster
#DeltaPerfyear_raster
#Precyear_raster
#PrecNBreed_raster
#Precbreed_raster



# YOU MUST COMPUTE THESE LAYERS ABOVE IN MAPINGUARI

# Let's build a sdm with the Performance and precipitation over the breeding season, their interaction and quadratic terms
# below is pseudocode that uses the above rasters


predictors <- stack(Perfbreed_raster,Precbreed_raster)
names(predictors) = c("PerfBreed","PrecBreed")
backgr <- randomPoints(predictors, 500)
presvals <- extract(predictors, P_thaul)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
m_full_model <- glm(pb ~ PerfBreed+PrecBreed+PerfBreed*PrecBreed+PerfBreed*PerfBreed+PrecBreed+PrecBreed, data=sdmdata)
summary(m_full_model)

quartz()
p_full_model <- predict(predictors, m_full_model)
plot(p_full_model,col=rainbow(100)[1:80],main="1975 full model")
points(P_thaul)
map('world',add=T)
