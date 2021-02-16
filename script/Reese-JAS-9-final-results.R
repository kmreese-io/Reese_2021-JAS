##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## FINAL RESULTS ##
##########################################################################################
##########################################################################################
## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

##########################################################################################
## Calculate total number of households in region by combining known household occupation with extrapolated household occupation by year
total.study.years <- seq(year.start,year.end,year.duration)
occupation.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')
sum.occupation.household <- as.numeric(colSums(occupation.household[,8:ncol(occupation.household)]))

total.extrapolated.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/extrapolated-population-by-household.csv',header=T,row.names=1)
sum.extrapolated.households <- as.numeric(colSums(total.extrapolated.households,na.rm=T))

region.population.household <- sum.occupation.household + sum.extrapolated.households

# Export total number of predicted households including surveyes and unsurveyed areas across central Mesa Verde region
utils::write.csv(region.population.household,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-population-by-household.csv',row.names=total.study.years)

##########################################################################################
## Calculate total population in region by smoothing region household occupation by average life-expectancy through time
total.study.years <- seq(year.start,year.end,year.duration)
region.population.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-population-by-household.csv',row.names=1)
# Create matrix for smoothed results of regional household population by life-expectancy
region.occupation.population <- matrix(NA,nrow=nrow(region.population.household),ncol=1)

smooth.start <- 450
smooth.end <- 1300

## Smooth the results with corresponding average life-expectancy [@Kohler_and_Reese_2014] to determine range of population estimates for study period
for(l in smooth.start:smooth.end) {
  window.life.expectancy <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == l ),2]
  smooth.prediction <- smoother::smth.gaussian(as.numeric(region.population.household[,1]),window=window.life.expectancy,tails=T)
  region.occupation.population[(l-449):(l-449)] <- round(smooth.prediction[(l-449):(l-449)])
}

# Export household results smoothed by average life-expectancy
utils::write.csv(region.occupation.population,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-occupation-by-population.csv',row.names=total.study.years)

# ##########################################################################################
# ##########################################################################################
# ## Calculate VEP II results for comparison with artificial neural network analysis
# vepii.database <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-n-results.csv')
# # Import VEP II momentization information
# vepii.momentization <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-momentization.csv')
# # Define habitation sites within community centers
# vepii.cc.households <- vepii.database[which(vepii.database$Center == 1 & vepii.database$recordtypedesc == 'Habitation'),]
# # Limit VEP II data to habitation sites outside of community centers
# vepii.households <- vepii.database[which(vepii.database$Center == 0 & vepii.database$recordtypedesc == 'Habitation'),]
# 
# # Import rasters of surveyed and unsurveyed areas for masking in extrapolation model
# surveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-surveyed')
# unsurveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-unsurveyed')
# 
# # Define six subregions for extrapolation
# subregion.1 <- study.area.subregions[1,]
# subregion.2 <- study.area.subregions[2,]
# subregion.3 <- study.area.subregions[3,]
# subregion.4 <- study.area.subregions[4,]
# subregion.5 <- study.area.subregions[5,]
# subregion.6 <- study.area.subregions[6,]
# 
# # Calculate proportion of each subregion that has previously been surveyed
# proportion.1 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.1),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.1))
# proportion.2 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.2),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.2))
# proportion.3 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.3),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.3))
# proportion.4 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.4),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.4))
# proportion.5 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.5),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.5))
# proportion.6 <- 1 / (((ncell(surveyed.area) - cellStats(raster::mask(surveyed.area,subregion.6),'countNA')) * (raster::res(surveyed.area)[1] * raster::res(surveyed.area)[2] )) / rgeos::gArea(subregion.6))
# 
# # Create matrix for results
# vepii.calculated.population <- matrix(NA,nrow=14,ncol=1)
# 
# # Extrapolate VEP II population estimates based on Step 5 outlined in @Schwindt_et_al_2016: 80
# for(c in 48:61) {
# 
#   # Calculate total households within community center for modeling period
#   sum.cc.households <- sum(vepii.cc.households[which(vepii.cc.households[,c] >= 1 ),c])
# 
#   # Limit VEP II data to occupied small residential sites with < 9 pitstructures
#   vepii.small.sites <- vepii.households[which(vepii.households$Peakpop < 9 & vepii.households[,c] >= 1),]
# 
#   # Convert small sites to spatial points
#   vepii.small.sites.spatial <- base::matrix(NA,nrow=nrow(vepii.small.sites),ncol=2)
#   vepii.small.sites.spatial[,1] <- vepii.small.sites$UTMEast
#   vepii.small.sites.spatial[,2] <- vepii.small.sites$UTMNorth
#   vepii.small.sites.spatial <- sp::SpatialPointsDataFrame(coords=vepii.small.sites.spatial,vepii.small.sites,proj4string=nad27.projection)
#   vepii.small.sites.spatial <- sp::spTransform(vepii.small.sites.spatial,master.projection)
# 
#   # Limit small sites by subregion
#   sites.1 <- vepii.small.sites.spatial[subregion.1,]
#   sites.2 <- vepii.small.sites.spatial[subregion.2,]
#   sites.3 <- vepii.small.sites.spatial[subregion.3,]
#   sites.4 <- vepii.small.sites.spatial[subregion.4,]
#   sites.5 <- vepii.small.sites.spatial[subregion.5,]
#   sites.6 <- vepii.small.sites.spatial[subregion.6,]
# 
#   # Extrapolate likely number of unrecorded sites and multiply by mean household occupation of recorded sites
#   extrapolated.1 <- (proportion.1 * nrow(sites.1)) * mean(as.data.frame(sites.1)[,c])
#   extrapolated.2 <- (proportion.2 * nrow(sites.2)) * mean(as.data.frame(sites.2)[,c])
#   extrapolated.3 <- (proportion.3 * nrow(sites.3)) * mean(as.data.frame(sites.3)[,c])
#   extrapolated.4 <- (proportion.4 * nrow(sites.4)) * mean(as.data.frame(sites.4)[,c])
#   extrapolated.5 <- (proportion.5 * nrow(sites.5)) * mean(as.data.frame(sites.5)[,c])
#   extrapolated.6 <- (proportion.6 * nrow(sites.6)) * mean(as.data.frame(sites.6)[,c])
# 
#   sum.ss.households <- sum(extrapolated.1,extrapolated.2,extrapolated.3,extrapolated.4,extrapolated.5,extrapolated.6)
# 
#   # Calculate the momentary population of community center sites and small non-community center sites
#   cc.population <- (sum.cc.households * (vepii.momentization[c-47,]$USELIFE_CC / vepii.momentization[c-47,]$DURATION)) * 6
#   ss.population <- (sum.ss.households * (vepii.momentization[c-47,]$USELIFE_SS / vepii.momentization[c-47,]$DURATION)) * 6
# 
#   # Aggregate VEP II reconstruction results
#   vepii.calculated.population[c-47,1] <- round(cc.population + ss.population)
# 
# }
# 
# utils::write.csv(vepii.calculated.population,'/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-reconstructed.csv')
# 
##########################################################################################
##########################################################################################
