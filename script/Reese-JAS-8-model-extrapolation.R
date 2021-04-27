##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## PREDICTIVE MODEL - EXTRAPOLATION ##
##########################################################################################
##########################################################################################
## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)

##########################################################################################
## Calculate topographic information for predictive layers 2--4 (region.dem is predictive layer 1)
region.slope <- raster::terrain(region.dem,'slope')
raster::writeRaster(region.slope,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-slope')
region.aspect <- raster::terrain(region.dem,'aspect')
raster::writeRaster(region.aspect,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-aspect')
region.flowdir <- raster::terrain(region.dem,'flowdir')
raster::writeRaster(region.flowdir,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-flowdir')

##########################################################################################
## Create cost-raster from region.dem
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)
require(gdistance)
n.directions <- 4
heightDiff <- function(x){x[2] - x[1]}
hd <- gdistance::transition(region.dem,heightDiff,directions=n.directions,symm=F)
slope <- gdistance::geoCorrection(hd)
adj <- raster::adjacent(region.dem,cells=1:ncell(region.dem),pairs=T,directions=n.directions)
speed <- slope
speed[adj] <- ((6 * exp(-3.5 * abs(slope[adj] + 0.05))) * 1000)
cost.raster <- gdistance::geoCorrection(speed)
parallel::stopCluster(clusters)

##########################################################################################
## Export cost-raster
base::saveRDS(cost.raster,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-raster-4')
cost.raster <- base::readRDS('./database/SPATIAL/DEM/COST/cost-raster-4')

##########################################################################################
## Import ephemeral drainage and permanent river resources, calculate accumulated cost to resources across study area, and export cost-surfaces
# Ephemeral drainages
drainages <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/HYDROGRAPHY/NORTHERN-SAN-JUAN',layer='VEPIIN-HYDRO')
projection(drainages) <- master.projection
drainages.limited <- raster::crop(drainages,study.area.extent)
drainage.points <- as(drainages.limited,'SpatialPoints')
cost <- gdistance::accCost(cost.raster,coordinates(drainage.points))
cost <- raster::resample(cost,region.dem)
raster::writeRaster(cost,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-drainages')
# Permanent rivers
rivers <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/HYDROGRAPHY/rivers-us',layer='us-rivers')
rivers <- sp::spTransform(rivers,master.projection)
rivers.limited <- raster::crop(rivers,study.area.extent)
river.points <- as(rivers.limited,'SpatialPoints')
cost <- gdistance::accCost(cost.raster,coordinates(river.points))
cost <- raster::resample(cost,region.dem)
raster::writeRaster(cost,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-rivers')

##########################################################################################
## Import topographic information for predictive layers 2--4 (region.dem is predictive layer 1), and cost-surfaces for predictive layers 5--6
region.slope <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-slope')
region.aspect <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-aspect')
region.flowdir <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-flowdir')
cost.drainages <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-drainages')
cost.rivers <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-rivers')

##########################################################################################
## Create predictive raster stack with universal layers 1--6
# Prediction layer: Elevation
layer.1 <- raster::crop(region.dem,study.area.extent)
# Prediction layer: Slope
layer.2 <- raster::crop(region.slope,study.area.extent)
# Prediction layer: Aspect
layer.3 <- raster::crop(region.aspect,study.area.extent)
# Prediction layer: Flow direction
layer.4 <- raster::crop(region.flowdir,study.area.extent)
# Prediction layer: Distance to ephemeral water sources
layer.5 <- raster::crop(cost.drainages,study.area.extent)
# Prediction layer: Distance to permanent water sources
layer.6 <- raster::crop(cost.rivers,study.area.extent)
# Universal predictive raster stack
prior.stack <- raster::stack(layer.1,layer.2,layer.3,layer.4,layer.5,layer.6)

##########################################################################################
## Download growing niche information for study area
study.area.extent.longlat <- sp::spTransform(study.area.extent,longlat.projection)
reconstruction <- paleocar::get_bocinsky2016(template = study.area.extent.longlat,
                                             label = 'VEPIIN-GN',
                                             raw.dir = '/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/PALEOCAR/RAW',
                                             extraction.dir = '/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/PALEOCAR/EXTRACTIONS/',
                                             prcp_threshold = 300,
                                             gdd_threshold = 1800,
                                             years = 1:2000,
                                             force.redo = F)

##########################################################################################
## Isolate growing niche, reproject raster stacks, resample to 10 m resolution, and export processed raster stacks
growing.niche <- reconstruction$niche[[(year.start-20):year.end]]
raster::projection(growing.niche) <- longlat.projection
growing.niche <- raster::projectRaster(growing.niche,crs=master.projection,method='ngb')
foreach::registerDoSEQ()
growing.niche.resample <- raster::resample(growing.niche,region.dem)
raster::writeRaster(growing.niche.resample,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/growing-niche',overwrite=T)

##########################################################################################
## Isolate temperature, reproject raster stacks, resample to 10 m resolution, and export processed raster stacks
temperature <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/PALEOCAR/EXTRACTIONS/GDD_1-2000.tif')
temperature <- temperature[[(year.start-20):year.end]]
raster::projection(temperature) <- longlat.projection
temperature <- raster::projectRaster(temperature,crs=master.projection,method='ngb')
foreach::registerDoSEQ()
temperature.resample <- raster::resample(temperature,region.dem)
raster::writeRaster(temperature.resample,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/temperature',overwrite=T)

##########################################################################################
## Isolate precipitation, reproject raster stacks, resample to 10 m resolution, and export processed raster stacks
precipitation <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/PALEOCAR/EXTRACTIONS/PPT_1-2000.tif')
precipitation <- precipitation[[(year.start-20):year.end]]
raster::projection(precipitation) <- longlat.projection
precipitation <- raster::projectRaster(precipitation,crs=master.projection,method='ngb')
foreach::registerDoSEQ()
precipitation.resample <- raster::resample(precipitation,region.dem)
raster::writeRaster(precipitation.resample,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/precipitation',overwrite=T)

##########################################################################################
## Import growing niche, temperature, and precipitation raster stacks for predictive layers 7--9
growing.niche <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/growing-niche')
raster::projection(growing.niche) <- master.projection
temperature <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/temperature')
raster::projection(temperature) <- master.projection
precipitation <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/precipitation')
raster::projection(precipitation) <- master.projection

##########################################################################################
## Import rasters of surveyed and unsurveyed areas for masking in predictive model
surveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-surveyed')
unsurveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-unsurveyed')

##########################################################################################
## Import occupation-by-household results to extrapolate known site locations to unsurveyed areas
population.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')
# Subset dataset by sites with < 9 total pitstructures
small.sites <- population.households[which(population.households$CENTER == 0 & population.households$PITSTRUCTURES < 9 & population.households$PITSTRUCTURES > 0),]
# Create matrix and file to export calculated extrapolated households
total.extrapolated.households <- matrix(NA,nrow=8,ncol=ncol(small.sites[,8:ncol(small.sites)]))
colnames(total.extrapolated.households) <- as.character(c(year.start:year.end))
utils::write.csv(total.extrapolated.households,file='/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/extrapolated-population-by-household.csv',row.names=as.character(c(1:8)))

##########################################################################################
## Household extrapolation model from surveyed to unsurveyed areas

for(years in 8:ncol(small.sites)) {

  # Load file with extrapolated predictions
  total.extrapolated.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/extrapolated-population-by-household.csv',header=T,row.names=1)
  # Identify households occupied with 1--8 pitstructures for given year
  year.occupation <- small.sites[which(small.sites[,years] > 0),]
  
  # If no household are predicted to be occupied in the given year, enter 0 extrapolated households, and proceed to following year
  no.occupation <- FALSE
  tryCatch({
    
    population.households.spatial <- base::matrix(NA,nrow=nrow(year.occupation),ncol=2)
    population.households.spatial[,1] <- year.occupation$X_COORDS
    population.households.spatial[,2] <- year.occupation$Y_COORDS
    population.households.spatial <- sp::SpatialPointsDataFrame(coords=population.households.spatial,year.occupation,proj4string=master.projection)
    names(population.households.spatial) <- names(year.occupation)
    
  },
  
  error = function(e) { no.occupation <<- TRUE})
  
  if(no.occupation == TRUE) {
    
    total.extrapolated.households[,years-8] <- 0
    
  }
  
  # If households are predicted to be occupied in the given year, continue prediction
  else{
    
    # Identify year for predictive model and 20 years before to calculate household perspective[@Jha_et_al_2018; Marin_2010; @West_et_al_2008] of growing niche, temperature, and precipitation
    start <- years - 8
    end <- years - 8 + 20
    
    # Average 20 years for predictive layers 7--9
    # Prediction layer: Average annual growing degree days (temperature)
    layer.7 <- raster::stackApply(temperature[[start:end]],indices=nlayers(temperature[[start:end]]),fun='mean')
    # Prediction layer: Average annual precipitation
    layer.8 <- raster::stackApply(precipitation[[start:end]],indices=nlayers(precipitation[[start:end]]),fun='mean')
    # Prediction layer: Average annual maize growing niche
    layer.9 <- raster::stackApply(growing.niche[[start:end]],indices=nlayers(growing.niche[[start:end]]),fun='mean')
    # Stack complete set of predictive raster layers 1--9
    raster.stack <- raster::stack(prior.stack,layer.7,layer.8,layer.9)
    
    # Begin parallel computing environment
    clusters <- parallel::makeCluster(8)
    doParallel::registerDoParallel(clusters)
    
    # Complete predictive model for sites by total number of recorded pitstructures 1--8
    extrapolated.households <- foreach::foreach(pitstructures = 1:8,.combine=rbind) %dopar% {
      
      # Identify sites with given number of pitstructures for given year
      sites.by.pitstructure <- year.occupation[year.occupation$PITSTRUCTURES == pitstructures ,]
      
      # If no households with given number of pitstructures are predicted to be occupied in the given year, enter 0 extrapolated households, and proceed to following number of pitstructures
      no.sites.by.pitstructure <- FALSE
      tryCatch({ 
        
        known.coordinates <- base::matrix(NA,nrow=nrow(sites.by.pitstructure),ncol=2)
        known.coordinates[,1] <- sites.by.pitstructure$X_COORDS
        known.coordinates[,2] <- sites.by.pitstructure$Y_COORDS
        known.coordinates <- sp::SpatialPointsDataFrame(coords=known.coordinates,sites.by.pitstructure,proj4string=master.projection)
        names(known.coordinates) <- names(sites.by.pitstructure)
        
      },
      
      error = function(e) { no.sites.by.pitstructure <<- TRUE})
      
      if(no.sites.by.pitstructure == TRUE) {
        
        total.extrapolated.households[pitstructures,years-8] <- 0
        
      }
      
      # If households with given number of pitstructures are predicted to be occupied in the given year, continue prediction
      else{
        
        known.coordinates <- base::matrix(NA,nrow=nrow(sites.by.pitstructure),ncol=2)
        known.coordinates[,1] <- sites.by.pitstructure$X_COORDS
        known.coordinates[,2] <- sites.by.pitstructure$Y_COORDS
        known.coordinates <- sp::SpatialPointsDataFrame(coords=known.coordinates,sites.by.pitstructure,proj4string=master.projection)
        names(known.coordinates) <- names(sites.by.pitstructure)
        
        # Run predictive model using complete set of predictive raster layers 1--9 and coordinates of occupied households with given number of pitstructures
        max.entropy <- dismo::maxent(raster.stack,sp::coordinates(known.coordinates),removeDuplicates=F)
        raster.probabilities <- dismo::predict(max.entropy,raster.stack)
        
        # Extract predictive probabilities for knowingly occupied cells and average predictive values
        mean.probabilities <- mean(raster:::extract(raster.probabilities,sp::coordinates(known.coordinates)))
        
        # Limit predictive raster values by surveyed area
        raster.probabilities.surveyed <- raster::mask(raster.probabilities,surveyed.area)
        
        # Determine ratio of knowingly occupied cells within surveyed area with total surveyed cells with predictive raster values >= average predictive values of knowingly occupied raster cells
        proportion.locations.surveyed <- nrow(sp::coordinates(known.coordinates)) / length(raster.probabilities.surveyed[raster.probabilities.surveyed >= mean.probabilities])
        
        # Limit predictive raster values by unsurveyed area
        raster.probabilities.unsurveyed <- raster::mask(raster.probabilities,unsurveyed.area)
        
        # Multiply ratio of knowingly occupied cells in surveyed area with number of cells in unsurveyed area with >= average predictive values of knowingly occupied raster cells
        n.probable.sites.unsurveyed <- length(raster.probabilities.unsurveyed[raster.probabilities.unsurveyed >= mean.probabilities]) * proportion.locations.surveyed
        
        # Multiply predicted number of unrecorded sites with given number of pitstructures by average number of households occupied for recorded sites with the given number of pitstructures within the given year
        extrapolated.households <- round(n.probable.sites.unsurveyed * mean(sites.by.pitstructure[,years]))
        
        # Export extrapolated number of households
        return(extrapolated.households)
        
      }
      
    }
    
    # Stop parallel computing environment
    parallel::stopCluster(clusters)
    
    # Write extrapolated results for small sites to column representing the given year
    total.extrapolated.households[,years-8] <- extrapolated.households
    
    # Remove used layers and clean workspace to save memory where possible
    rm(layer.7,layer.8,layer.9,raster.stack,raster.probabilities,mean.probabilities,raster.probabilities.surveyed,proportion.locations.surveyed,raster.probabilities.unsurveyed)
    gc()
    
  }
  
  # Save number of extrapolated households by pitstructure for given year
  write.csv(total.extrapolated.households,file='/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/extrapolated-population-by-household.csv',row.names=as.character(c(1:8)))
  
}

##########################################################################################
##########################################################################################
