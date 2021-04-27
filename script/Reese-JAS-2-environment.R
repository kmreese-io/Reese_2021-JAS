##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## ENVIRONMENT ##
##########################################################################################
##########################################################################################
## Environment universals
master.projection <- sp::CRS('+proj=utm +datum=NAD83 +zone=12')
nad27.projection <- sp::CRS('+proj=utm +datum=NAD27 +zone=12')
longlat.projection <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')
colors <- grDevices::colorRampPalette(c('black','white'))(10000)
heatcolors <- rev(grDevices::colorRampPalette(c(as.character(RColorBrewer::brewer.pal(11,'Spectral'))))(10000))

##########################################################################################
## Spatial information - study area
region.dem <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-dem.grd')
region.hillshade <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-hillshade.grd')
study.area.extent <- polygonUTM_NAD83(raster::xmin(region.dem),raster::xmax(region.dem),raster::ymin(region.dem),raster::ymax(region.dem),12)
study.area.subregions <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-regional/',layer='vepii-n-subregions')
raster::projection(study.area.subregions) <- master.projection

##########################################################################################
## Spatial information - boundaries
state.boundaries <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/states-united-states/',layer='us-state-boundaries')
raster::projection(state.boundaries) <- master.projection
mvnp <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-national-parks/',layer='mv-np-boundary')
raster::projection(mvnp) <- master.projection
federal.land <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/ownership-federal/',layer='federal-land-boundaries')
raster::projection(federal.land) <- master.projection
canm <- federal.land[which(federal.land$ORIG_NAME == 'CANYONS OF THE ANCIENTS NATIONAL MONUMENT'),]
vepii.survey <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-survey/',layer='vepii-previous-coverage')
raster::projection(vepii.survey) <- master.projection

##########################################################################################
## Cultural information - recorded site data
mvnes.survey <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/NORTH-ESCARPMENT/boundaries-survey-extent/',layer='ne-survey-pedestrian-coverage')
raster::projection(mvnes.survey) <- master.projection
mvnes.sites <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/NORTH-ESCARPMENT/mvnes-sites/',layer='ne-sites')
raster::projection(mvnes.sites) <- master.projection

##########################################################################################
## Cultural information - VEP II database
vepii.database <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-n-results.csv')
vepii.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-tree-rings.csv')

##########################################################################################
## Survey coverage and proportion of surveyed/unsurveyed areas
survey.vepii <- raster::mask(region.dem,vepii.survey)
survey.mvnes <- raster::mask(region.dem,mvnes.survey)
surveyed.area <- raster::merge(survey.vepii,survey.mvnes)
raster::writeRaster(surveyed.area,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-surveyed',overwrite=T)
unsurveyed.area <- raster::mask(region.dem,surveyed.area,inverse=T)
raster::writeRaster(unsurveyed.area,'/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-unsurveyed',overwrite=T)

n.raster.cells.surveyed <- ncell(region.dem) - cellStats(surveyed.area,'countNA')
n.raster.cells.unsurveyed <- ncell(region.dem) - n.raster.cells.surveyed
# n.raster.cells.surveyed <- 11606270
# n.raster.cells.unsurveyed <- 42563140

##########################################################################################
##########################################################################################
