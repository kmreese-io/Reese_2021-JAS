##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## MASTER ##

## R PACKAGES
packages <-c('RColorBrewer','sp','grDevices','tibble','dplyr','tidyr','caTools','caret','neuralnet','doParallel','spatstat','smoother')
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

## PROJECT DIRECTORY
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-households-density/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-households/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-all/',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-all-density/',recursive=T,showWarnings=F)

## FUNCTIONS
base::source('/Users/kmreese/Documents/PROJECTS/SOURCE/FUNCTIONS/normalize.R')
base::source('/Users/kmreese/Documents/PROJECTS/SOURCE/FUNCTIONS/polygonUTM_NAD83.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-ann-iterations.R')

## ENVIRONMENT
master.projection <- sp::CRS('+proj=utm +datum=NAD83 +zone=12')
nad27.projection <- sp::CRS('+proj=utm +datum=NAD27 +zone=12')
longlat.projection <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')
colors <- grDevices::colorRampPalette(c('black','white'))(10000)
heatcolors <- rev(grDevices::colorRampPalette(c(as.character(RColorBrewer::brewer.pal(11,'Spectral'))))(10000))

## SPATIAL AND CULTURAL DATASETS
region.dem <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-dem.grd')
region.hillshade <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-hillshade.grd')
study.area.extent <- polygonUTM_NAD83(raster::xmin(region.dem),raster::xmax(region.dem),raster::ymin(region.dem),raster::ymax(region.dem),12)
state.boundaries <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/states-united-states/',layer='us-state-boundaries')
raster::projection(state.boundaries) <- master.projection
mvnp <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-national-parks/',layer='mv-np-boundary')
raster::projection(mvnp) <- master.projection
federal.land <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/ownership-federal/',layer='federal-land-boundaries')
raster::projection(federal.land) <- master.projection
canm <- federal.land[which(federal.land$ORIG_NAME == 'CANYONS OF THE ANCIENTS NATIONAL MONUMENT'),]
mvnes.boundary <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/BOUNDARIES/boundaries-survey/',layer='mvnes-total-survey-access-area')
raster::projection(mvnes.boundary) <- master.projection
mvnes.survey <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/NORTH-ESCARPMENT/boundaries-survey-extent/',layer='ne-survey-pedestrian-coverage')
raster::projection(mvnes.survey) <- master.projection
mvnes.sites <- rgdal::readOGR('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/SHAPEFILES/NORTH-ESCARPMENT/mvnes-sites/',layer='ne-sites')
raster::projection(mvnes.sites) <- master.projection

vepii.database <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-n-results.csv')
vepii.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-tree-rings.csv')

## SCRIPT
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-dataset-preparation.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-model-iterations.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-model-final.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-results.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-figures.R')
