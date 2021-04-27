##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## PREDICTIVE MODEL - OPTIMAL MODEL PARAMETERS FOR COMMUNITY CENTERS ##
##########################################################################################
##########################################################################################
## Define working directory and universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.cc.1

##########################################################################################
## Identify optimal model parameters for Community Cetner sites based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc','/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal.cc <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
## Load optimal artificial neural network as trained during model iterations
ann.model <- readRDS(paste('../model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc','/models/artificial-neural-network-',node.optimal,'.rds',sep=''))
saveRDS(ann.model,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/cc-artificial-neural-network.rds')

##########################################################################################
##########################################################################################
## Load dataset with ceramic assemblage, but without absolute site dating, and combine columns with overlapping typologies
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-cc-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):paste('X',as.character(year.end),sep='')))

# Define columns with variables for consideration by the artifical neural network
variable.columns <- length(2:which(colnames(dataset) == paste('X',year.start,sep='')))

##########################################################################################
## Identify all VEP II sites in Community Centers with/without tree-ring information, ceramics information, and remove VEP II sites within Mesa Verde North Escarpment study area

# Create spatialPoints from VEP II database and convert to master.projection
vepii.coords <- base::matrix(NA,nrow=nrow(vepii.database),ncol=2)
vepii.coords[,1] <- vepii.database$UTMEast
vepii.coords[,2] <- vepii.database$UTMNorth
vepii.coords <- sp::SpatialPointsDataFrame(coords=vepii.coords,vepii.database,proj4string=nad27.projection)
vepii.database.coords <- sp::spTransform(vepii.coords,master.projection)

vepii.coords <- vepii.database.coords[which(vepii.database.coords$Center == 1 ),]

# Remove all VEP II sites within Mesa Verde North Escarpment boundary
vepii.coords <- spatialEco::erase.point(vepii.coords,mvnes.survey,inside=TRUE)

# Define all VEP II sites with known tree-ring dates
vepii.known.dates <- as.data.frame(vepii.coords)[(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(tree.rings.aggregated)$SITE_ID),]
vepii.known.dates.limited <- tree.rings.aggregated[tree.rings.aggregated$SITE_ID %in% vepii.known.dates$SITE_ID,]
vepii.known.dates.limited[vepii.known.dates.limited == 0] <- NA
vepii.known.dates.limited <- vepii.known.dates.limited[which(rowMeans(vepii.known.dates.limited[,8:ncol(vepii.known.dates.limited)],na.rm=T) == 1),1]

# Remove all VEP II sites with known tree-ring dates
vepii.unknown.dates <- as.data.frame(vepii.coords)[!(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(tree.rings.aggregated)$SITE_ID),]
vepii.known.dates.limited <-  as.data.frame(vepii.coords)[as.data.frame(vepii.coords)$SITE_ID %in% vepii.known.dates.limited,]
vepii.unknown.dates <- rbind(vepii.unknown.dates,vepii.known.dates.limited)

# Identify all VEP II sites with unknown tree ring dates but with ceramic data using SITE_ID, and merge with ceramic tally information
vepii.known.ceramics <- vepii.unknown.dates[vepii.unknown.dates$SITE_ID %in% ceramics.tally$SITE_ID,]
vepii.known.ceramics.tally <- base::merge(vepii.known.ceramics,ceramics.tally,by='SITE_ID')

# Identify all VEP II sites with unknown tree ring dates and without ceramic data using SITE_ID
vepii.unknown.ceramics <- vepii.unknown.dates[!vepii.unknown.dates$SITE_ID %in% ceramics.tally$SITE_ID,]

# Combine columns of ceramics with overlapping typologies 
vepii.known.ceramics.tally$BW_MANCOS <- dplyr::coalesce(vepii.known.ceramics.tally$BW_MANCOS,vepii.known.ceramics.tally$BW_WETHERILL)
vepii.known.ceramics.data <- base::as.data.frame(vepii.known.ceramics.tally %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Identify VEP II sites with and without diagnostic ceramic information
vepii.diagnostic.ceramics <- vepii.known.ceramics.data[rowSums(vepii.known.ceramics.data[,2:ncol(vepii.known.ceramics.data)]) != 0 , ]
vepii.nondiagnostic.ceramics <- vepii.known.ceramics.data[rowSums(vepii.known.ceramics.data[,2:ncol(vepii.known.ceramics.data)]) == 0 , ]

# Combine VEP II non-diagnostic ceramic sites with sites without ceramic tally information
vepii.nondiagnostic.ceramics.info <- vepii.database[vepii.database$SITE_ID %in% vepii.nondiagnostic.ceramics$SITE_ID,]
vepii.unknowns <- rbind(vepii.unknown.ceramics[1:(ncol(vepii.unknown.ceramics)-2)],vepii.nondiagnostic.ceramics.info)

# Create spatialPoints of unknown tree-ring and unknown ceramics sites
vepii.unknowns.coords <- base::matrix(NA,nrow=nrow(vepii.unknowns),ncol=2)
vepii.unknowns.coords[,1] <- vepii.unknowns$UTMEast
vepii.unknowns.coords[,2] <- vepii.unknowns$UTMNorth
vepii.unknowns.coords <- sp::SpatialPointsDataFrame(coords=vepii.unknowns.coords,vepii.unknowns,proj4string=nad27.projection)
vepii.unknowns.coords <- sp::spTransform(vepii.unknowns.coords,master.projection)

##########################################################################################
##########################################################################################
## Predict Community Center occupation for sites with ceramic tally information and without tree-ring information
test.set.original <- vepii.diagnostic.ceramics

# Load ideal neural network model for predicting Community Center sites
ann.model <- readRDS('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/cc-artificial-neural-network.rds')

# Normalize the ceramic data across rows - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to predict site occupations
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding maximum date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create matrix to export results from following for loop
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

# Limit predictions by maximum date ranges of ceramic production and redistribute predictions, if applicable
for(i in 1:nrow(test.set)) {
  
  if(predictions.SITE_ID[i,]$SITE_ID %in% vepii.known.dates.limited$SITE_ID == TRUE) {
    
    known.cutting.date <- tree.rings.aggregated[which(tree.rings.aggregated$SITE_ID == predictions.SITE_ID[i,]$SITE_ID),]
    known.cutting.date.matrix <- cbind(as.matrix(seq(year.start,year.end,year.duration)),t(as.matrix(known.cutting.date[,7:ncol(known.cutting.date)])))
    known.cutting.date.range <- (which(known.cutting.date.matrix[,2] > 0,arr.ind=TRUE))
    date.range.maximum.column <- known.cutting.date.range[length(known.cutting.date.range)]
    date.range.minimum.column <- (which(known.cutting.date.matrix[,2] > 0,arr.ind=TRUE))[1]
    predictions.SITE_ID[i,(date.range.minimum.column+variable.columns):(date.range.maximum.column+variable.columns)] <- 1
    predictions.SITE_ID[i,(variable.columns+1):(date.range.minimum.column+14)] <- 0
    
    site.predictions <- predictions.SITE_ID[i,]
    site.row <- test.set[i,2:variable.columns]
    limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
    type.names <- diagnostic.ceramics[c(limit.types),]
    
    date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
    date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
    out.of.date.range <- seq(year.start,year.end,year.duration)
    out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
    
    total.range.predicted <- predictions.SITE_ID[i,(which(colnames(predictions.SITE_ID) == paste('X',year.start,sep=''))):(which(colnames(predictions.SITE_ID) == paste('X',year.end,sep='')))]
    ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,sep='')]
    normalize.new.range <- normalize(ceramic.range.predicted)
    
    combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
    order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
    
    isolate.range.predictions <- unname(order.range.predictions[,2])
    ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
    
  }
  
  else{
    
    site.predictions <- predictions.SITE_ID[i,]
    site.row <- test.set[i,2:variable.columns]
    limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
    type.names <- diagnostic.ceramics[c(limit.types),]
    
    date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
    date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
    out.of.date.range <- seq(year.start,year.end,year.duration)
    out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
    
    total.range.predicted <- predictions.SITE_ID[i,(which(colnames(predictions.SITE_ID) == paste('X',year.start,sep=''))):(which(colnames(predictions.SITE_ID) == paste('X',year.end,sep='')))]
    ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,sep='')]
    normalize.new.range <- normalize(ceramic.range.predicted)
    
    combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
    order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
    
    isolate.range.predictions <- unname(order.range.predictions[,2])
    ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
    
  }
  
}
ceramic.informed.occupation <- as.data.frame(ceramic.informed.occupation.ranges.matrix)
names(ceramic.informed.occupation) <- c('SITE_ID',paste(seq(year.start,year.end,year.duration)))

# Combine predictions with corresponding site information
information <- as.data.frame(vepii.database.coords)[as.data.frame(vepii.database.coords)$SITE_ID %in% ceramic.informed.occupation$SITE_ID,]
vepii.information <- base::as.data.frame(information %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.information) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
cc.ceramics.sites <- base::merge(vepii.information,ceramic.informed.occupation,by='SITE_ID')

# Combine known occupation dates with corresponding site information
known.sites <- as.data.frame(vepii.database.coords)[(as.data.frame(vepii.database.coords)$SITE_ID %in% vepii.known.dates$SITE_ID),]
known.dataset.information <- base::as.data.frame(known.sites %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(known.dataset.information) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
cc.trees.sites <- base::merge(known.dataset.information,tree.rings.aggregated[,c(1,7:ncol(tree.rings.aggregated))],by='SITE_ID')
names(cc.trees.sites) <- names(cc.ceramics.sites)

##########################################################################################
# Import Mesa Verde North Escarpment ceramics dataset for predictions
mvne.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/Reese_2022-AA2.csv')
mvne.ceramics <- mvne.ceramics[which(mvne.ceramics$CENTER == 1 ),]
mvne.ceramics[is.na(mvne.ceramics)] <- 0

# Combine columns of ceramics with overlapping typologies 
mvne.ceramics.sum <- plyr::ddply(mvne.ceramics[,c(1,14:ncol(mvne.ceramics))],'SITE_ID',plyr::numcolwise(sum))
mvne.ceramics.sum$BW_MANCOS <- dplyr::coalesce(mvne.ceramics.sum$BW_MANCOS,mvne.ceramics.sum$BW_WETHERILL)
mvne.ceramics.data <- base::as.data.frame(mvne.ceramics.sum %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Rename ceramics dataset to re-use code chunks from Reese-JAS-ann-iterations.R for simplicity, and remove all rows with no diagnostic ceramics
mvne.diagnostic.ceramics <- mvne.ceramics.data[rowSums(mvne.ceramics.data[,2:ncol(mvne.ceramics.data)]) != 0 , ]
mvne.nondiagnostic.ceramics <- mvne.ceramics.data[rowSums(mvne.ceramics.data[,2:ncol(mvne.ceramics.data)]) == 0 , ]

test.set.original <- mvne.diagnostic.ceramics

# Normalize the ceramic data across rows - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to predict site occupations
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding maximum date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create matrix to export results from following for loop
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

# Limit predictions by maximum date ranges of ceramic production and redistribute predictions, if applicable
for(i in 1:nrow(test.set)) {
  
  site.predictions <- predictions.SITE_ID[i,]
  site.row <- test.set[i,2:variable.columns]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- diagnostic.ceramics[c(limit.types),]
  
  date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
  date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
  out.of.date.range <- seq(year.start,year.end,year.duration)
  out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
  
  total.range.predicted <- predictions.SITE_ID[i,(which(colnames(predictions.SITE_ID) == paste('X',year.start,sep=''))):(which(colnames(predictions.SITE_ID) == paste('X',year.end,sep='')))]
  ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,sep='')]
  normalize.new.range <- normalize(ceramic.range.predicted)
  
  combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
  order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
  
  isolate.range.predictions <- unname(order.range.predictions[,2])
  ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
  
}
ceramic.informed.occupation <- as.data.frame(ceramic.informed.occupation.ranges.matrix)
names(ceramic.informed.occupation) <- c('SITE_ID',paste(seq(year.start,year.end,year.duration)))

# Combine predictions with corresponding site information
mvnes.information <- base::as.data.frame(mvne.ceramics %>% dplyr::select('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))
cc.mvne.sites <- base::merge(mvnes.information,ceramic.informed.occupation,by='SITE_ID')

##########################################################################################
# Create spatialPoints from predicted and known site information
predicted.information <- rbind(cc.ceramics.sites,cc.trees.sites,cc.mvne.sites)
predicted.information.spatial <- base::matrix(NA,nrow=nrow(predicted.information),ncol=2)
predicted.information.spatial[,1] <- predicted.information$X_COORDS
predicted.information.spatial[,2] <- predicted.information$Y_COORDS
predicted.information.spatial <- sp::SpatialPointsDataFrame(coords=predicted.information.spatial,predicted.information,proj4string=master.projection)

# Combine sites with non-diagnostic ceramics and no ceramics
mvne.unknowns <- mvne.ceramics[mvne.ceramics$SITE_ID %in% mvne.nondiagnostic.ceramics$SITE_ID,]
mvne.unknowns <- base::as.data.frame(as.data.frame(mvne.unknowns) %>% dplyr::select('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))
vepii.unknowns <- base::as.data.frame(as.data.frame(vepii.unknowns.coords) %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.unknowns) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')

# Create spatialPoints of all sites with unknown dating information
unknowns <- rbind(vepii.unknowns,mvne.unknowns)
unknowns.spatial <- base::matrix(NA,nrow=nrow(unknowns),ncol=2)
unknowns.spatial[,1] <- unknowns$X_COORDS
unknowns.spatial[,2] <- unknowns$Y_COORDS
unknowns.spatial <- sp::SpatialPointsDataFrame(coords=unknowns.spatial,unknowns,proj4string=master.projection)

# Create matrix for following for loop
proximity.estimated.occupation <- base::matrix(NA,nrow=nrow(unknowns),ncol=ncol(predicted.information))

for(i in 1:nrow(unknowns)) {
  # Calculate Euclidean distance between each site with no ANN prediction and all sites with ANN prediction
  euclidean.distance <- as.matrix(raster::pointDistance(coordinates(unknowns.spatial)[i,],as.matrix(coordinates(predicted.information.spatial)),lonlat=F))
  euclidean.distance.siteid <- cbind(predicted.information.spatial$SITE_ID,euclidean.distance)
  # Sort sites with ANN prediction by Euclidean distance, from closest-to-farthest
  euclidean.distance.sorted <- as.matrix(euclidean.distance.siteid[order(euclidean.distance.siteid[,2]),])
  # Identify closest site with ANN prediction
  siteid <- euclidean.distance.sorted[1,1]
  # Assign dating information from site with ANN prediction to site with no ANN prediction
  dating.information <- predicted.information[which(predicted.information$SITE_ID == siteid),]
  dating.information <- base::colMeans(dating.information[,9:ncol(predicted.information)])
  proximity.information <- cbind(unknowns[i,1:8],t(as.matrix(dating.information)))
  proximity.estimated.occupation[i,] <- base::as.matrix(proximity.information)
}
proximity.estimated.occupation <- base::as.data.frame(proximity.estimated.occupation)
names(proximity.estimated.occupation) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))

cc.unknown.sites <- proximity.estimated.occupation

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## SMALL SITES
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

## Define working directory and universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.ss.1

##########################################################################################
## Identify optimal model parameters for small sites based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss','/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal.ss <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
## Load optimal artificial neural network as trained during model iterations
ann.model <- readRDS(paste('../model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss','/models/artificial-neural-network-',node.optimal,'.rds',sep=''))
saveRDS(ann.model,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/ss-artificial-neural-network.rds')

##########################################################################################
##########################################################################################
## Load dataset with ceramic assemblage, but without absolute site dating, and combine columns with overlapping typologies
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-ss-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):paste('X',as.character(year.end),sep='')))

# Define columns with variables for consideration by the artificial neural network
variable.columns <- length(2:which(colnames(dataset) == paste('X',year.start,sep='')))

##########################################################################################
## Identify all VEP II sites in Community Centers with/without tree-ring information, ceramics information, and remove VEP II sites within Mesa Verde North Escarpment study area

# Create spatialPoints from VEP II database and convert to master.projection
vepii.coords <- base::matrix(NA,nrow=nrow(vepii.database),ncol=2)
vepii.coords[,1] <- vepii.database$UTMEast
vepii.coords[,2] <- vepii.database$UTMNorth
vepii.coords <- sp::SpatialPointsDataFrame(coords=vepii.coords,vepii.database,proj4string=nad27.projection)
vepii.database.coords <- sp::spTransform(vepii.coords,master.projection)

vepii.coords <- vepii.database.coords[which(vepii.database.coords$Center == 0 ),]

# Remove all VEP II sites within Mesa Verde North Escarpment boundary
vepii.coords <- spatialEco::erase.point(vepii.coords,mvnes.survey,inside=TRUE)

# Define all VEP II sites with known tree-ring dates
vepii.known.dates <- as.data.frame(vepii.coords)[(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(tree.rings.aggregated)$SITE_ID),]

# Remove all VEP II sites with known tree-ring dates
vepii.unknown.dates <- as.data.frame(vepii.coords)[!(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(tree.rings.aggregated)$SITE_ID),]

# Identify all VEP II sites with unknown tree ring dates but with ceramic data using SITE_ID, and merge with ceramic tally information
vepii.known.ceramics <- vepii.unknown.dates[vepii.unknown.dates$SITE_ID %in% ceramics.tally$SITE_ID,]
vepii.known.ceramics.tally <- base::merge(vepii.known.ceramics,ceramics.tally,by='SITE_ID')

# Identify all VEP II sites with unknown tree ring dates and without ceramic tallies using SITE_ID
vepii.unknown.ceramics <- vepii.unknown.dates[!vepii.unknown.dates$SITE_ID %in% ceramics.tally$SITE_ID,]

# Combine columns of ceramics with overlapping typologies 
vepii.known.ceramics.tally$BW_MANCOS <- dplyr::coalesce(vepii.known.ceramics.tally$BW_MANCOS,vepii.known.ceramics.tally$BW_WETHERILL)
vepii.known.ceramics.data <- base::as.data.frame(vepii.known.ceramics.tally %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Identify VEP II sites with and without diagnostic ceramic information
vepii.diagnostic.ceramics <- vepii.known.ceramics.data[rowSums(vepii.known.ceramics.data[,2:ncol(vepii.known.ceramics.data)]) != 0 , ]
vepii.nondiagnostic.ceramics <- vepii.known.ceramics.data[rowSums(vepii.known.ceramics.data[,2:ncol(vepii.known.ceramics.data)]) == 0 , ]

# Define remaining sites with no ceramic information whatsoever
vepii.ceramics.availibility <- c(ceramics.tally$SITE_ID,ceramics.presence$SITE_ID)
vepii.unknowns.coords <- vepii.coords[!(vepii.coords$SITE_ID %in% vepii.ceramics.availibility),]

##########################################################################################
##########################################################################################
## Predict Community Center occupation for sites with ceramic tally information and without tree-ring information
test.set.original <- vepii.diagnostic.ceramics

# Load ideal neural network model for predicting Community Center sites
ann.model <- readRDS('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/ss-artificial-neural-network.rds')

# Normalize the ceramic data across rows - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to predict site occupations
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding maximum date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create matrix to export results from following for loop
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

# Limit predictions by maximum date ranges of ceramic production and redistribute predictions, if applicable
for(i in 1:nrow(test.set)) {
  
  site.predictions <- predictions.SITE_ID[i,]
  site.row <- test.set[i,2:variable.columns]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- diagnostic.ceramics[c(limit.types),]
  
  date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
  date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
  out.of.date.range <- seq(year.start,year.end,year.duration)
  out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
  
  total.range.predicted <- predictions.SITE_ID[i,(which(colnames(predictions.SITE_ID) == paste('X',year.start,sep=''))):(which(colnames(predictions.SITE_ID) == paste('X',year.end,sep='')))]
  ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,sep='')]
  normalize.new.range <- normalize(ceramic.range.predicted)
  
  combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
  order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
  
  isolate.range.predictions <- unname(order.range.predictions[,2])
  ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
  
}
ceramic.informed.occupation <- as.data.frame(ceramic.informed.occupation.ranges.matrix)
names(ceramic.informed.occupation) <- c('SITE_ID',paste(seq(year.start,year.end,year.duration)))

# Combine predictions with corresponding site information
information <- as.data.frame(vepii.database.coords)[as.data.frame(vepii.database.coords)$SITE_ID %in% ceramic.informed.occupation$SITE_ID,]
vepii.information <- base::as.data.frame(information %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.information) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
ss.ceramics.sites <- base::merge(vepii.information,ceramic.informed.occupation,by='SITE_ID')

# Combine known occupation dates with corresponding site information
known.sites <- as.data.frame(vepii.database.coords)[(as.data.frame(vepii.database.coords)$SITE_ID %in% vepii.known.dates$SITE_ID),]
known.dataset.information <- base::as.data.frame(known.sites %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(known.dataset.information) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
ss.trees.sites <- base::merge(known.dataset.information,tree.rings.aggregated[,c(1,7:ncol(tree.rings.aggregated))],by='SITE_ID')
names(ss.trees.sites) <- names(ss.ceramics.sites)

##########################################################################################
# Import Mesa Verde North Escarpment ceramics dataset for predictions
mvne.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/Reese_2022-AA2.csv')
mvne.ceramics <- mvne.ceramics[which(mvne.ceramics$CENTER == 0 ),]
mvne.ceramics[is.na(mvne.ceramics)] <- 0

# Combine columns of ceramics with overlapping typologies 
mvne.ceramics.sum <- plyr::ddply(mvne.ceramics[,c(1,14:ncol(mvne.ceramics))],'SITE_ID',plyr::numcolwise(sum))
mvne.ceramics.sum$BW_MANCOS <- dplyr::coalesce(mvne.ceramics.sum$BW_MANCOS,mvne.ceramics.sum$BW_WETHERILL)
mvne.ceramics.data <- base::as.data.frame(mvne.ceramics.sum %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Rename ceramics dataset to re-use code chunks from Reese-JAS-ann-iterations.R for simplicity, and remove all rows with no diagnostic ceramics
mvne.diagnostic.ceramics <- mvne.ceramics.data[rowSums(mvne.ceramics.data[,2:ncol(mvne.ceramics.data)]) != 0 , ]
mvne.nondiagnostic.ceramics <- mvne.ceramics.data[rowSums(mvne.ceramics.data[,2:ncol(mvne.ceramics.data)]) == 0 , ]

# Run artificial neural network model for known ceramic tallies
test.set.original <- mvne.diagnostic.ceramics

# Normalize the ceramic data across rows - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to predict site occupations
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding maximum date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create matrix to export results from following for loop
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

# Limit predictions by maximum date ranges of ceramic production and redistribute predictions, if applicable
for(i in 1:nrow(test.set)) {
  
  site.predictions <- predictions.SITE_ID[i,]
  site.row <- test.set[i,2:variable.columns]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- diagnostic.ceramics[c(limit.types),]
  
  date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
  date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
  out.of.date.range <- seq(year.start,year.end,year.duration)
  out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
  
  total.range.predicted <- predictions.SITE_ID[i,(which(colnames(predictions.SITE_ID) == paste('X',year.start,sep=''))):(which(colnames(predictions.SITE_ID) == paste('X',year.end,sep='')))]
  ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,sep='')]
  normalize.new.range <- normalize(ceramic.range.predicted)
  
  combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
  order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
  
  isolate.range.predictions <- unname(order.range.predictions[,2])
  ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
  
}
ceramic.informed.occupation <- as.data.frame(ceramic.informed.occupation.ranges.matrix)
names(ceramic.informed.occupation) <- c('SITE_ID',paste(seq(year.start,year.end,year.duration)))

# Combine predictions with corresponding site information
mvnes.information <- base::as.data.frame(mvne.ceramics %>% dplyr::select('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))
ss.mvne.sites <- base::merge(mvnes.information,ceramic.informed.occupation,by='SITE_ID')

##########################################################################################
# Create spatialPoints from predicted and known site information
predicted.information <- rbind(ss.ceramics.sites,ss.trees.sites,ss.mvne.sites)
predicted.information.spatial <- base::matrix(NA,nrow=nrow(predicted.information),ncol=2)
predicted.information.spatial[,1] <- predicted.information$X_COORDS
predicted.information.spatial[,2] <- predicted.information$Y_COORDS
predicted.information.spatial <- sp::SpatialPointsDataFrame(coords=predicted.information.spatial,predicted.information,proj4string=master.projection)

# Extract all known ceramic information from ceramic tallies and limit to those that do not include diagnostic wares
vepii.tally.information <- base::as.data.frame(vepii.known.ceramics.tally %>% dplyr::select('SITE_ID','GRAY_PLAIN':'BW_MESAVERDE'))
vepii.nondiagnostic.tally <- vepii.tally.information[vepii.tally.information$SITE_ID %in% vepii.nondiagnostic.ceramics$SITE_ID,]
vepii.nondiagnostic.tally$CORRUGATED_MESAVERDE <- dplyr::coalesce(vepii.nondiagnostic.tally$CORRUGATED_MESAVERDE,vepii.nondiagnostic.tally$CORRUGATED_HOVENWEEP)
vepii.nondiagnostic.tally$GRAY_MOCCASIN <- dplyr::coalesce(vepii.nondiagnostic.tally$GRAY_MOCCASIN,vepii.nondiagnostic.tally$GRAY_NECKBANDED)
vepii.nondiagnostic.tally <- base::as.data.frame(vepii.nondiagnostic.tally %>% dplyr::select(-'CORRUGATED_HOVENWEEP',-'GRAY_NECKBANDED'))

# Identify VEP II sites with presence/absence ceramic information but without a tally
vepii.presence <- ceramics.presence[ceramics.presence$SITE_ID %in% vepii.unknown.dates$SITE_ID,]
vepii.presence$CORRUGATED_MESAVERDE <- dplyr::coalesce(vepii.presence$CORRUGATED_MESAVERDE,vepii.presence$CORRUGATED_HOVENWEEP)
vepii.presence$GRAY_MOCCASIN <- dplyr::coalesce(vepii.presence$GRAY_MOCCASIN,vepii.presence$GRAY_NECKBANDED)
vepii.presence <- base::as.data.frame(vepii.presence %>% dplyr::select(-'CORRUGATED_HOVENWEEP',-'GRAY_NECKBANDED'))

# Combine overlapping typologies, limit to sites with non-diagnostic ceramics, and combine with VEP II non-diagnostic and presence ceramics sites
mvne.ceramics$GRAY_CHAPIN <- dplyr::coalesce(mvne.ceramics$GRAY_CHAPIN,mvne.ceramics$GRAY_FUGITIVE)
mvne.ceramics$CORRUGATED_MESAVERDE <- dplyr::coalesce(mvne.ceramics$CORRUGATED_MESAVERDE,mvne.ceramics$CORRUGATED_HOVENWEEP)
mvne.ceramics$GRAY_MOCCASIN <- dplyr::coalesce(mvne.ceramics$GRAY_MOCCASIN,mvne.ceramics$GRAY_NECKBANDED)
mvne.ceramics.collated <- base::as.data.frame(mvne.ceramics %>% dplyr::select(-'GRAY_FUGITIVE',-'CORRUGATED_HOVENWEEP',-'GRAY_NECKBANDED'))
mvne.ceramic.guides <- mvne.ceramics.collated[mvne.ceramics.collated$SITE_ID %in% mvne.nondiagnostic.ceramics$SITE_ID,c(1,14:ncol(mvne.ceramics.collated))]

# Combine all non-diagnostic and presence ceramic data
ceramic.guides <- rbind(vepii.nondiagnostic.tally,vepii.presence,mvne.ceramic.guides)
ceramic.guides.total <- ceramic.guides[which(rowSums(ceramic.guides[,2:ncol(ceramic.guides)]) > 0),]

# Import table of ceramic types and corresponding minimum date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
ranges.ceramics <- nsj.ceramics[,c('START','END')]
ranges.ceramics.names <- nsj.ceramics[,'TYPE']
rownames(ranges.ceramics) <- ranges.ceramics.names

# Create matrix to place tallies of type of ceramic wares
ceramics.spread <- matrix(0,nrow=nrow(ceramic.guides.total),ncol=(year.end-year.start+2))
ceramics.spread[,1] <- ceramic.guides.total$SITE_ID

for(c in 1:nrow(ceramic.guides.total)){
  
  site.row <- ceramic.guides.total[c,2:ncol(ceramic.guides.total)]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- ranges.ceramics[c(limit.types),]
  
  ceramics.proportions.distribution <- matrix(0,nrow=nrow(type.names),ncol=(year.end-year.start+1))
  
  for(y in 1:nrow(type.names)) {
    spread.start <- type.names[y,]$START - year.start + 1
    spread.end <- type.names[y,]$END - year.start + 1
    spread.tally <- site.row[,which(colnames(site.row) == rownames(type.names)[y])]
    spread.proportion <- spread.tally / (spread.end - spread.start)
    ceramics.proportions.distribution[y,spread.start:spread.end] <- spread.proportion
    
  }
  
  total.spread <- normalize(colSums(ceramics.proportions.distribution))
  row.spread.max <- max(total.spread)
  total.spread[total.spread < row.spread.max] <- 0
  ceramics.spread[c,2:ncol(ceramics.spread)] <- total.spread
  
}
ceramics.spread <- base::as.data.frame(ceramics.spread)
names(ceramics.spread) <- c('SITE_ID',as.character(seq(year.start,year.end,year.duration)))

vepii.unknown.dates.information <- base::as.data.frame(vepii.unknown.dates %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
vepii.unknown.dates.spatial <- base::matrix(NA,nrow=nrow(vepii.unknown.dates.information),ncol=2)
vepii.unknown.dates.spatial[,1] <- vepii.unknown.dates$coords.x1
vepii.unknown.dates.spatial[,2] <- vepii.unknown.dates$coords.x2
vepii.unknown.dates.spatial <- sp::SpatialPointsDataFrame(coords=vepii.unknown.dates.spatial,vepii.unknown.dates.information,proj4string=master.projection)
ceramics.spread.vepii <- vepii.unknown.dates.spatial[vepii.unknown.dates.spatial$SITE_ID %in% ceramics.spread$SITE_ID,]
names(ceramics.spread.vepii) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')

mvne.ceramics.information <- base::as.data.frame(mvne.ceramics %>% dplyr::select('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))
mvne.unknown.dates.spatial <- base::matrix(NA,nrow=nrow(mvne.ceramics.information),ncol=2)
mvne.unknown.dates.spatial[,1] <- mvne.ceramics.information$X_COORDS
mvne.unknown.dates.spatial[,2] <- mvne.ceramics.information$Y_COORDS
mvne.unknown.dates.spatial <- sp::SpatialPointsDataFrame(coords=mvne.unknown.dates.spatial,mvne.ceramics.information,proj4string=master.projection)
ceramics.spread.mvne <- mvne.unknown.dates.spatial[mvne.unknown.dates.spatial$SITE_ID %in% ceramics.spread$SITE_ID,]

ceramics.spread.spatial <- rbind(ceramics.spread.vepii,ceramics.spread.mvne)

neighbor.informed.occupation <- matrix(0,nrow=nrow(ceramics.spread),ncol=(year.end-year.start+2))
neighbor.informed.occupation[,1] <- ceramics.spread$SITE_ID

for(t in 1:nrow(ceramics.spread)) {
  
  site.spread <- ceramics.spread[t,2:ncol(ceramics.spread)]
  years.most.likely <- as.numeric(colnames(site.spread[,which(site.spread > 0)]))
  
  target <- ceramics.spread.spatial[which(ceramics.spread.spatial$SITE_ID == ceramics.spread[t,]$SITE_ID),]
  
  euclidean.distance <- as.matrix(raster::pointDistance(coordinates(target),as.matrix(coordinates(predicted.information.spatial)),lonlat=F))
  euclidean.distance.siteid <- cbind(predicted.information.spatial$SITE_ID,euclidean.distance)
  euclidean.distance.sorted <- unique(as.matrix(euclidean.distance.siteid[order(euclidean.distance.siteid[,2]),]))
  
  for(e in 1:nrow(euclidean.distance.sorted)) {
    
    siteid <- euclidean.distance.sorted[e,1]
    
    dating.information <- unique(predicted.information[which(predicted.information$SITE_ID == siteid),9:ncol(predicted.information)])
    dating.spread <- dating.information[1,]
    
    years.neighbor <- as.numeric(colnames(dating.spread[,which(dating.spread == max(dating.spread))]))

    stop <- FALSE
    
    if(length(years.neighbor) == 0) {next}
    
    tryCatch({
      
      mean(years.neighbor %in% years.most.likely) == 1 },
      
      error = function(e) { stop <<- TRUE})
    
    if(stop == TRUE) { next }
    
    if(mean(years.neighbor %in% years.most.likely) == 1) {
      
      neighbor.start <- years.neighbor[1] - year.start + 1
      neighbor.end <- years.neighbor[length(years.neighbor)] - year.start + 1
      
      neighbor.informed.occupation[t,neighbor.start:neighbor.end] <- 1
      
      stop <- TRUE
      
      if(stop == TRUE) {break}
      
    }
    
  }
  
}

neighbor.informed.occupation <- base::as.data.frame(neighbor.informed.occupation)
names(neighbor.informed.occupation) <- c('SITE_ID',as.character(seq(year.start,year.end,year.duration)))

occupation.sorted <- neighbor.informed.occupation[order(neighbor.informed.occupation$SITE_ID),]
information.sorted <- as.data.frame(ceramics.spread.spatial)[order(as.data.frame(ceramics.spread.spatial)$SITE_ID),]

proximity.predictions <- merge(information.sorted,occupation.sorted,by='SITE_ID')
ss.ceramic.informed.sites <- base::as.data.frame(as.data.frame(proximity.predictions) %>% dplyr::select('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(year.start):as.character(year.end)))
ss.ceramic.informed.sites <- ss.ceramic.informed.sites[which(rowSums(ss.ceramic.informed.sites[,9:ncol(ss.ceramic.informed.sites)]) > 0 ),]

##########################################################################################
## Identify all sites with no ceramic information and save to add later

# Combine sites with non-diagnostic ceramics and no ceramics
vepii.unknowns <- base::as.data.frame(as.data.frame(vepii.unknowns.coords) %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
no.ceramics.data <- ceramic.guides[which(rowSums(ceramic.guides[,2:ncol(ceramic.guides)]) == 0),]
no.ceramics.data.information <- vepii.coords[vepii.coords$SITE_ID %in% no.ceramics.data$SITE_ID,]
no.ceramics.data.information <- base::as.data.frame(as.data.frame(no.ceramics.data.information) %>% dplyr::select('SITE_ID','SITE_NO','Center','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))

# Combine with sites for which no matches occurred in the ceramic informed analysis, above
no.ss.ceramic.informed.sites <- ss.ceramic.informed.sites[which(rowSums(ss.ceramic.informed.sites[,9:ncol(ss.ceramic.informed.sites)]) == 0 ),]

vepii.unknowns <- rbind(vepii.unknowns,no.ceramics.data.information)
names(vepii.unknowns) <- c('SITE_ID','SITE_NO','CENTER','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
ss.unknown.sites <- rbind(vepii.unknowns,no.ss.ceramic.informed.sites[,1:8])

utils::write.csv(ss.unknown.sites,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-unknowns.csv',row.names=F)

##########################################################################################

region.occupation.predictions.total <- rbind(cc.ceramics.sites,ss.ceramics.sites,cc.trees.sites,ss.trees.sites,cc.mvne.sites,ss.mvne.sites,cc.unknown.sites,ss.ceramic.informed.sites)
utils::write.csv(region.occupation.predictions.total,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-region-complete.csv',row.names=F)

##########################################################################################
##########################################################################################
