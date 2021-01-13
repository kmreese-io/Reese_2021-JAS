##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## PREDICTIVE MODEL - OPTIMAL MODEL PARAMETERS ##
##########################################################################################
##########################################################################################
## Define working directory and universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

##########################################################################################
## Identify optimal model parameters based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
## Load optimal artificial neural network as trained during model iterations
ann.model <- readRDS(paste('../model-iterations/',year.start,'-',year.end,'x',year.duration,'/models/artificial-neural-network-',node.optimal,'.rds',sep=''))
saveRDS(ann.model,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/artificial-neural-network.rds')
ann.model <- readRDS('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/artificial-neural-network.rds')

##########################################################################################
## Load dataset with ceramic assemblage, but without absolute site dating, and combine columns with overlapping typologies
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):paste('X',as.character(year.end),sep='')))

# Define columns with variables for consideration by the artifical neural network
variable.columns <- length(2:which(colnames(dataset) == paste('X',year.start,sep='')))

##########################################################################################
##########################################################################################
## Identify all VEP II sites with ceramics information and remove those within Mesa Verde North Escarpment study area
vepii.ceramics <- ceramics.aggregated
vepii.ceramics.coords <- base::merge(vepii.ceramics,vepii.database,by='SITE_ID')
vepii.coords <- base::matrix(NA,nrow=nrow(vepii.ceramics.coords),ncol=2)
vepii.coords[,1] <- vepii.ceramics.coords$UTMEast
vepii.coords[,2] <- vepii.ceramics.coords$UTMNorth
vepii.coords <- sp::SpatialPointsDataFrame(coords=vepii.coords,vepii.ceramics.coords,proj4string=nad27.projection)
vepii.coords <- sp::spTransform(vepii.coords,master.projection)
vepii.mvnes.coords <- vepii.coords[mvnes.survey,]
vepii <- as.data.frame(vepii.coords)[!(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(vepii.mvnes.coords)$SITE_ID),]

# Combine columns of ceramics with overlapping typologies 
vepii$BW_MANCOS <- dplyr::coalesce(vepii$BW_MANCOS,vepii$BW_WETHERILL)
vepii.ceramics.data <- base::as.data.frame(vepii %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Rename ceramics dataset to re-use code chunks from Reese-JAS-ann-iterations.R for simplicity, and remove all rows with no diagnostic ceramics
test.set.original <- vepii.ceramics.data
test.set.original <- test.set.original[rowSums(test.set.original[,2:ncol(test.set.original)]) != 0 , ]

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
vepii.information <- base::as.data.frame(vepii %>% dplyr::select('SITE_ID','SITE_NO','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.information) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
vepii.occupation.predictions <- tibble::as_tibble(base::merge(vepii.information,ceramic.informed.occupation,by='SITE_ID'))

# Save prediction results for VEP II sites (excluding those within the Mesa Verde North Escarpment dataset)
utils::write.csv(vepii.occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-vepii.csv',row.names=F)

##########################################################################################
##########################################################################################
## Import Mesa Verde North Escarpment ceramics information
mvne.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/Reese-AA.csv')
mvne.ceramics[is.na(mvne.ceramics)] <- 0

# Combine columns of ceramics with overlapping typologies 
mvne.ceramics.sum <- plyr::ddply(mvne.ceramics[,c(1,13:ncol(mvne.ceramics))],'SITE_ID',plyr::numcolwise(sum))
mvne.ceramics.sum$BW_MANCOS <- dplyr::coalesce(mvne.ceramics.sum$BW_MANCOS,mvne.ceramics.sum$BW_WETHERILL)
mvne.ceramics.data <- base::as.data.frame(mvne.ceramics.sum %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

# Rename ceramics dataset to re-use code chunks from Reese-JAS-ann-iterations.R for simplicity, and remove all rows with no diagnostic ceramics
test.set.original <- mvne.ceramics.data
test.set.original[is.na(test.set.original)] <- 0
test.set.original <- test.set.original[rowSums(test.set.original[,2:ncol(test.set.original)]) != 0 , ]

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
mvnes.information <- base::as.data.frame(mvne.ceramics %>% dplyr::select('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))
mvnes.occupation.predictions <- tibble::as_tibble(base::merge(mvnes.information,ceramic.informed.occupation,by='SITE_ID'))

# Save prediction results for Mesa Verde North Escarpment sites
utils::write.csv(mvnes.occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-mvnes.csv',row.names=F)

##########################################################################################
##########################################################################################
## Combine VEP II predictions with predictions for Mesa Verde North Escarpment
region.occupation.predictions <- base::rbind(vepii.occupation.predictions,mvnes.occupation.predictions)
names(region.occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))

# Save prediction results for all recorded sites within central Mesa Verde region with known ceramic assemblage
utils::write.csv(region.occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-region.csv',row.names=F)

##########################################################################################
##########################################################################################
## Predict occupations of sites with no recorded ceramic assemblages, or those with no diagnostic wares, based on closest Euclidean distances

# Identify sites within VEP II database with no known ceramic assemblages, or no diagnostic ceramic wares, and convert to SpatialPointsDataFrame (site with no ANN prediction)
sites.no.ceramics <- base::subset(vepii.database,!(vepii.database$SITE_ID %in% region.occupation.predictions$SITE_ID))
no.ceramics.coords.nad27 <- base::matrix(NA,nrow=nrow(sites.no.ceramics),ncol=2)
no.ceramics.coords.nad27[,1] <- sites.no.ceramics$UTMEast
no.ceramics.coords.nad27[,2] <- sites.no.ceramics$UTMNorth
no.ceramics.coords.nad27 <- sp::SpatialPointsDataFrame(coords=no.ceramics.coords.nad27,sites.no.ceramics,proj4string=nad27.projection)
no.ceramics.coords.nad83 <- as.data.frame(sp::spTransform(no.ceramics.coords.nad27,master.projection))
no.ceramics <- base::as.data.frame(no.ceramics.coords.nad83 %>% dplyr::select('SITE_ID','SITE_NO','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(no.ceramics) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
no.ceramics.coords <- base::matrix(NA,nrow=nrow(no.ceramics),ncol=2)
no.ceramics.coords[,1] <- no.ceramics$X_COORDS
no.ceramics.coords[,2] <- no.ceramics$Y_COORDS
no.ceramics.sp <- sp::SpatialPointsDataFrame(coords=no.ceramics.coords,no.ceramics,proj4string=master.projection)
no.ceramics.df <- base::as.data.frame(no.ceramics.sp)

# Convert sites with predictions based on ceramic assemblages to SpatialPointsDataFrame (sites with ANN prediction)
known.coords <- base::matrix(NA,nrow=nrow(region.occupation.predictions),ncol=2)
known.coords[,1] <- region.occupation.predictions$X_COORDS
known.coords[,2] <- region.occupation.predictions$Y_COORDS
yes.ceramics.sp <- sp::SpatialPointsDataFrame(coords=known.coords,region.occupation.predictions,proj4string=master.projection)
yes.ceramics.df <- base::as.data.frame(yes.ceramics.sp)

# Create matrix for collowing for loop
proximity.estimated.occupation <- base::matrix(NA,nrow=nrow(no.ceramics),ncol=ncol(region.occupation.predictions))

for(i in 1:nrow(no.ceramics.df)) {
  # Calculate Euclidean distance between each site with no ANN prediction and all sites with ANN prediction
  euclidean.distance <- as.matrix(raster::pointDistance(coordinates(no.ceramics.sp)[i,],as.matrix(coordinates(yes.ceramics.sp)),lonlat=F))
  euclidean.distance.siteid <- cbind(yes.ceramics.sp$SITE_ID,euclidean.distance)
  # Sort sites with ANN prediction by Euclidean distance, from closest-to-farthest
  euclidean.distance.sorted <- as.matrix(euclidean.distance.siteid[order(euclidean.distance.siteid[,2]),])
  # Identify closest site with ANN prediction
  siteid <- euclidean.distance.sorted[1,1]
  # Assign dating information from site with ANN prediction to site with no ANN prediction
  dating.information <- yes.ceramics.df[which(yes.ceramics.df$SITE_ID == siteid),]
  dating.information <- base::colMeans(dating.information[,8:ncol(region.occupation.predictions)])
  proximity.information <- cbind(no.ceramics.df[i,1:7],t(as.matrix(dating.information)))
  proximity.estimated.occupation[i,] <- base::as.matrix(proximity.information)
}
proximity.estimated.occupation <- base::as.data.frame(proximity.estimated.occupation)
names(proximity.estimated.occupation) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))
names(region.occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))

# Combine results of sites with no ANN prediction and all sites with ANN prediction
region.occupation.predictions.total <- rbind(region.occupation.predictions,proximity.estimated.occupation)

# Save the aggregated site occupation prediction results
utils::write.csv(region.occupation.predictions.total,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-region-complete.csv',row.names=F)

##########################################################################################
##########################################################################################
