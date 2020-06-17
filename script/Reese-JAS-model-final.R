##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## FINAL MODEL ##

setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-mean/accuracy-ceramics-smoothed-mean.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,3:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):paste('X',as.character(year.end),sep='')))
training.set.original <- dataset

training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
variable.columns <- length(2:which(colnames(training.set.original) == paste('X',year.start,sep='')))
training.set.original <- training.set.original[rowSums(training.set.original[,2:variable.columns]) != 0 , ]

# Normalize the ceramic data across rows in both training set and test set - normalizing by SITE_ID
training.normalized.matrix <- matrix(NA,nrow=nrow(training.set.original),ncol=(variable.columns-1))
for(i in 1:nrow(training.set.original)) {
  normalized.row <- as.matrix(normalize(training.set.original[i,2:(variable.columns)]))
  training.normalized.matrix[i,] <- normalized.row
}

training.set <- as.data.frame(cbind(training.set.original[,1],unlist(training.normalized.matrix[,1:(variable.columns-1)]),training.set.original[(variable.columns+1):ncol(training.set.original)]))
colnames(training.set) <- c(base::paste(as.character(colnames(dataset))))

# Replace any NaNs with zeros
training.set[is.na(training.set)] <- 0

# Define independent and dependent variables
independent <- base::paste(as.character(colnames(dataset[2:variable.columns])))
dependent <- base::paste(as.character(colnames(dataset[(variable.columns+1):ncol(dataset)])))

# Create formula
ann.formula <- stats::as.formula(paste(paste(dependent,collapse='+'),' ~ ',paste(independent,collapse='+')))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

ann.model <- neuralnet::neuralnet(formula = ann.formula,     
                                  data = training.set,           
                                  threshold = 0.0000001,
                                  linear.output = F,
                                  hidden = node.optimal,
                                  stepmax = 100000000,
                                  rep = 10
)

# End parallel computing
parallel::stopCluster(clusters)

saveRDS(ann.model,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/artificial-neural-network.rds')

vepii.ceramics <- ceramics.aggregated
vepii.ceramics.coords <- base::merge(vepii.ceramics,vepii.database,by='SITE_ID')
vepii.coords <- base::matrix(NA,nrow=nrow(vepii.ceramics.coords),ncol=2)
vepii.coords[,1] <- vepii.ceramics.coords$UTMEast
vepii.coords[,2] <- vepii.ceramics.coords$UTMNorth
vepii.coords <- sp::SpatialPointsDataFrame(coords=vepii.coords,vepii.ceramics.coords,proj4string=nad27.projection)
vepii.coords <- sp::spTransform(vepii.coords,master.projection)
vepii.mvnes.coords <- vepii.coords[mvnes.survey,]
vepii <- as.data.frame(vepii.coords)[!(as.data.frame(vepii.coords)$SITE_ID %in% as.data.frame(vepii.mvnes.coords)$SITE_ID),]

vepii$BW_MANCOS <- dplyr::coalesce(vepii$BW_MANCOS,vepii$BW_WETHERILL)
vepii.ceramics.data <- base::as.data.frame(vepii %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))
test.set.original <- vepii.ceramics.data

variable.columns <- length(2:which(colnames(training.set.original) == paste('X',year.start,sep='')))

test.set.original <- test.set.original[rowSums(test.set.original[,2:ncol(test.set.original)]) != 0 , ]

# Normalize the ceramic data across rows in both training set and test set - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}

test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to calculate site predictions and combine with known cutting dates
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

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
vepii.information <- base::as.data.frame(vepii %>% dplyr::select('SITE_ID','SITE_NO','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.information) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')

vepii.occupation.predictions <- tibble::as_tibble(base::merge(vepii.information,ceramic.informed.occupation,by='SITE_ID'))
utils::write.csv(vepii.occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/vepii-occupation-predictions.csv',row.names=F)

mvne.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/Reese-AA.csv')
mvne.ceramics[is.na(mvne.ceramics)] <- 0

mvne.ceramics.sum <- plyr::ddply(mvne.ceramics[,c(1,13:ncol(mvne.ceramics))],'SITE_ID',plyr::numcolwise(sum))
mvne.ceramics.sum$BW_MANCOS <- dplyr::coalesce(mvne.ceramics.sum$BW_MANCOS,mvne.ceramics.sum$BW_WETHERILL)
mvne.ceramics.data <- base::as.data.frame(mvne.ceramics.sum %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE'))

test.set.original <- mvne.ceramics.data
test.set.original[is.na(test.set.original)] <- 0

test.set.original <- test.set.original[rowSums(test.set.original[,2:ncol(test.set.original)]) != 0 , ]

# Normalize the ceramic data across rows in both training set and test set - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(ncol(test.set.original)-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:ncol(test.set.original)]))
  test.normalized.matrix[i,] <- normalized.row
}

test.set <- as.data.frame(cbind(test.set.original$SITE_ID,test.normalized.matrix))
colnames(test.set) <- c(base::paste(as.character(colnames(test.set.original))))

# Use neural network model to calculate site predictions and combine with known cutting dates
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set.original$SITE_ID),as.matrix(test.set.original[,2:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(dataset)

# Import table of diagnostic ceramic types and corresponding date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)

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
mvnes.information <- base::as.data.frame(mvne.ceramics %>% dplyr::select('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS'))

mvnes.occupation.predictions <- tibble::as_tibble(base::merge(mvnes.information,ceramic.informed.occupation,by='SITE_ID'))
utils::write.csv(mvnes.occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/mvnes-occupation-predictions.csv',row.names=F)

vepii.known.sites <- merge(as.data.frame(vepii.coords),dataset,by='SITE_ID')
vepii.known.dates <- base::as.data.frame(vepii.known.sites %>% dplyr::select(paste('X',as.character(year.start),sep=''):paste('X',as.character(year.end),sep='')))
vepii.known.dates[vepii.known.dates > 0 ] <- 1
vepii.known.information <- base::as.data.frame(vepii.known.sites %>% dplyr::select('SITE_ID','SITE_NO','recordtypedesc','roomblock.area..m2.','TOTPITST','coords.x1','coords.x2'))
names(vepii.known.information) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
vepii.occupation.known <- cbind(vepii.known.information,vepii.known.dates)

vepii.occupation.known <- base::as.data.frame(vepii.occupation.known)
vepii.occupation.predictions <- base::as.data.frame(vepii.occupation.predictions)
mvnes.occupation.predictions <- base::as.data.frame(mvnes.occupation.predictions)

names(vepii.occupation.known) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
names(vepii.occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')
names(mvnes.occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS')

occupation.predictions <- base::rbind(vepii.occupation.known,vepii.occupation.predictions,mvnes.occupation.predictions)
names(occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))

utils::write.csv(occupation.predictions,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-predictions.csv',row.names=F)

#########################################################################################
## OCCUPATION PREDICTION BASED ON SITE PROXIMITY FOR THOSE WITH NO CERAMICS DATA ##
#########################################################################################

sites.no.ceramics <- base::subset(vepii.database,!(vepii.database$SITE_ID %in% occupation.predictions$SITE_ID))
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

known.coords <- base::matrix(NA,nrow=nrow(occupation.predictions),ncol=2)
known.coords[,1] <- occupation.predictions$X_COORDS
known.coords[,2] <- occupation.predictions$Y_COORDS
yes.ceramics.sp <- sp::SpatialPointsDataFrame(coords=known.coords,occupation.predictions,proj4string=master.projection)
yes.ceramics.df <- base::as.data.frame(yes.ceramics.sp)

proximity.estimated.occupation <- base::matrix(NA,nrow=nrow(no.ceramics),ncol=ncol(occupation.predictions))

for(i in 1:nrow(no.ceramics.df)) {
  euclidean.distance <- as.matrix(raster::pointDistance(coordinates(no.ceramics.sp)[i,],as.matrix(coordinates(yes.ceramics.sp)),lonlat=F))
  euclidean.distance.siteid <- cbind(yes.ceramics.sp$SITE_ID,euclidean.distance)
  euclidean.distance.sorted <- as.matrix(euclidean.distance.siteid[order(euclidean.distance.siteid[,2]),])
  siteid <- euclidean.distance.sorted[1,1]
  dating.information <- yes.ceramics.df[which(yes.ceramics.df$SITE_ID == siteid),]
  dating.information <- base::colMeans(dating.information[,8:ncol(occupation.predictions)])
  
  
  proximity.information <- cbind(no.ceramics.df[i,1:7],t(as.matrix(dating.information)))
  
  proximity.estimated.occupation[i,] <- base::as.matrix(proximity.information)
}

proximity.estimated.occupation <- base::as.data.frame(proximity.estimated.occupation)
names(proximity.estimated.occupation) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))
names(occupation.predictions) <- c('SITE_ID','SITE_NO','PRIMARY','ROOMBLOCK_AREA','PITSTRUCTURES','X_COORDS','Y_COORDS',as.character(seq(year.start,year.end,year.duration)))

occupation.predictions.total <- rbind(occupation.predictions,proximity.estimated.occupation)
utils::write.csv(occupation.predictions.total,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-predictions-total.csv',row.names=F)
