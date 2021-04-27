##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## DATASET PREPARATION ##
##########################################################################################
##########################################################################################
## Dataset preparation - tree rings

# Import VEP II tree ring dataset and match Site ID with Smithsonian site numbers, when available
vepii.tree.rings$SITE_ID <- vepii.database$ID[match(vepii.tree.rings$Site.No,vepii.database$COsitenum)]

# Subset the tree ring dataset to include only dates from the Four Corners and Mesa Verde
tree.rings.n <- vepii.tree.rings[which(vepii.tree.rings$Region == 'FourCorners' | vepii.tree.rings$Region == 'MesaVerde'),]

# Further subset the tree ring dataset to include only cutting and near-cutting dates
tree.rings <- tibble::as_tibble(tree.rings.n[which(tree.rings.n$Type.of.Date == 'Cutting' | tree.rings.n$Type.of.Date == 'Near Cutting'),])

# Select, re-order, and re-name columns for consistency through analysis
tree.rings <- tree.rings[,c('SITE_ID','Site.No','Site.Name','Inside.Date','Inside.Suffix','Outside.Date','Outside.Suffix','Type.of.Date','Region','Provenience','Species','TRL.Spec.No','TRLControl')]
tree.rings.names <- c('SITE_ID','SITE_NO','SITE_NAME','DATE_INSIDE','DATE_INSIDE_SUFFIX','DATE_OUTSIDE','DATE_OUTSIDE_SUFFIX','DATE_TYPE','REGION','PROVENIENCE','SPECIES','TRL_SPECIES_NO','TRL_CONTROL')
names(tree.rings) <- tree.rings.names

# Save tree ring dataset with incomplete site numbers
utils::write.csv(tree.rings,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-incomplete-SITE_ID.csv',row.names=F)

# Read through SITE_NAME columns and add Smithsonian site numbers, when available, outside R environment, then load in completed tree ring information
processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-complete-SITE_ID.csv')

# Remove tree ring dates that do not have an associated site number (consequently, dates with no known location or associated ceramic information because they cannot be matched with corresponding sites in architectural, feature, or ceramic datasets)
processed.tree.rings <- processed.tree.rings[!is.na(processed.tree.rings$SITE_ID),]

# Limit the information used in the following processes to necessary columns
tree.rings <- processed.tree.rings[,c('SITE_ID','DATE_INSIDE','DATE_OUTSIDE')]

##########################################################################################
## Dataset preparation - tree rings by 1-year increments

# Create table for each row in tree ring database with columns from the earliest date in the dataset to the ending date of the study period (AD 1300)
years.1 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1))))
names(years.1) <- as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1))
tree.rings.years.1 <- base::cbind(tree.rings,years.1)

# Assign architectural use-life years to corresponding increments
architecture.use.life <- matrix(NA,nrow=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)),ncol=3)
architecture.use.life[,1] <- seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)

# Architecture use-life for small sites
architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),2] <- 8
architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(799,architecture.use.life[,1])),1),2] <- 13
architecture.use.life[seq(which(grepl(800,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),2] <- 18
architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1099,architecture.use.life[,1])),1),2] <- 21
architecture.use.life[seq(which(grepl(1100,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),2] <- 40
architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),2] <- 39:0

# Architecture use-life for community center sites
architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),3] <- 28
architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),3] <- 28
architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),3] <- 40
architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),3] <- 39:0

architecture.use.life <- base::as.data.frame(architecture.use.life)
names(architecture.use.life) <- c('YEAR','SS_USE_LIFE','CC_USE_LIFE')
utils::write.csv(architecture.use.life,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-1.csv',row.names=F)

## Small Sites
# Assign tallies to corresponding columns for the known cutting date, and following columns corresponding to the use-life of small site structures
architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-1.csv')
for(i in 1:nrow(tree.rings.years.1)) {
  cutting.date <- plyr::round_any(tree.rings.years.1[i,]$DATE_OUTSIDE,1,f=floor)
  index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor))) + 4
  use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
  tree.rings.years.1[i,index:(index + (plyr::round_any(use.life$SS_USE_LIFE,1,f=floor)-1))] <- 1
}
tree.rings.years.1[,4:ncol(tree.rings.years.1)][is.na(tree.rings.years.1[,4:ncol(tree.rings.years.1)])] <- 0

# Save occupation tallies assigned by corresponding increment
utils::write.csv(tree.rings.years.1,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-ss-1.csv',row.names=F)

# Import occupation tallies assigned by corresponding increment
processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-ss-1.csv')

# Aggregate occupation tallies by SITE_ID, creating a table with one row == one site
unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
for(i in 1:length(unique.SITE_ID)) {
  one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
  aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
  processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
}
processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)))

# Save tree ring tallies aggregated by site and by corresponding increment
utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-ss-1.csv',row.names=F)

## Community Center sites
# Assign tallies to corresponding columns for the known cutting date, and following columns corresponding to the use-life of community center structures
architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-1.csv')
for(i in 1:nrow(tree.rings.years.1)) {
  cutting.date <- plyr::round_any(tree.rings.years.1[i,]$DATE_OUTSIDE,1,f=floor)
  index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor))) + 4
  use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
  tree.rings.years.1[i,index:(index + (plyr::round_any(use.life$CC_USE_LIFE,1,f=floor)-1))] <- 1
}
tree.rings.years.1[,4:ncol(tree.rings.years.1)][is.na(tree.rings.years.1[,4:ncol(tree.rings.years.1)])] <- 0

# Save occupation tallies assigned by corresponding increment
utils::write.csv(tree.rings.years.1,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-cc-1.csv',row.names=F)

# Import occupation tallies assigned by corresponding increment
processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-cc-1.csv')

# Aggregate occupation tallies by SITE_ID, creating a table with one row == one site
unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
for(i in 1:length(unique.SITE_ID)) {
  one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
  aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
  processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
}
processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)))

# Save tree ring tallies aggregated by site and by corresponding increment
utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-cc-1.csv',row.names=F)

##########################################################################################
## Dataset preparation - tree rings by 200-year increments for example artificial neural network in Figure 1

# Create table for each row in tree ring database with columns from the earliest date in the dataset to the ending date of the study period (AD 1300)
years.200 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor),1400,200))))
names(years.200) <- as.character(seq(400,1400,200))
tree.rings.years.200 <- base::cbind(tree.rings,years.200)

# Assign tallies for cutting dates (this section only used for example in Figure 1)
for(i in 1:nrow(tree.rings.years.200)) {
  cutting.date <- plyr::round_any(tree.rings.years.200[i,]$DATE_OUTSIDE,200,f=floor)
  index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor)) / 200) + 4
  tree.rings.years.200[i,index] <- 1
}
tree.rings.years.200[,4:ncol(tree.rings.years.200)][is.na(tree.rings.years.200[,4:ncol(tree.rings.years.200)])] <- 0

# Save occupation tallies assigned by corresponding increment
utils::write.csv(tree.rings.years.200,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-200.csv',row.names=F)

# Import occupation tallies assigned by corresponding increment
processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-200.csv')

# Aggregate occupation tallies by SITE_ID, creating a table with one row == one site
unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
for(i in 1:length(unique.SITE_ID)) {
  one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
  aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
  processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
}
processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor),1300,200)))

# Save tree ring tallies aggregated by site and by corresponding increment
utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-200.csv',row.names=F)

#########################################################################################
##########################################################################################
## Dataset preparation - ceramic tallies

# Import VEP II ceramics dataset, identify diagnostic wares, non-diagnositc wares, and comment columns
vepii.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-ceramics.csv')
ceramics <- vepii.ceramics[,c(3,8:39,43)]
ceramics.names <- c('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','GRAY_NECKBANDED','GRAY_PLAIN','CORRUGATED_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','CORRUGATED_HOVENWEEP','CORRUGATED','GRAY_OTHER','GRAY_COMMENTS','BW_CHAPIN','BW_PIEDRA','WHITE_EARLY','BW_CORTEZ','BW_MANCOS','BW_PII','BW_MCELMO','BW_MESAVERDE','BW_PIII','BW_MINERAL','BW_ORGANIC','WHITE_LATE','WHITE_OTHER','WHITE_COMMENT','RO_ABAJO','BR_BLUFF','BR_DEADMANS','SJ_REDWARE','RED_OTHER','RED_COMMENT','BW_WETHERILL')
names(ceramics) <- ceramics.names
ceramics <- base::as.data.frame(ceramics %>% dplyr::select('SITE_ID','GRAY_PLAIN','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED','CORRUGATED_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','GRAY_NECKBANDED','CORRUGATED_HOVENWEEP','GRAY_OTHER','GRAY_COMMENTS','SJ_REDWARE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','RED_OTHER','RED_COMMENT','WHITE_EARLY','WHITE_LATE','BW_PII','BW_PIII','BW_MINERAL','BW_ORGANIC','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_WETHERILL','BW_MCELMO','BW_MESAVERDE','WHITE_OTHER','WHITE_COMMENT'))

# Save ceramic dataset with comment columns
utils::write.csv(ceramics,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-incomplete.csv',row.names=F)

# Read through comment columns and add tallies where necessary outside R environment, then load in completed ceramic information
processed.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-total.csv')

# Aggregate sites with multiple ceramic tallies, creating a table with one row == one site
unique.SITE_ID <- base::unique(processed.ceramics$SITE_ID)
processed.ceramics.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.ceramics))
for(i in 1:length(unique.SITE_ID)) {
  one.site.data <- processed.ceramics[which(processed.ceramics$SITE_ID == unique.SITE_ID[i]),]
  aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,2:ncol(processed.ceramics)])))
  aggregated.SITE_ID <- base::cbind(unique.SITE_ID[i],aggregated.site.data)
  processed.ceramics.aggregated[i,] <- aggregated.SITE_ID[1,]
}
processed.ceramics.aggregated <- base::as.data.frame(processed.ceramics.aggregated)
names(processed.ceramics.aggregated) <- names(processed.ceramics)

# Identify sites with ceramic presence/absence information
ceramics.aggregated.presence <- processed.ceramics.aggregated[which(matrixStats::rowMaxs(as.matrix(processed.ceramics.aggregated[,2:ncol(processed.ceramics.aggregated)])) == 1 ),]

# Combine overlapping typologies
ceramics.aggregated.presence$GRAY_CHAPIN <- dplyr::coalesce(ceramics.aggregated.presence$GRAY_CHAPIN,ceramics.aggregated.presence$GRAY_FUGITIVE)

# Remove overlapping or generic typologies
ceramics.aggregated.presence <- base::as.data.frame(ceramics.aggregated.presence %>% dplyr::select(-'GRAY_FUGITIVE','GRAY_NECKBANDED'))

# Save aggregated ceramic presence/absence
utils::write.csv(ceramics.aggregated.presence,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated-presence.csv',row.names=F)

# Identify sites with ceramic tally information
ceramics.aggregated.tally <- processed.ceramics.aggregated[which(matrixStats::rowMaxs(as.matrix(processed.ceramics.aggregated[,2:ncol(processed.ceramics.aggregated)])) > 1 ),]

# Combine overlapping typologies
ceramics.aggregated.tally$GRAY_CHAPIN <- dplyr::coalesce(ceramics.aggregated.tally$GRAY_CHAPIN,ceramics.aggregated.tally$GRAY_FUGITIVE)

# Remove overlapping or generic typologies
ceramics.aggregated.tally <- base::as.data.frame(ceramics.aggregated.tally %>% dplyr::select(-'GRAY_FUGITIVE','GRAY_NECKBANDED'))

# Save aggregated ceramic tallies
utils::write.csv(ceramics.aggregated.tally,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated-tally.csv',row.names=F)

#########################################################################################
##########################################################################################
## Aggregating ceramic tally datasets with tree ring dataset

ceramics.aggregated.tally <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated-tally.csv')

# Aggregate cultural data with tree rings at 1-year increments for small sites
tree.rings.aggregated.ss.1 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-ss-1.csv')
dataset.ss <- base::merge(ceramics.aggregated.tally,tree.rings.aggregated.ss.1,by='SITE_ID')
utils::write.csv(dataset.ss,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-ss-1.csv',row.names=F)

# Aggregate cultural data with tree rings at 1-year increments for community center sites
tree.rings.aggregated.cc.1 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-cc-1.csv')
dataset.cc <- base::merge(ceramics.aggregated.tally,tree.rings.aggregated.cc.1,by='SITE_ID')
utils::write.csv(dataset.cc,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-cc-1.csv',row.names=F)

# Aggregate cultural data with tree rings at 200-year increments
dataset <- base::merge(ceramics.aggregated.tally,tree.rings.aggregated.200,by='SITE_ID')
utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-200.csv',row.names=F)

#########################################################################################
#########################################################################################
## Calculating annual rate of peak population decay by length of occupation, as reported in Ortman_et_al_2007:Figure 8

# Import table with values displayed in Ortman_et_al_2007:Figure 8
excavated.sites <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/Ortman_et_al_2007-Figure-8.csv',header=T)

# Create sequences of peak population decay to distribute known years and changes through time by reported ranges of years
AD450.AD900 <- rep(excavated.sites[1,]$PEAK_DECAY,times=900-450)
AD900.AD1080 <- seq(excavated.sites[1,]$PEAK_DECAY,excavated.sites[2,]$PEAK_DECAY,(excavated.sites[2,]$PEAK_DECAY-excavated.sites[1,]$PEAK_DECAY)/(1080-900))
AD1080.AD1270 <- rep(excavated.sites[2,]$PEAK_DECAY,times=1270-1080)
AD1270.AD1300 <- rep(excavated.sites[3,]$PEAK_DECAY,times=1300-1270)

# Combine sequences of peak population decay by year
population.decay <- rbind(as.matrix(AD450.AD900),as.matrix(AD900.AD1080),as.matrix(AD1080.AD1270),as.matrix(AD1270.AD1300))

# Combine corresponding year with peak population decay
population.decay.by.year <- cbind(as.matrix(seq(450,1300,1)),population.decay)

# Save peak population decay by year
utils::write.csv(population.decay.by.year,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/population-decay-by-year.csv',row.names=F)

#########################################################################################
#########################################################################################
## Calculating average life-expectancy by year, as reported in Kohler_and_Reese_2014:10104

# Create sequences of life-expectancy to evenly distribute changes through time by reported ranges of years
BC900.AD600 <- seq(35,37,(37-35)/(900+600))
AD600.AD1000 <- seq(37,40,(40-37)/(1000-600))
AD1000.AD1150 <- seq(40,35,(35-40)/(1150-1000))
AD1150.AD1300 <- seq(35,36,(36-35)/(1300-1150))

# Combine sequences of life-expectancy by year
life.expectancy <- rbind(as.matrix(BC900.AD600),as.matrix(AD600.AD1000[-1]),as.matrix(AD1000.AD1150[-1]),as.matrix(AD1150.AD1300[-1]))

# Combine corresponding year with life-expectancy
life.expectancy.by.year <- cbind(as.matrix(seq(-900,1300,1)),life.expectancy)

# Save life-expectancy by year
utils::write.csv(life.expectancy.by.year,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/life-expectancy-by-year.csv',row.names=F)

##########################################################################################
#########################################################################################
## Finalized datasets to load when running model
# Tree ring data by increments
tree.rings.aggregated.ss.1 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-ss-1.csv')
tree.rings.aggregated.cc.1 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-cc-1.csv')
tree.rings.aggregated.200 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-200.csv')

# Ceramic information
ceramics.tally <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated-tally.csv')
ceramics.presence <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated-presence.csv')

# Life-expectancy by year
population.decay.by.year <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/population-decay-by-year.csv')

# Life-expectancy by year
life.expectancy.by.year <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/life-expectancy-by-year.csv')






