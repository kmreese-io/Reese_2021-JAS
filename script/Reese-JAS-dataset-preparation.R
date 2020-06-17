##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## DATASET PREPARATION ##

# ##########################################################################################
# ## TREE RING PREPARATION
# vepii.tree.rings$SITE_ID <- vepii.database$ID[match(vepii.tree.rings$Site.No,vepii.database$COsitenum)]
# tree.rings.n <- vepii.tree.rings[which(vepii.tree.rings$Region == 'FourCorners' | vepii.tree.rings$Region == 'MesaVerde'),]
# tree.rings <- tibble::as_tibble(tree.rings.n[which(tree.rings.n$Type.of.Date == 'Cutting' | tree.rings.n$Type.of.Date == 'Near Cutting'),])
# tree.rings <- tree.rings[,c('SITE_ID','Site.No','Site.Name','Inside.Date','Inside.Suffix','Outside.Date','Outside.Suffix','Type.of.Date','Region','Provenience','Species','TRL.Spec.No','TRLControl')]
# tree.rings.names <- c('SITE_ID','SITE_NO','SITE_NAME','DATE_INSIDE','DATE_INSIDE_SUFFIX','DATE_OUTSIDE','DATE_OUTSIDE_SUFFIX','DATE_TYPE','REGION','PROVENIENCE','SPECIES','TRL_SPECIES_NO','TRL_CONTROL')
# names(tree.rings) <- tree.rings.names
# 
# utils::write.csv(tree.rings,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-incomplete-SITE_ID.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-complete-SITE_ID.csv')
# processed.tree.rings <- processed.tree.rings[!is.na(processed.tree.rings$SITE_ID),]
# tree.rings <- processed.tree.rings[,c('SITE_ID','DATE_INSIDE','DATE_OUTSIDE')]
# ##########################################################################################
# ## TREE RINGS BY 1 YEAR INCREMENTS
# years.1 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1))))
# names(years.1) <- as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1))
# tree.rings.years.1 <- base::cbind(tree.rings,years.1)
# 
# architecture.use.life <- matrix(NA,nrow=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)),ncol=2)
# architecture.use.life[,1] <- seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)
# architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),2] <- 8
# architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(799,architecture.use.life[,1])),1),2] <- 13
# architecture.use.life[seq(which(grepl(800,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),2] <- 18
# architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1099,architecture.use.life[,1])),1),2] <- 21
# architecture.use.life[seq(which(grepl(1100,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),2] <- 40
# architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),2] <- 39:0
# architecture.use.life <- base::as.data.frame(architecture.use.life)
# names(architecture.use.life) <- c('YEAR','USE_LIFE')
# 
# utils::write.csv(architecture.use.life,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-1.csv',row.names=F)
# 
# architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-1.csv')
# 
# for(i in 1:nrow(tree.rings.years.1)) {
#   cutting.date <- plyr::round_any(tree.rings.years.1[i,]$DATE_OUTSIDE,1,f=floor)
#   index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor))) + 4
#   use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
#   tree.rings.years.1[i,index:(index + (plyr::round_any(use.life$USE_LIFE,1,f=floor)-1))] <- 1
# }
# 
# tree.rings.years.1[,4:ncol(tree.rings.years.1)][is.na(tree.rings.years.1[,4:ncol(tree.rings.years.1)])] <- 0
# 
# utils::write.csv(tree.rings.years.1,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-1.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-1.csv')
# 
# unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
# processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
#   processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
# }
# processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
# names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),1,f=floor),1300,1)))
# 
# utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-1.csv',row.names=F)
# ##########################################################################################
# ## TREE RINGS BY 10 YEAR INCREMENTS
# years.10 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor),1300,10))))
# names(years.10) <- as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor),1300,10))
# tree.rings.years.10 <- base::cbind(tree.rings,years.10)
# 
# architecture.use.life <- matrix(NA,nrow=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor),1300,1)),ncol=2)
# architecture.use.life[,1] <- seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor),1300,1)
# architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),2] <- 8
# architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(799,architecture.use.life[,1])),1),2] <- 13
# architecture.use.life[seq(which(grepl(800,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),2] <- 18
# architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1099,architecture.use.life[,1])),1),2] <- 21
# architecture.use.life[seq(which(grepl(1100,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),2] <- 40
# architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),2] <- 39:0
# architecture.use.life <- base::as.data.frame(architecture.use.life)
# names(architecture.use.life) <- c('YEAR','USE_LIFE')
# 
# utils::write.csv(architecture.use.life,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-10.csv',row.names=F)
# 
# architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-10.csv')
# 
# for(i in 1:nrow(tree.rings.years.10)) {
#   cutting.date <- plyr::round_any(tree.rings.years.10[i,]$DATE_OUTSIDE,10,f=floor)
#   index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor)) / 10) + 4
#   use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
#   tree.rings.years.10[i,index:(index + plyr::round_any(use.life$USE_LIFE/10,1,f=floor))] <- 1
# }
# 
# tree.rings.years.10[,4:ncol(tree.rings.years.10)][is.na(tree.rings.years.10[,4:ncol(tree.rings.years.10)])] <- 0
# 
# utils::write.csv(tree.rings.years.10,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-10.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-10.csv')
# 
# unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
# processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
#   processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
# }
# processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
# names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),10,f=floor),1300,10)))
# 
# utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-10.csv',row.names=F)
# ##########################################################################################
# ## TREE RINGS BY 20 YEAR INCREMENTS
# years.20 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor),1300,20))))
# names(years.20) <- as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor),1300,20))
# tree.rings.years.20 <- base::cbind(tree.rings,years.20)
# 
# architecture.use.life <- matrix(NA,nrow=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor),1300,1)),ncol=2)
# architecture.use.life[,1] <- seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor),1300,1)
# architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),2] <- 8
# architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(799,architecture.use.life[,1])),1),2] <- 13
# architecture.use.life[seq(which(grepl(800,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),2] <- 18
# architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1099,architecture.use.life[,1])),1),2] <- 21
# architecture.use.life[seq(which(grepl(1100,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),2] <- 40
# architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),2] <- 39:0
# architecture.use.life <- base::as.data.frame(architecture.use.life)
# names(architecture.use.life) <- c('YEAR','USE_LIFE')
# 
# utils::write.csv(architecture.use.life,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-20.csv',row.names=F)
# 
# architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-20.csv')
# 
# for(i in 1:nrow(tree.rings.years.20)) {
#   cutting.date <- plyr::round_any(tree.rings.years.20[i,]$DATE_OUTSIDE,20,f=floor)
#   index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor)) / 20) + 4
#   use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
#   tree.rings.years.20[i,index:(index + plyr::round_any(use.life$USE_LIFE/20,1,f=floor))] <- 1
# }
# 
# tree.rings.years.20[,4:ncol(tree.rings.years.20)][is.na(tree.rings.years.20[,4:ncol(tree.rings.years.20)])] <- 0
# 
# utils::write.csv(tree.rings.years.20,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-20.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-20.csv')
# 
# unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
# processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
#   processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
# }
# processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
# names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),20,f=floor),1300,20)))
# 
# utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-20.csv',row.names=F)
# ##########################################################################################
# ## TREE RINGS BY 30 YEAR INCREMENTS
# years.30 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor),1300,30))))
# names(years.30) <- as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor),1300,30))
# tree.rings.years.30 <- base::cbind(tree.rings,years.30)
# 
# architecture.use.life <- matrix(NA,nrow=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor),1300,1)),ncol=2)
# architecture.use.life[,1] <- seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor),1300,1)
# architecture.use.life[seq(1,which(grepl(724,architecture.use.life[,1])),1),2] <- 8
# architecture.use.life[seq(which(grepl(725,architecture.use.life[,1])),which(grepl(799,architecture.use.life[,1])),1),2] <- 13
# architecture.use.life[seq(which(grepl(800,architecture.use.life[,1])),which(grepl(1019,architecture.use.life[,1])),1),2] <- 18
# architecture.use.life[seq(which(grepl(1020,architecture.use.life[,1])),which(grepl(1099,architecture.use.life[,1])),1),2] <- 21
# architecture.use.life[seq(which(grepl(1100,architecture.use.life[,1])),which(grepl(1260,architecture.use.life[,1])),1),2] <- 40
# architecture.use.life[seq(which(grepl(1261,architecture.use.life[,1])),which(grepl(1300,architecture.use.life[,1])),1),2] <- 39:0
# architecture.use.life <- base::as.data.frame(architecture.use.life)
# names(architecture.use.life) <- c('YEAR','USE_LIFE')
# 
# utils::write.csv(architecture.use.life,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-30.csv',row.names=F)
# 
# architecture.use.life <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/architecture-use-life-30.csv')
# 
# for(i in 1:nrow(tree.rings.years.30)) {
#   cutting.date <- plyr::round_any(tree.rings.years.30[i,]$DATE_OUTSIDE,30,f=floor)
#   index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor)) / 30) + 4
#   use.life <- architecture.use.life[which(architecture.use.life$YEAR == cutting.date),]
#   tree.rings.years.30[i,index:(index + plyr::round_any(use.life$USE_LIFE/30,1,f=floor))] <- 1
# }
# 
# tree.rings.years.30[,4:ncol(tree.rings.years.30)][is.na(tree.rings.years.30[,4:ncol(tree.rings.years.30)])] <- 0
# 
# utils::write.csv(tree.rings.years.30,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-30.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-30.csv')
# 
# unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
# processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
#   processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
# }
# processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
# names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),30,f=floor),1300,30)))
# 
# utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-30.csv',row.names=F)
# ##########################################################################################
# ## TREE RINGS BY 200 YEAR INCREMENTS FOR EXAMPLE FIGURE
# years.200 <- base::as.data.frame(base::matrix(NA,nrow=nrow(tree.rings),ncol=length(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor),1400,200))))
# names(years.200) <- as.character(seq(400,1400,200))
# tree.rings.years.200 <- base::cbind(tree.rings,years.200)
# 
# for(i in 1:nrow(tree.rings.years.200)) {
#   cutting.date <- plyr::round_any(tree.rings.years.200[i,]$DATE_OUTSIDE,200,f=floor)
#   index <- ((cutting.date - plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor)) / 200) + 4
#   tree.rings.years.200[i,index] <- 1
# }
# 
# tree.rings.years.200[,4:ncol(tree.rings.years.200)][is.na(tree.rings.years.200[,4:ncol(tree.rings.years.200)])] <- 0
# 
# utils::write.csv(tree.rings.years.200,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-200.csv',row.names=F)
# 
# processed.tree.rings <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-200.csv')
# 
# unique.SITE_ID <- base::unique(processed.tree.rings$SITE_ID)
# processed.tree.rings.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.tree.rings)-3)
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.tree.rings[which(processed.tree.rings$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,4:ncol(processed.tree.rings)])))
#   processed.tree.rings.aggregated[i,] <- aggregated.site.data[1,]
# }
# processed.tree.rings.aggregated <- base::as.data.frame(base::cbind(unique.SITE_ID,processed.tree.rings.aggregated))
# names(processed.tree.rings.aggregated) <- c('SITE_ID',as.character(seq(plyr::round_any(min(tree.rings$DATE_OUTSIDE),200,f=floor),1300,200)))
# 
# utils::write.csv(processed.tree.rings.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-200.csv',row.names=F)
# ##########################################################################################
# ## ARCHITECTURE AND CERAMIC DATA
# vepii.site.attributes <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-site-attributes.csv')
# vepii.site.features <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-site-features.csv')
# 
# vepii.cultural.features <- tibble::as_tibble(base::merge(vepii.site.attributes,vepii.site.features,by='Site.ID'))
# cultural.features <- vepii.cultural.features[,c('Site.ID','Tower','Multiwall','Encwall','Pitstrdep','PitstrOS','Check.Dam','Plaza','Towers','Multiwalls','Dshape','OSpitstrs','Other','Comment.on.Other')]
# cultural.features.names <- c('SITE_ID','TOWER','MULTIWALL','ENCLOSING','PITSTRUCTURE','OVERSIZED','CHECKDAM','PLAZA','R.TOWER','R.MULTIWALL','DSHAPE','R.OVERSIZED','OTHER','COMMENT')
# names(cultural.features) <- cultural.features.names
# 
# # Combine repeated architectural features
# cultural.features$TOWER <- dplyr::coalesce(cultural.features$TOWER,cultural.features$R.TOWER)
# cultural.features$MULTIWALL <- dplyr::coalesce(cultural.features$MULTIWALL,cultural.features$R.MULTIWALL)
# cultural.features$OVERSIZED <- dplyr::coalesce(cultural.features$OVERSIZED,cultural.features$R.OVERSIZED)
# 
# # Remove columns of repeated architectural features and reorder columns
# cultural.features %>% dplyr::select(-'R.TOWER','R.MULTIWALL','R.OVERSIZED')
# cultural.features <- base::as.data.frame(cultural.features %>% dplyr::select('SITE_ID','PITSTRUCTURE','OVERSIZED','PLAZA','CHECKDAM','TOWER','ENCLOSING','MULTIWALL','DSHAPE','OTHER','COMMENT'))
# 
# utils::write.csv(cultural.features,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/cultural-features-incomplete.csv',row.names=F)
# 
# processed.cultural.features <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/cultural-features-total.csv')
# unique.SITE_ID <- base::unique(processed.cultural.features$SITE_ID)
# 
# processed.cultural.features.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.cultural.features))
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.cultural.features[which(processed.cultural.features$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,2:ncol(processed.cultural.features)])))
#   aggregated.SITE_ID <- base::cbind(unique.SITE_ID[i],aggregated.site.data)
#   processed.cultural.features.aggregated[i,] <- aggregated.SITE_ID[1,]
# }
# processed.cultural.features.aggregated <- base::as.data.frame(processed.cultural.features.aggregated)
# names(processed.cultural.features.aggregated) <- names(processed.cultural.features)
# 
# utils::write.csv(processed.cultural.features.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/cultural-features-aggregated.csv',row.names=F)
#
# vepii.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/vepii-database/vepii-ceramics.csv')
# vepii.ceramics.tally <- vepii.ceramics[which(vepii.ceramics$datatype == 'Tally'),]
# ceramics <- vepii.ceramics.tally[,c(3,8:39,43)]
# ceramics.names <- c('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','GRAY_NECKBANDED','GRAY_PLAIN','CORRUGATED_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','CORRUGATED_HOVENWEEP','CORRUGATED','GRAY_OTHER','GRAY_COMMENTS','BW_CHAPIN','BW_PIEDRA','WHITE_EARLY','BW_CORTEZ','BW_MANCOS','BW_PII','BW_MCELMO','BW_MESAVERDE','BW_PIII','BW_MINERAL','BW_ORGANIC','WHITE_LATE','WHITE_OTHER','WHITE_COMMENT','RO_ABAJO','BR_BLUFF','BR_DEADMANS','SJ_REDWARE','RED_OTHER','RED_COMMENT','BW_WETHERILL')
# names(ceramics) <- ceramics.names
# ceramics <- base::as.data.frame(ceramics %>% dplyr::select('SITE_ID','GRAY_PLAIN','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED','CORRUGATED_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','GRAY_NECKBANDED','CORRUGATED_HOVENWEEP','GRAY_OTHER','GRAY_COMMENTS','SJ_REDWARE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','RED_OTHER','RED_COMMENT','WHITE_EARLY','WHITE_LATE','BW_PII','BW_PIII','BW_MINERAL','BW_ORGANIC','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_WETHERILL','BW_MCELMO','BW_MESAVERDE','WHITE_OTHER','WHITE_COMMENT'))
# 
# utils::write.csv(ceramics,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-incomplete.csv',row.names=F)
# 
# processed.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-total.csv')
# processed.ceramics$GRAY_CHAPIN <- dplyr::coalesce(processed.ceramics$GRAY_CHAPIN,processed.ceramics$GRAY_FUGITIVE)
# processed.ceramics <- base::as.data.frame(processed.ceramics %>% dplyr::select(-'GRAY_FUGITIVE','GRAY_NECKBANDED'))
# unique.SITE_ID <- base::unique(processed.ceramics$SITE_ID)
# 
# processed.ceramics.aggregated <- matrix(NA,nrow=length(unique.SITE_ID),ncol=ncol(processed.ceramics))
# for(i in 1:length(unique.SITE_ID)) {
#   one.site.data <- processed.ceramics[which(processed.ceramics$SITE_ID == unique.SITE_ID[i]),]
#   aggregated.site.data <- t(as.matrix(mapply(sum,one.site.data[,2:ncol(processed.ceramics)])))
#   aggregated.SITE_ID <- base::cbind(unique.SITE_ID[i],aggregated.site.data)
#   processed.ceramics.aggregated[i,] <- aggregated.SITE_ID[1,]
# }
# processed.ceramics.aggregated <- base::as.data.frame(processed.ceramics.aggregated)
# names(processed.ceramics.aggregated) <- names(processed.ceramics)
# 
# utils::write.csv(processed.ceramics.aggregated,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated.csv',row.names=F)
# ##########################################################################################
# ## AGGREGATING MATERIAL DATASETS WITH TREE RINGS
# site.information.total.aggregated <- base::merge(ceramics.aggregated,cultural.features.aggregated,by='SITE_ID')
# dataset <- base::merge(site.information.total.aggregated,tree.rings.aggregated.1,by='SITE_ID')
# utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv',row.names=F)
# 
# site.information.total.aggregated <- base::merge(ceramics.aggregated,cultural.features.aggregated,by='SITE_ID')
# dataset <- base::merge(site.information.total.aggregated,tree.rings.aggregated.10,by='SITE_ID')
# utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-10.csv',row.names=F)
# 
# site.information.total.aggregated <- base::merge(ceramics.aggregated,cultural.features.aggregated,by='SITE_ID')
# dataset <- base::merge(site.information.total.aggregated,tree.rings.aggregated.20,by='SITE_ID')
# utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-20.csv',row.names=F)
# 
# site.information.total.aggregated <- base::merge(ceramics.aggregated,cultural.features.aggregated,by='SITE_ID')
# dataset <- base::merge(site.information.total.aggregated,tree.rings.aggregated.30,by='SITE_ID')
# utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-30.csv',row.names=F)
# 
# site.information.total.aggregated <- base::merge(ceramics.aggregated,cultural.features.aggregated,by='SITE_ID')
# dataset <- base::merge(site.information.total.aggregated,tree.rings.aggregated.200,by='SITE_ID')
# utils::write.csv(dataset,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-200.csv',row.names=F)
##########################################################################################
# ## LIFE EXPECTANCY BY YEAR (KOHLER AND REESE 2014:10104)
# BC900.AD600 <- seq(35,37,(37-35)/(900+600))
# AD600.AD1000 <- seq(37,40,(40-37)/(1000-600))
# AD1000.AD1150 <- seq(40,35,(35-40)/(1150-1000))
# AD1150.AD1300 <- seq(35,36,(36-35)/(1300-1150))
# life.expectancy <- rbind(as.matrix(BC900.AD600),as.matrix(AD600.AD1000[-1]),as.matrix(AD1000.AD1150[-1]),as.matrix(AD1150.AD1300[-1]))
# 
# years.bc <- as.matrix(seq(-900,0,1))
# years.ad <- as.matrix(seq(1,1300,1))
# years.total <- rbind(years.bc,years.ad)
# 
# life.expectancy.by.year <- cbind(years.total,life.expectancy)
# 
# utils::write.csv(life.expectancy.by.year,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/life-expectancy-by-year.csv',row.names=F)
##########################################################################################
## FINALIZED DATASETS
tree.rings.aggregated.1 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-1.csv')
tree.rings.aggregated.10 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-10.csv')
tree.rings.aggregated.20 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-20.csv')
tree.rings.aggregated.30 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-30.csv')
tree.rings.aggregated.200 <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-aggregated-200.csv')

cultural.features.aggregated <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/cultural-features-aggregated.csv')
ceramics.aggregated <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/ceramics-aggregated.csv')
life.expectancy.by.year <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/life-expectancy-by-year.csv')






