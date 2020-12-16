##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## RESULTS ##
##########################################################################################
##########################################################################################
## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

##########################################################################################
## Identify optimal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:100)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
##########################################################################################
## Smooth all initial predictions based on optimal smoothing window
occupation.predictions.total <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-region-complete.csv')
occupation.predictions.smoothed <- matrix(NA,nrow=nrow(occupation.predictions.total),ncol=length(8:ncol(occupation.predictions.total)))
for(i in 1:nrow(occupation.predictions.total)) {
  occupation.predictions.smoothed[i,] <- smoother::smth.gaussian(as.matrix(occupation.predictions.total[i,8:ncol(occupation.predictions.total)]),window=window.optimal,tails=T)
}
occupation.predictions.smoothed <- cbind(occupation.predictions.total[,1:7],occupation.predictions.smoothed)
names(occupation.predictions.smoothed) <- names(occupation.predictions.total)

##########################################################################################
## Export predicted occupation based on ideal model parameters
utils::write.csv(occupation.predictions.smoothed,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-probability-region-complete-smoothed.csv',row.names=F)
occupation.predictions.smoothed <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-probability-region-complete-smoothed.csv')

##########################################################################################
##########################################################################################
## Identify all habitation sites, including habitations/roomblocks without directly-associated pitstructures
habitations <- occupation.predictions.smoothed[which(occupation.predictions.smoothed$PRIMARY == 'HABITATION' | occupation.predictions.smoothed$PRIMARY == 'Habitation'),]
habitations <- habitations[!duplicated(habitations$SITE_ID),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES > 0 ),]
household.allotment <- matrix(NA,nrow=nrow(habitations.with.pitstructures),ncol=length(8:ncol(habitations.with.pitstructures)))
for(i in 1:nrow(habitations.with.pitstructures)) {
  household.allotment[i,] <- round(as.matrix(habitations.with.pitstructures[i,]$PITSTRUCTURES * habitations.with.pitstructures[i,8:ncol(habitations.with.pitstructures)]))
}
occupation.households <- cbind(habitations.with.pitstructures[,1:7],household.allotment)
names(occupation.households) <- names(habitations.with.pitstructures)

##########################################################################################
## Export predicted occupation by residence with numbers of households allocated through time (occupation-by-household)
utils::write.csv(occupation.households,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv',row.names=F)
occupation.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')

##########################################################################################
##########################################################################################
## Smooth site occupation predictions with average life-expectancy through time

## Import predicted occupation based on ideal model parameters
utils::write.csv(occupation.predictions.smoothed,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-probability-region-complete-smoothed.csv',row.names=F)
occupation.predictions.smoothed <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-probability-region-complete-smoothed.csv')

## Smooth the results with corresponding average life-expectancy [@Kohler_and_Reese_2014] to determine range of population estimates for study period
smooth.start <- 450
smooth.end <- 600
smoothed.predictions.1 <- matrix(NA,nrow=nrow(occupation.predictions.smoothed),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.predictions.smoothed)) {
  range.predictions <- as.numeric(occupation.predictions.smoothed[j,(which(colnames(occupation.predictions.smoothed) == paste('X',smooth.start,sep='')):which(colnames(occupation.predictions.smoothed) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
  smoothed.predictions.1[j,] <- smooth.prediction
}

smooth.start <- 601
smooth.end <- 1000
smoothed.predictions.2 <- matrix(NA,nrow=nrow(occupation.predictions.smoothed),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.predictions.smoothed)) {
  range.predictions <- as.numeric(occupation.predictions.smoothed[j,(which(colnames(occupation.predictions.smoothed) == paste('X',smooth.start,sep='')):which(colnames(occupation.predictions.smoothed) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
  smoothed.predictions.2[j,] <- smooth.prediction
}

smooth.start <- 1001
smooth.end <- 1150
smoothed.predictions.3 <- matrix(NA,nrow=nrow(occupation.predictions.smoothed),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.predictions.smoothed)) {
  range.predictions <- as.numeric(occupation.predictions.smoothed[j,(which(colnames(occupation.predictions.smoothed) == paste('X',smooth.start,sep='')):which(colnames(occupation.predictions.smoothed) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
  smoothed.predictions.3[j,] <- smooth.prediction
}

smooth.start <- 1151
smooth.end <- 1300
smoothed.predictions.4 <- matrix(NA,nrow=nrow(occupation.predictions.smoothed),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.predictions.smoothed)) {
  range.predictions <- as.numeric(occupation.predictions.smoothed[j,(which(colnames(occupation.predictions.smoothed) == paste('X',smooth.start,sep='')):which(colnames(occupation.predictions.smoothed) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
  smoothed.predictions.4[j,] <- smooth.prediction
}

smoothed.predictions <- cbind(smoothed.predictions.1,smoothed.predictions.2,smoothed.predictions.3,smoothed.predictions.4)

occupation.population.smoothed <- cbind(occupation.predictions.smoothed[,1:7],smoothed.predictions)
names(occupation.population.smoothed) <- names(occupation.predictions.smoothed)

##########################################################################################
## Export predicted occupation smoothed by population
utils::write.csv(occupation.population.smoothed,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/population-probability-region-complete-smoothed.csv',row.names=F)
occupation.population.smoothed <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/population-probability-region-complete-smoothed.csv')

##########################################################################################
##########################################################################################
## Identify all habitation sites, including habitations/roomblocks without directly-associated pitstructures
habitations <- occupation.population.smoothed[which(occupation.population.smoothed$PRIMARY == 'HABITATION' | occupation.population.smoothed$PRIMARY == 'Habitation'),]
habitations <- habitations[!duplicated(habitations$SITE_ID),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES > 0 ),]
household.allotment <- matrix(NA,nrow=nrow(habitations.with.pitstructures),ncol=length(8:ncol(habitations.with.pitstructures)))
for(i in 1:nrow(habitations.with.pitstructures)) {
  household.allotment[i,] <- round(as.matrix(habitations.with.pitstructures[i,]$PITSTRUCTURES * habitations.with.pitstructures[i,8:ncol(habitations.with.pitstructures)]))
}
household.allotment <- cbind(habitations.with.pitstructures[,1:7],household.allotment)
names(household.allotment) <- names(occupation.population.smoothed)

roomblocks <- rbind(occupation.population.smoothed[which(occupation.population.smoothed$PRIMARY == 'ROOMBLOCK' ),],habitations[which(habitations$PITSTRUCTURES == 0 ),])
roomblock.allotment <- matrix(NA,nrow=nrow(roomblocks),ncol=length(8:ncol(roomblocks)))
for(i in 1:nrow(roomblocks)) {
  roomblock.allotment[i,] <- round(as.matrix(1 * roomblocks[i,8:ncol(roomblocks)]))
}
roomblock.allotment <- cbind(roomblocks[,1:7],roomblock.allotment)
names(roomblock.allotment) <- names(household.allotment)

occupation.population <- rbind(household.allotment,roomblock.allotment)

##########################################################################################
## Export predicted occupation by population with numbers of households allocated through time (occupation-by-population)
utils::write.csv(occupation.population,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-population.csv',row.names=F)
occupation.population <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-population.csv')

##########################################################################################
##########################################################################################
