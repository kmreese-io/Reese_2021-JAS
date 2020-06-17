##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## RESULTS ##

# Determine the optimal smoothing window for the number of nodes
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-mean/accuracy-ceramics-smoothed-mean.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:100)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

# Identify all households and repeat them based on number of pitstructures
occupation.predictions.total <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-predictions-total.csv')
occupation.information <- unique(occupation.predictions.total)
habitations <- occupation.information[which(occupation.information$PRIMARY == 'HABITATION' | occupation.information$PRIMARY == 'Habitation'),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES > 0 ),]
habitations.with.repitition <- habitations.with.pitstructures[rep(seq(nrow(habitations.with.pitstructures)),habitations.with.pitstructures$PITSTRUCTURES),1:ncol(habitations.with.pitstructures)]
roomblocks <- rbind(occupation.information[which(occupation.information$PRIMARY == 'ROOMBLOCK' ),],habitations[which(habitations$PITSTRUCTURES == 0 ),])
occupation.households <- rbind(habitations.with.repitition,roomblocks)

# Smooth the results with corresponding average life expectancy to determine range of population estimates for study period
smooth.start <- 450
smooth.end <- 600
smoothed.predictions.1 <- matrix(NA,nrow=nrow(occupation.households),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.households)) {
  range.predictions <- as.numeric(occupation.households[j,(which(colnames(occupation.households) == paste('X',smooth.start,sep='')):which(colnames(occupation.households) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- round(smoother::smth.gaussian(range.predictions,window=window,tails=T))
  smoothed.predictions.1[j,] <- smooth.prediction
}

smooth.start <- 601
smooth.end <- 1000
smoothed.predictions.2 <- matrix(NA,nrow=nrow(occupation.households),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.households)) {
  range.predictions <- as.numeric(occupation.households[j,(which(colnames(occupation.households) == paste('X',smooth.start,sep='')):which(colnames(occupation.households) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- round(smoother::smth.gaussian(range.predictions,window=window,tails=T))
  smoothed.predictions.2[j,] <- smooth.prediction
}

smooth.start <- 1001
smooth.end <- 1150
smoothed.predictions.3 <- matrix(NA,nrow=nrow(occupation.households),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.households)) {
  range.predictions <- as.numeric(occupation.households[j,(which(colnames(occupation.households) == paste('X',smooth.start,sep='')):which(colnames(occupation.households) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- round(smoother::smth.gaussian(range.predictions,window=window,tails=T))
  smoothed.predictions.3[j,] <- smooth.prediction
}

smooth.start <- 1151
smooth.end <- 1300
smoothed.predictions.4 <- matrix(NA,nrow=nrow(occupation.households),ncol=length(seq(smooth.start,smooth.end,1)))
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)

for(j in 1:nrow(occupation.households)) {
  range.predictions <- as.numeric(occupation.households[j,(which(colnames(occupation.households) == paste('X',smooth.start,sep='')):which(colnames(occupation.households) == paste('X',smooth.end,sep='')))])
  smooth.prediction <- round(smoother::smth.gaussian(range.predictions,window=window,tails=T))
  smoothed.predictions.4[j,] <- smooth.prediction
}

smoothed.predictions <- cbind(smoothed.predictions.1,smoothed.predictions.2,smoothed.predictions.3,smoothed.predictions.4)

total.original.population <- round(colSums(occupation.households[,8:ncol(occupation.households)]))
total.smoothed.population <- colSums(smoothed.predictions)

annual.household.population <- cbind(seq(year.start,year.end,year.duration),total.original.population,total.smoothed.population)
utils::write.csv(annual.household.population,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/annual-household-population.csv',row.names=F)

