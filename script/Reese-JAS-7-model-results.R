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
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
##########################################################################################
## Direct household allocation by site pre-smoothing to inform depopulation period in population reconstruction
occupation.predictions.total <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-probability-region-complete.csv')
habitations <- occupation.predictions.total[which(occupation.predictions.total$PRIMARY == 'HABITATION' | occupation.predictions.total$PRIMARY == 'Habitation'),]
habitations <- habitations[!duplicated(habitations$SITE_ID),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES > 0 ),]
household.allotment <- matrix(NA,nrow=nrow(habitations.with.pitstructures),ncol=length(8:ncol(habitations.with.pitstructures)))
for(i in 1:nrow(habitations.with.pitstructures)) {
  household.allotment[i,] <- round(as.matrix(habitations.with.pitstructures[i,]$PITSTRUCTURES * habitations.with.pitstructures[i,8:ncol(habitations.with.pitstructures)]))
}
occupation.households <- cbind(habitations.with.pitstructures[,1:7],household.allotment)
names(occupation.households) <- names(habitations.with.pitstructures)

households.presmoothed <- cbind(seq(year.start,year.end,year.duration),colSums(occupation.households[,8:ncol(occupation.households)]))
utils::write.csv(households.presmoothed,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-region-pre-smoothed.csv',row.names=F)
households.presmoothed <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/household-region-pre-smoothed.csv')

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
## Determine peak number of occupied pitstructures (when pitstructures >= 2) based on total predicted length of occupation and average population decay rate 
habitations <- occupation.predictions.smoothed[which(occupation.predictions.smoothed$PRIMARY == 'HABITATION' | occupation.predictions.smoothed$PRIMARY == 'Habitation'),]
habitations <- habitations[!duplicated(habitations$SITE_ID),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES >= 2 ),]

peak.population <- matrix(NA,ncol=1,nrow=nrow(habitations.with.pitstructures))

for(p in 1:nrow(habitations.with.pitstructures)) {
  
  site <- habitations.with.pitstructures[p,]
  probability.minimum <- 1 / site$PITSTRUCTURE
  site.probability <- cbind(seq(year.start,year.end,year.duration),t(site[,8:ncol(site)]))
  site.occupied <- site.probability[which(site.probability[,2] >= probability.minimum ),]
  mean.decay <- mean(population.decay.by.year[,2][population.decay.by.year[,1] %in% site.occupied[,1]])
  site.occupied.duration <- nrow(site.occupied)
  total.decay <- mean.decay * site.occupied.duration
  
  if(site.occupied.duration == 0 | total.decay < probability.minimum) {
    
    peak.population[p,] <- site$PITSTRUCTURE
    
  }
  
  else{
    
    peak.population.by.pitstructure <- round(site$PITSTRUCTURE - (site$PITSTRUCTURE * total.decay))
    peak.population[p,] <- peak.population.by.pitstructure
    
  }
  
}

household.allotment <- matrix(NA,nrow=nrow(habitations.with.pitstructures),ncol=length(8:ncol(habitations.with.pitstructures)))
for(i in 1:nrow(habitations.with.pitstructures)) {
  household.allotment[i,] <- round(as.matrix(peak.population[i,] * habitations.with.pitstructures[i,8:ncol(habitations.with.pitstructures)]))
}
occupation.households.large <- cbind(habitations.with.pitstructures[,1:7],household.allotment)
names(occupation.households.large) <- names(habitations.with.pitstructures)

##########################################################################################
##########################################################################################
## Identify all habitation sites with 1 associated pitstructures
habitations <- occupation.predictions.smoothed[which(occupation.predictions.smoothed$PRIMARY == 'HABITATION' | occupation.predictions.smoothed$PRIMARY == 'Habitation'),]
habitations <- habitations[!duplicated(habitations$SITE_ID),]
habitations.with.pitstructures <- habitations[which(habitations$PITSTRUCTURES == 1 ),]
household.allotment <- matrix(NA,nrow=nrow(habitations.with.pitstructures),ncol=length(8:ncol(habitations.with.pitstructures)))
for(i in 1:nrow(habitations.with.pitstructures)) {
  household.allotment[i,] <- round(as.matrix(habitations.with.pitstructures[i,]$PITSTRUCTURES * habitations.with.pitstructures[i,8:ncol(habitations.with.pitstructures)]))
}
occupation.households.small <- cbind(habitations.with.pitstructures[,1:7],household.allotment)
names(occupation.households.small) <- names(habitations.with.pitstructures)

##########################################################################################
## Export peak household population by habitation site
peak.population.sites <- rbind(occupation.households.small[,1:7],occupation.households.large[,1:7])
peak.population.counts <- as.matrix(c(base::rep(1,times=nrow(occupation.households.small)),peak.population))
peak.population.information <- cbind(peak.population.sites,peak.population.counts)
utils::write.csv(peak.population.information,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/peak-occupation-by-site.csv',row.names=F)
peak.population.information <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/peak-occupation-by-site.csv')

##########################################################################################
## Export predicted occupation by residence with numbers of households allocated through time (occupation-by-household)
occupation.households <- rbind(occupation.households.small,occupation.households.large)
utils::write.csv(occupation.households,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv',row.names=F)
occupation.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')

##########################################################################################
##########################################################################################
## Calculate total population in region by smoothing region household occupation by average life-expectancy through time
occupation.population <- matrix(NA,nrow=nrow(occupation.households),ncol=ncol(occupation.households)-7)

for(s in 1:nrow(occupation.households)) {
  
  smoothing.site <- t(occupation.households[s,8:ncol(occupation.households)])
  
  ## Smooth the results with corresponding average life-expectancy [@Kohler_and_Reese_2014] to determine range of population estimates for study period
  for(l in year.start:year.end) {
    window.life.expectancy <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == l ),2]
    smooth.prediction <- smoother::smth.gaussian(as.numeric(smoothing.site),window=window.life.expectancy,tails=T)
    occupation.population[s,l-449] <- round(smooth.prediction[l-449])
  }
  
}

occupation.population <- cbind(occupation.households[,1:7],occupation.population)
names(occupation.population) <- names(occupation.households)

##########################################################################################
## Export predicted occupation by population with numbers of households allocated through time (occupation-by-population)
utils::write.csv(occupation.population,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-population.csv',row.names=F)
occupation.population <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-population.csv')

##########################################################################################
##########################################################################################
