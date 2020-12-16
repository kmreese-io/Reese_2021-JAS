##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## FINAL RESULTS ##
##########################################################################################
##########################################################################################
## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

##########################################################################################
## Calculate total number of households in region by combining known household occupation with extrapolated household occupation by year
total.study.years <- seq(year.start,year.end,year.duration)
occupation.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')
occupation.households <- occupation.households[which(occupation.households$PITSTRUCTURES >= 1 ),]
total.extrapolated.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/extrapolated-occupation-by-household.csv',header=T,row.names=1)
sum.occupation.households <- as.numeric(colSums(occupation.households[,8:ncol(occupation.households)]))
sum.extrapolated.households <- as.numeric(colSums(total.extrapolated.households,na.rm=T))
region.occupation.household <- sum.occupation.households + sum.extrapolated.households

# Export total number of predicted households including surveyes and unsurveyed areas across central Mesa Verde region
utils::write.csv(region.occupation.household,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-occupation-by-household.csv',row.names=total.study.years)

##########################################################################################
## Calculate total population in region by smoothing region household occupation by average life-expectancy through time
total.study.years <- seq(year.start,year.end,year.duration)
region.occupation.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-occupation-by-household.csv',row.names=1)
# Proportionally decrease households from 1280--1300 to predicted numbers of total households to zero by 1300
region.occupation.household[(length(total.study.years)-20):length(total.study.years),] <- round(seq(region.occupation.household[(length(total.study.years)-20),],0,(-region.occupation.household[(length(total.study.years)-20),] / 20)))
# Create matrix for smoothed results of regional household population by life-expectancy
region.occupation.population <- matrix(NA,nrow=nrow(region.occupation.household),ncol=1)

## Smooth the results with corresponding average life-expectancy [@Kohler_and_Reese_2014] to determine range of population estimates for study period
smooth.start <- 450
smooth.end <- 600
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)
range.predictions <- as.numeric(region.occupation.household[(which(rownames(region.occupation.household) == as.character(smooth.start)):which(rownames(region.occupation.household) == as.character(smooth.end))),])
smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
region.occupation.population[(smooth.start-449):(smooth.end-449)] <- round(smooth.prediction)

smooth.start <- 601
smooth.end <- 1000
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)
range.predictions <- as.numeric(region.occupation.household[(which(rownames(region.occupation.household) == as.character(smooth.start)):which(rownames(region.occupation.household) == as.character(smooth.end))),])
smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
region.occupation.population[(smooth.start-449):(smooth.end-449)] <- round(smooth.prediction)

smooth.start <- 1001
smooth.end <- 1150
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)
range.predictions <- as.numeric(region.occupation.household[(which(rownames(region.occupation.household) == as.character(smooth.start)):which(rownames(region.occupation.household) == as.character(smooth.end))),])
smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
region.occupation.population[(smooth.start-449):(smooth.end-449)] <- round(smooth.prediction)

smooth.start <- 1151
smooth.end <- 1300
start <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.start),2]
end <- life.expectancy.by.year[which(life.expectancy.by.year[,1] == smooth.end),2]
window <- round(mean(c(start,end)),digits=0)
range.predictions <- as.numeric(region.occupation.household[(which(rownames(region.occupation.household) == as.character(smooth.start)):which(rownames(region.occupation.household) == as.character(smooth.end))),])
smooth.prediction <- smoother::smth.gaussian(range.predictions,window=window,tails=T)
region.occupation.population[(smooth.start-449):(smooth.end-449)] <- round(smooth.prediction)

# Proportionally decrease household population from 1280--1300 to predicted numbers of total occupied households to zero by 1300
region.occupation.population[(length(total.study.years)-20):length(total.study.years),] <- round(seq(region.occupation.population[(length(total.study.years)-20),],0,(-region.occupation.population[(length(total.study.years)-20),] / 20)))

# Export household results smoothed by average life-expectancy
utils::write.csv(region.occupation.population,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-occupation-by-population.csv',row.names=total.study.years)

##########################################################################################
##########################################################################################