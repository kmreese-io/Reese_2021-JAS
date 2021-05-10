##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## FIGURES ##
##########################################################################################
##########################################################################################
## FIGURE 1: EXAMPLE ARTIFICIAL NEURAL NETWORK

# Import example dataset created outside R environment
example.data <- as.data.frame(utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/figure-1-example-data.csv'))

# Define independent and dependent variables
example.independent <- base::paste(as.character(colnames(example.data[1:5])))
example.dependent <- base::paste(as.character(colnames(example.data[6:7])))

# Create formula
example.ann.formula <- stats::as.formula(paste(paste(example.dependent,collapse='+'),' ~ ',paste(example.independent,collapse='+')))

# Create example artificial neural network with the desired number of nodes for example figure
example.ann.model <- neuralnet::neuralnet(formula = example.ann.formula,     
                                          data = example.data,           
                                          threshold = 1,
                                          linear.output = F,
                                          hidden = 3,
                                          stepmax = 100000000,
                                          rep = 1
)

# Define labels of input variables and response variables for example figure
example.ann.model$model.list$variables <- c(' Input 1',' Input 2',' Input 3',' Input 4',' ')
example.ann.model$model.list$response <- c(' Predict 1',' ')

##########################################################################################
##########################################################################################
## Begin building Figure 1
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 1x.pdf',height=6,width=6)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

# Plot artificial neural network
graphics::plot(example.ann.model,rep='best',col.entry='black',col.entry.synapse='black',col.hidden='darkred',col.hidden.synapse='darkred',col.out='darkgreen',col.out.synapse='darkgreen',show.weights=F,arrow.length=0.175,radius=0.40,x.entry=0.2,x.out=0.8,fontsize=8,information=F,intercept=F,dimension=6)

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## FIGURE 2: OVERVIEW OF STUDY AREA WITH PUBLIC LANDS AND FOUR CORNERS SUBPLOT

##########################################################################################
##########################################################################################
## Begin building Figure 2
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 2.pdf')
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

# Plotting environment
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')

# Hillshade, DEM, Mesa Verde National Park boundary, and Canyons of the Ancients National Monument boundary
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(mvnp,col=scales::alpha('darkgreen',alpha=0.4),border=NA,add=T)
raster::plot(canm,col=scales::alpha('darkgreen',alpha=0.4),border=NA,add=T)

# Four Corners subplot with state boundaries
TeachingDemos::subplot(raster::plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Utah'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Colorado'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'New Mexico'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Arizona'),],border='black',col='gray75'))

# Extent of study region within Four Corners subplot with state boundaries
TeachingDemos::subplot(raster::plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(raster::plot(study.area.extent,border='black',lwd=1.5,xlim=c(550000,900000),ylim=c(3700000,4650000)),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(graphics::box(),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)

# Federal land labels
graphics::text(726000,4126000,'Mesa Verde National Park',col='darkgreen',cex=0.75)
graphics::text(685000,4145000,'Canyons of the Ancients',col='darkgreen',cex=0.75)
graphics::text(685000,4144000,'National Monument',col='darkgreen',cex=0.75)

# State labels within Four Corners subplot
graphics::text(raster::xmin(region.dem)+4000,raster::ymax(region.dem)-6000,'UT',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+7500,raster::ymax(region.dem)-6000,'CO',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+4000,raster::ymax(region.dem)-9000,'AZ',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+7500,raster::ymax(region.dem)-9000,'NM',cex=0.7,col='gray40')

# Scale
graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1000,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1000,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-16000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-11000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-11000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::text(raster::xmax(region.dem)-15900,raster::ymin(region.dem)+975,'0',cex=0.7,pos=2)
graphics::text(raster::xmax(region.dem)-6100,raster::ymin(region.dem)+975,'10 km',cex=0.7,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+750,raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,raster::xmax(region.dem)-1750,raster::ymin(region.dem)+6750,lwd=1.5)
graphics::text(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+4750,'N')

# Framing box around plot
graphics::box()

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## FIGURE 3: OPTIMAL NEURAL NETWORK MODEL ACCURACY (Y-AXIS) BY YEAR (X-AXIS) WITH UPPER AND LOWER CONFIDENCE INTERVAL, AND CORRESPONDING NUMBERS OF TREE RINGS WITHIN DATABASE FOR EACH YEAR

# Import tree ring database with corresponding year increment as used throughout the analysis
tree.rings.known <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-1.csv')

# Import balanced accuracy by year as recorded for the optimal final small sites model, and identify the optimal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]
window.iterations.annual <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss/smoothing-windows/annual/annual-accuracy-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.annual.ss <- window.iterations.annual[,2:ncol(window.iterations.annual)] * 100
index.best.ss <- node.optimal.position[1]

# Import the lower confidence interval by year corresponding with the optimal model parameters for small sites model
window.iterations.lower <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss/smoothing-windows/lower/annual-lower-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.lower.ss <- window.iterations.lower[,2:ncol(window.iterations.lower)] * 100

# Import the upper confidence interval by year corresponding with the optimal model parameters for small sites model
window.iterations.upper <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss/smoothing-windows/upper/annual-upper-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.upper.ss <- window.iterations.upper[,2:ncol(window.iterations.upper)] * 100

# Import balanced accuracy by year as recorded for the optimal final community center sites model, and identify the optimal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]
window.iterations.annual <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc/smoothing-windows/annual/annual-accuracy-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.annual.cc <- window.iterations.annual[,2:ncol(window.iterations.annual)] * 100
index.best.cc <- node.optimal.position[1]

# Import the lower confidence interval by year corresponding with the optimal model parameters for small sites model
window.iterations.lower <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc/smoothing-windows/lower/annual-lower-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.lower.cc <- window.iterations.lower[,2:ncol(window.iterations.lower)] * 100

# Import the upper confidence interval by year corresponding with the optimal model parameters for small sites model
window.iterations.upper <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc/smoothing-windows/upper/annual-upper-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent.upper.cc <- window.iterations.upper[,2:ncol(window.iterations.upper)] * 100

##########################################################################################
##########################################################################################
## Begin building Figure 3
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 3.pdf',height=5,width=9)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.5,0.6,0.10,0.6),oma=c(0.5,0.5,0.5,0.5))

# Plotting environment
graphics::plot(1,type="n",xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,105),xaxs="i",yaxs="i",axes=F,main='')

# Delineate refined Pueblo time periods as defined by @Bocinsky_et_al_2016
graphics::abline(v=c(500,700,890,1145,1285),col='gray30',lty=5,xpd=F)

# (PURPLE) Histogram of tree-ring cutting and near-cutting dates, by year, used in training the artificial neural network
graphics::hist(tree.rings.known$DATE_OUTSIDE,breaks=855,border='#5E4FA2',add=T)

# Call new plot
graphics::par(new=T)

# Plotting environment
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,105),xaxs='i',yaxs='i',axes=F,main='')

## Small Site model
# (GRAY) Plot shaded range between lower and upper confidence intervals
graphics::polygon(c(seq(year.start,year.end,year.duration),rev(seq(year.start,year.end,year.duration))),c(window.iterations.percent.lower.ss[index.best.ss,],rev(window.iterations.percent.upper.ss[index.best.ss,])),col=scales::alpha('gray',alpha=0.4),border=NA)

# (GRAY) Plot border lines of shaded range between lower and upper confidence intervals
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.lower.ss[index.best.ss,],type='l',col='darkgray',lwd=0.5)
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.upper.ss[index.best.ss,],type='l',col='darkgray',lwd=0.5)

# (RED) Plot annual accuracy of optimal model
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.annual.ss[index.best.ss,],type='l',col='#9E0142',lwd=1)

## Community Center model
# (GRAY) Plot shaded range between lower and upper confidence intervals
graphics::polygon(c(seq(year.start,year.end,year.duration),rev(seq(year.start,year.end,year.duration))),c(window.iterations.percent.lower.cc[index.best.cc,]-50,rev(window.iterations.percent.upper.cc[index.best.cc,]-50)),col=scales::alpha('gray',alpha=0.4),border=NA)

# (GRAY) Plot border lines of shaded range between lower and upper confidence intervals
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.lower.cc[index.best.cc,]-50,type='l',col='darkgray',lwd=0.5)
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.upper.cc[index.best.cc,]-50,type='l',col='darkgray',lwd=0.5)

# (RED) Plot annual accuracy of optimal model
graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent.annual.cc[index.best.cc,]-50,type='l',col='#9E0142',lwd=1)

# (1) Axes and labels for plotting environment
graphics::axis(1,at=seq(500,year.end,100),tick=T,labels=F,col='black')
graphics::mtext(as.character(seq(500,year.end,100)),side=1,line=0.5,at=seq(500,year.end,100),cex=0.75,col='black')
graphics::segments(450,0,1300,0,col='black',lwd=1,xpd=T)
graphics::mtext('Years (AD)',side=1,line=1.5,col='black')

# (2) Axes for plotting environment
graphics::axis(2,at=seq(0,100,25),tick=T,labels=F,col='#9E0142')

# (2a) Labels for small sites accuracy plot
graphics::mtext(as.character(c(75,100)),side=2,line=0.75,at=c(75,100),cex=0.75,las=2,col='#9E0142')
graphics::mtext('Small Sites Model',side=2,line=2.5,at=75,col='#9E0142')
graphics::mtext('Accuracy by Year (%)',side=2,line=1.75,at=75,col='#9E0142')

# (2b) Labels for community sites accuracy plot
graphics::mtext(as.character(c(75,100)),side=2,line=0.75,at=c(25,50),cex=0.75,las=2,col='#9E0142')
graphics::mtext('Village Sites Model',side=2,line=2.5,at=25,col='#9E0142')
graphics::mtext('Accuracy by Year (%)',side=2,line=1.75,at=25,col='#9E0142')

# (2) Axes and labels for plotting environment
# graphics::axis(2,at=seq(0,100,25),tick=T,labels=F,col='#9E0142')

# (4) Axes and labels for plotting environment
graphics::axis(4,at=seq(0,100,25),tick=T,labels=F,col='#5E4FA2')
graphics::mtext(as.character(seq(0,100,25)),side=4,line=0.75,at=seq(0,100,25),cex=0.75,las=2,col='#5E4FA2')
graphics::mtext('Cutting and Near-cutting Dates (n)',side=4,line=2.0,col='#5E4FA2')

# Labels for Pueblo periods, centered within refined Pueblo time periods as defined by @Bocinsky_et_al_2016 
graphics::text(500+((700-500)/2),105,'Basketmaker III',xpd=T)
graphics::text(700+((890-700)/2),105,'Pueblo I',xpd=T)
graphics::text(890+((1145-890)/2),105,'Pueblo II',xpd=T)
graphics::text(1145+((1285-1145)/2),105,'Pueblo III',xpd=T)

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## FIGURE 4: SIX-YEAR EXAMPLE RESULT (AD 1055--1060) OF OCCUPIED RESIDENCES, WITH WEIGHTED DENSITY BY NUMBER OF HOUSHEOLDS (ONLY INCLUDE RECORDED SITES WITHIN SURVEYED AREAS)  

# Import population-by-household for density plot of residences within surveyed areas, weighted by numbers of households
occupation.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')

# Define full range of years in study
year.start <- 450
year.end <- 1300
year.duration <- 1
year.sequence <- seq((year.start-7),1300,1)
year <- seq(year.start,year.end,year.duration)

# Subset households by full range of years in study
occupation.information <- occupation.household
column.start <- which(colnames(occupation.information) == paste('X',year.start,sep=''))
column.end <- which(colnames(occupation.information) == paste('X',year.end,sep=''))
occupation.information[,column.start:column.end] <- round(occupation.information[,column.start:column.end],digits=2)
occupation.data <- base::matrix(NA,nrow=nrow(occupation.information),ncol=2)
occupation.data[,1] <- occupation.information$X_COORDS
occupation.data[,2] <- occupation.information$Y_COORDS

# Calculate maximum range of densities across all study years for standardized example plots
density.extremes <- matrix(NA,ncol=2,nrow=length(column.start:column.end))
for(d in column.start:column.end) {
  
  dating.data <- occupation.information
  time.period <- dating.data[which(dating.data[,d] > 0 ),]
  
  if(nrow(time.period) == 0) {next}
  
  occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
  occupation.coords[,1] <- time.period$X_COORDS
  occupation.coords[,2] <- time.period$Y_COORDS
  occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
  occupation.coords <- occupation.coords[study.area.extent,]
  occupation.datums <- matrix(sp::coordinates(occupation.coords),ncol=2,byrow=FALSE)
  density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
  density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,d]),adjust=0.1)
  
  # Export density ranges of years in study
  density.extremes[d-(column.start-1),] <- c(min(density.plot),max(density.plot))
}

# Define range of example years
example.years.range <- seq(1055,1060,1)

##########################################################################################
##########################################################################################
## Begin building Figure 4
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 4.pdf',height=9,width=6)
graphics::par(mfrow=c(3,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

##########################################################################################
## Figure 4a: years 1 of 6 in household density plot

# Plot universals
example.year <- example.years.range[1]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##########################################################################################
## Figure 4b: years 2 of 6 in household density plot

# Plot universals
example.year <- example.years.range[2]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

## North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##########################################################################################
## Figure 4c: years 3 of 6 in household density plot

# Plot universals
example.year <- example.years.range[3]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##########################################################################################
## Figure 4d: years 4 of 6 in household density plot

# Plot universals
example.year <- example.years.range[4]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##########################################################################################
## Figure 4e: years 5 of 6 in household density plot

# Plot universals
example.year <- example.years.range[5]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##########################################################################################
## Figure 4f: years 6 of 6 in household density plot

# Plot universals
example.year <- example.years.range[6]
example.column <- example.year-450+9
dating.data <- occupation.information
time.period <- dating.data[which(dating.data[,(example.column)] > 0 ),]

# Create density plot
occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
occupation.coords[,1] <- time.period$X_COORDS
occupation.coords[,2] <- time.period$Y_COORDS
occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
occupation.coords <- occupation.coords[study.area.extent,]
occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,example.column]),adjust=0.1)

# Plotting environment, hillshade and DEM background, and standardized density plot
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs='i',yaxs='i',axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.6,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.6,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=1)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(example.year),col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## FIGURE 5: PREDICTIVE MODEL EXAMPLE FOR RESIDENTIAL SITE EXTRAPOLATION - AD 1060 WITH FOUR PITSTRUCTURES

# Import population-by-household for use in predictive model
occupation.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')

# Define example year, example site size, and subset population-by-household with example parameters
sample.year <- 1060
start <- sample.year - 410 - 20
end <- sample.year - 410
column <- which(colnames(occupation.household) == paste('X',sample.year,sep=''))
dating.data <- occupation.household[which(occupation.household[,column] > 0 ),]
dating.data <- dating.data[which(dating.data$PITSTRUCTURES == 4 & dating.data$CENTER == 0),]

# Create SpatialPointsDataFrame with example sites
occupation.coords <- base::matrix(NA,nrow=nrow(dating.data),ncol=2)
occupation.coords[,1] <- dating.data$X_COORDS
occupation.coords[,2] <- dating.data$Y_COORDS
known.coordinates <- sp::SpatialPointsDataFrame(coords=occupation.coords,dating.data,proj4string=master.projection)

# Rasters of surveyed areas for masking in predictive model
surveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-surveyed')
unsurveyed.area <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/area-unsurveyed')

# Import topographic information for predictive layers 2--4 (region.dem is predictive layer 1), cost-surfaces for predictive layers 5--6, and environmental data for layers 7--9
region.slope <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-slope')
region.aspect <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-aspect')
region.flowdir <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/vepii-n-flowdir')
cost.drainages <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-drainages')
cost.rivers <- raster::raster('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/COST-RASTERS/NORTHERN-SAN-JUAN/cost-to-rivers')
growing.niche <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/growing-niche')
raster::projection(growing.niche) <- master.projection
temperature <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/temperature')
raster::projection(temperature) <- master.projection
precipitation <- raster::brick('/Users/kmreese/Documents/PROJECTS/DATABASE/SPATIAL/DEM/REGIONS/NORTHERN-SAN-JUAN/precipitation')
raster::projection(precipitation) <- master.projection

# Create predictive raster stack with universal layers 1--6
# Prediction layer: Elevation
layer.1 <- raster::crop(region.dem,study.area.extent)
# Prediction layer: Slope
layer.2 <- raster::crop(region.slope,study.area.extent)
# Prediction layer: Aspect
layer.3 <- raster::crop(region.aspect,study.area.extent)
# Prediction layer: Flow direction
layer.4 <- raster::crop(region.flowdir,study.area.extent)
# Prediction layer: Distance to ephemeral water sources
layer.5 <- raster::crop(cost.drainages,study.area.extent)
# Prediction layer: Distance to permanent water sources
layer.6 <- raster::crop(cost.rivers,study.area.extent)
# Prediction layer: Average annual growing degree days (temperature), average previous 20 years
layer.7 <- raster::stackApply(temperature[[start:end]],indices=nlayers(temperature[[start:end]]),fun='mean')
# Prediction layer: Average annual precipitation, average previous 20 years
layer.8 <- raster::stackApply(precipitation[[start:end]],indices=nlayers(precipitation[[start:end]]),fun='mean')
# Prediction layer: Average annual maize growing niche, average previous 20 years
layer.9 <- raster::stackApply(growing.niche[[start:end]],indices=nlayers(growing.niche[[start:end]]),fun='mean')
# Universal predictive raster stack
raster.stack <- raster::stack(layer.1,layer.2,layer.3,layer.4,layer.5,layer.6,layer.7,layer.8,layer.9)
# Train the example predictive model
max.entropy <- dismo::maxent(raster.stack,sp::coordinates(known.coordinates),removeDuplicates=F)
raster.probabilities <- dismo::predict(max.entropy,raster.stack)
# Predictive example raster: AD 1060 with four pitstructures
predictive.example <- raster::writeRaster(raster.probabilities,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/predictive-example',overwrite=T)
predictive.example <- raster::raster('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/predictive-example')
raster::projection(predictive.example) <- master.projection

# Predictive raster masked by surveyed and unsurveyed areas
predictive.example.surveyed <- raster::mask(predictive.example,surveyed.area)
predictive.example.unsurveyed <- raster::mask(predictive.example,unsurveyed.area)

##########################################################################################
##########################################################################################
## Begin building Figure 5
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 5.pdf',height=6,width=6)
graphics::par(mfrow=c(2,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

##############################################################
## Figure 5a: DEM with hillshade, shaded areas of previous survey, and recorded habitation sites with 4 pitstructures occupied in AD 1060

# Plotting environment, hillshade and DEM background, example site coordinates, and previously-surveyed polygons
graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(surveyed.area,col=scales::alpha('gray40',alpha=0.6),border=NA,add=T,axes=F,legend=F)

# Known site locations
graphics::points(coordinates(occupation.coords),cex=0.5,pch=3)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.5,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.5,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=0.7)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,'a',col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##############################################################
## Figure 5b: DEM with hillshade, full extent of predictive model, and recorded habitation sites with 4 pitstructures occupied in AD 1060

# Plotting environment, hillshade and DEM background, example site coordinates, and example predictive raster
graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(predictive.example,col=scales::alpha(heatcolors,alpha=0.80),add=T,axes=F,legend=F)
graphics::points(coordinates(occupation.coords),cex=0.5,pch=3)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.5,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.5,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=0.7)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,'b',col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##############################################################
## Figure 5c: DEM with hillshade, predictive model cropped to surveyed areas, and recorded habitation sites with 4 pitstructures occupied in AD 1060

# Plotting environment, hillshade and DEM background, example site coordinates, and example predictive raster showing only values within previously-surveyed areas
graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(predictive.example.surveyed,col=scales::alpha(heatcolors,alpha=0.80),add=T,axes=F,legend=F)
graphics::points(coordinates(occupation.coords),cex=0.5,pch=3)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.5,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.5,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=0.7)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,'c',col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

##############################################################
## Figure 5d: DEM with hillshade, predictive model cropped to unsurveyed areas

# Plotting environment, hillshade and DEM background, example site coordinates, and example predictive raster showing only values within previously-unsurveyed areas
graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(predictive.example.unsurveyed,col=scales::alpha(heatcolors,alpha=0.80),add=T,axes=F,legend=F)
graphics::points(coordinates(occupation.coords),cex=0.5,pch=3)

# Scale
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2500,lwd=1)
graphics::segments(raster::xmax(region.dem)-19000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-19000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-14000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-14000,raster::ymin(region.dem)+2000,lwd=1)
graphics::segments(raster::xmax(region.dem)-9000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-9000,raster::ymin(region.dem)+2000,lwd=1)
graphics::text(raster::xmax(region.dem)-18000,raster::ymin(region.dem)+2250,'0',cex=0.5,pos=2)
graphics::text(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2250,'10 km',cex=0.5,pos=4)

# North arrow
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-3000,raster::ymin(region.dem)+10500,lwd=1)
graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N',cex=0.7)

# Plot identification text
graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,'d',col='white',pos=4,cex=2,offset=0)

# Framing box around plot
graphics::box()

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## FIGURE 6: DEMOGRAPHICICS WITH KNOWN OCCUPIED HOUSEHOLDS, AND TOTAL POPULATION RECONSTRUCTION

# VEP II midpoints of modeling periods
vepii.midpoints <- c(600,662.5,762.5,820,860,900,950,1000,1040,1080,1120,1160,1202.5,1242.5,1270,1300)

# Reconstructed total households from VEP II results folllowing methods reported in @Schwindt_et_al_2016, but using extrapolation method presented in this project
vepii.calculated.households <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/vepii-extrapolated-households.csv',row.names=1)
vepii.calculated.households <- c(0,as.numeric(unlist(vepii.calculated.households)),0)

# Final household occupation predictions by recorded residential site
occupation.household <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-household.csv')
occupation.household[is.na(occupation.household)] <- 0

# Final population predictions with extrapolated results and smoothed by life-expectancy (file shows households, must be multiplied by 3, 5, and 7 for total range of potential numbers of people)
region.occupation.population <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final/region-occupation-by-population.csv',row.names=1)

##########################################################################################
##########################################################################################
## Begin building Figure 6
grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 6.pdf',height=5,width=9)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.5,0.6,0.10,0.6),oma=c(0.5,0.5,0.5,0.5))

# Plotting environment
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(year.start,year.end+5),ylim=c(0,30500),xaxs='i',yaxs='i',axes=F,main='')

# Pueblo time periods refined by this analysis
graphics::abline(v=c(470,710,890,1145,1295),col='gray30',lty=5,lwd=1.5,xpd=F)

# Periods of transition from exploration to exploitation, identified by results presented here, following @Bocinsky_et_al_2016
graphics::abline(v=c(600,790,1035,1200),col='gray',lty=3,lwd=1.5,xpd=F)

# (PURPLE) VEP II results, with reported informal 80% confidence intervals, plotted by midpoints of modeling periods
graphics::polygon(c(vepii.midpoints,rev(vepii.midpoints)),c((vepii.calculated.households * 7),rev(vepii.calculated.households * 3.3)),col=scales::alpha('#5E4FA2',alpha=0.15),border=NA)
graphics::lines(vepii.midpoints,(vepii.calculated.households * 6),col=scales::alpha('#5E4FA2',alpha=0.4),lwd=1.5)

# (GRAY) Final population predictions with extrapolated results and smoothed by life-expectancy (multiplied by 3.3 and 6)
graphics::polygon(c(seq(year.start,year.end,year.duration),rev(seq(year.start,year.end,year.duration))),c((region.occupation.population[,1] * 7),rev((region.occupation.population[,1] * 3.3))),col=scales::alpha('gray75',alpha=0.60),border=NA)

# Call new plot
graphics::par(new=T)

# Plotting environment
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(year.start,year.end+5),ylim=c(0,3500),xaxs='i',yaxs='i',axes=F,main='')

# (RED) Raw numbers of known habitation sites predicted to be occupied per year by the artificial neural network
graphics::lines(seq(year.start,year.end,year.duration),c(colSums(occupation.household[,9:ncol(occupation.household)])),col='#9E0142',lwd=1)

# (4) Axes and labels for plotting environment
graphics::axis(4,at=seq(0,3500,1000),col='#9E0142',tick=T,labels=F)
graphics::mtext(as.character(seq(0,3500,1000)),at=as.character(seq(0,3500,1000)),col='#9E0142',side=4,line=0.75,cex=0.75,las=2)
graphics::mtext('Recorded Occupied Residences',col='#9E0142',side=4,line=2.5)

# Call new plot
graphics::par(new=T)

# Plotting environment
graphics::plot(1,type='n',xlab='',ylab='',xlim=c(year.start,year.end+5),ylim=c(0,30500),xaxs='i',yaxs='i',axes=F,main='')

# (BLACK) Final population predictions with extrapolated results and smoothed by life-expectancy (multiplied by 6 people per household)
graphics::lines(seq(year.start,year.end,year.duration),(region.occupation.population[,1] * 6),col='black',lwd=3)

# (1) Axes and labels for plotting environment
graphics::axis(1,at=seq(500,year.end,100),tick=T,labels=F,col='black')
graphics::mtext(as.character(seq(500,year.end,100)),side=1,line=0.5,at=seq(500,1300,100),cex=0.75,col='black')
graphics::mtext('Years (AD)',side=1,line=1.5,col='black')

# (2) Axes and labels for plotting environment
graphics::axis(2,at=seq(0,30500,5000),tick=T,labels=F)
graphics::mtext(as.character(seq(0,30500,5000)),at=as.character(seq(0,30500,5000)),side=2,line=0.75,cex=0.75,las=2)
graphics::mtext('Total Regional Population',side=2,line=2.5)

# Labels for plot
graphics::text(460+((710-460)/2),30500,'Basketmaker III',xpd=T)
graphics::text(710+((900-710)/2),30500,'Pueblo I',xpd=T)
graphics::text(900+((1150-900)/2),30500,'Pueblo II',xpd=T)
graphics::text(1150+((1295-1150)/2),30500,'Pueblo III',xpd=T)

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## Figure SI: Annual household density from AD 450--1300, standardized density range across years

occupation.population <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/occupation-by-population.csv')

year.start <- 450
year.end <- 1300
year.duration <- 1

year.sequence <- seq((year.start-8),1300,1)

occupation.information <- occupation.population
column.start <- which(colnames(occupation.information) == paste('X',year.start,sep=''))
column.end <- which(colnames(occupation.information) == paste('X',year.end,sep=''))
occupation.information[,column.start:column.end] <- round(occupation.information[,column.start:column.end],digits=2)
occupation.data <- base::matrix(NA,nrow=nrow(occupation.information),ncol=2)
occupation.data[,1] <- occupation.information$X_COORDS
occupation.data[,2] <- occupation.information$Y_COORDS

year <- seq(year.start,year.end,year.duration)

density.extremes <- matrix(NA,ncol=2,nrow=length(column.start:column.end))

for(d in column.start:column.end) {
  
  dating.data <- occupation.information
  time.period <- dating.data[which(dating.data[,d] > 0 ),]
  
  if(nrow(time.period) == 0) {next}
  
  occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
  occupation.coords[,1] <- time.period$X_COORDS
  occupation.coords[,2] <- time.period$Y_COORDS
  occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
  occupation.coords <- occupation.coords[study.area.extent,]
  
  occupation.datums <- matrix(sp::coordinates(occupation.coords),ncol=2,byrow=FALSE)
  density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
  density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,d]),adjust=0.1)
  
  density.extremes[d-(column.start-1),] <- c(min(density.plot),max(density.plot))
}

##########################################################################################
##########################################################################################
## Begin building Supplemental Material 1

for(i in column.start:column.end) {
  
  create.empty.plot <- FALSE
  
  dating.data <- occupation.information
  time.period <- dating.data[which(dating.data[,i] > 0 ),]
  
  occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
  occupation.coords[,1] <- time.period$X_COORDS
  occupation.coords[,2] <- time.period$Y_COORDS
  
  tryCatch({
    
    occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection) },
    
    error = function(e) { create.empty.plot <<- TRUE})
  
  if(create.empty.plot == TRUE) {
    
    grDevices::pdf(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-population-density-standardized/',names(time.period[i]),'.pdf',sep=''))
    graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))
    
    graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
    raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
    raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
    
    graphics::rect(raster::xmin(region.dem),raster::ymin(region.dem),raster::xmax(region.dem),raster::ymax(region.dem),col=scales::alpha("#5E4FA2",alpha=0.4))
    
    graphics::text(raster::xmin(region.dem)-1000,raster::ymin(region.dem)+2000,as.character(year.sequence[i]),col='white',pos=4,cex=4)
    
    # Scale
    graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1000,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1000,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-16000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-11000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-11000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::text(raster::xmax(region.dem)-15900,raster::ymin(region.dem)+975,'0',cex=0.7,pos=2)
    graphics::text(raster::xmax(region.dem)-6100,raster::ymin(region.dem)+975,'10 km',cex=0.7,pos=4)
    
    # North arrow
    graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+750,raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,raster::xmax(region.dem)-1750,raster::ymin(region.dem)+6750,lwd=1.5)
    graphics::text(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+4750,'N')
    
    # Framing box around plot
    graphics::box()
    
    # Finish building figure
    grDevices::dev.off()
    
  }
  
  else{
    
    occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
    occupation.coords <- occupation.coords[study.area.extent,]
    
    occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
    density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
    density.plot <- stats::density(density.points,weights=as.numeric(occupation.coords@data[,i]),adjust=0.1)
    
    grDevices::pdf(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-population-density-standardized/',names(time.period[i]),'.pdf',sep=''))
    graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))
    
    graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
    raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
    raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
    
    raster::plot(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),col=scales::alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
    graphics::contour(density.plot,zlim=c(min(density.extremes,na.rm=T),max(density.extremes,na.rm=T)),nlevels=5,add=T,axes=F,legend=F,labels='',lwd=0.5)
    
    graphics::text(raster::xmin(region.dem)-1000,raster::ymin(region.dem)+2000,as.character(year.sequence[i]),col='white',pos=4,cex=4)
    
    # Scale
    graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1000,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1000,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-16000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-11000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-11000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+750,lwd=1.5)
    graphics::text(raster::xmax(region.dem)-15900,raster::ymin(region.dem)+975,'0',cex=0.7,pos=2)
    graphics::text(raster::xmax(region.dem)-6100,raster::ymin(region.dem)+975,'10 km',cex=0.7,pos=4)
    
    # North arrow
    graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+750,raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,lwd=1.5)
    graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,raster::xmax(region.dem)-1750,raster::ymin(region.dem)+6750,lwd=1.5)
    graphics::text(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+4750,'N')
    
    # Framing box around plot
    graphics::box()
    
    # Finish building figure
    grDevices::dev.off()
    
  }
  
}

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
## Figure SI: Area Under the Curve

## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.ss.1

## Identify optimal model parameters based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-ss-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

x <- dataset

set.seed(471919)
split <- caTools::sample.split(x,SplitRatio=0.8)
training.set.original <- base::subset(x,split == T)
training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
test.set.original <- base::subset(x,split == F)
test.set.original <- test.set.original[which(!is.na(test.set.original[,1])),]

# Identify columns with predictive variables (ceramic data)
variable.columns <- length(2:which(colnames(test.set.original) == paste('X',year.start,sep='')))

# Remove rows with no instances of predictive variables (no ceramic tallies)
training.set.original <- training.set.original[rowSums(training.set.original[,2:variable.columns]) != 0 , ]
test.set.original <- test.set.original[rowSums(test.set.original[,2:variable.columns]) != 0 , ]

# Normalize the ceramic data across rows in training set - normalizing by SITE_ID
training.normalized.matrix <- matrix(NA,nrow=nrow(training.set.original),ncol=(variable.columns-1))
for(i in 1:nrow(training.set.original)) {
  normalized.row <- as.matrix(normalize(training.set.original[i,2:(variable.columns)]))
  training.normalized.matrix[i,] <- normalized.row
}
training.set <- as.data.frame(cbind(training.set.original[,1],unlist(training.normalized.matrix[,1:(variable.columns-1)]),training.set.original[(variable.columns+1):ncol(training.set.original)]))
colnames(training.set) <- c(base::paste(as.character(colnames(x))))

# Normalize the ceramic data across rows in test set - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(variable.columns-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:(variable.columns)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original[,1],unlist(test.normalized.matrix[,1:(variable.columns-1)]),test.set.original[(variable.columns+1):ncol(test.set.original)]))
colnames(test.set) <- c(base::paste(as.character(colnames(x))))

# Replace any NaNs with zeros
training.set[is.na(training.set)] <- 0
test.set[is.na(test.set)] <- 0

# Load optimal artificial neural network
ann.model <- readRDS('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/ss-artificial-neural-network.rds')

# Use neural network model to calculate site predictions and combine with known cutting dates
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set[,1:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(test.set.original)
predictions.SITE_NO <- base::merge(tree.rings.aggregated,predictions.SITE_ID,by='SITE_ID')

# Import table of diagnostic ceramic types and corresponding date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create tables to save calculations
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)
smoothed.accuracy <- matrix(NA,nrow=1,ncol=50)
smoothed.accuracy.balanced <- matrix(NA,nrow=1,ncol=50)

# Bound any extreme site predictions by maximum ceramic ranges of production
for(i in 1:nrow(test.set)) {
  
  site.predictions <- predictions.SITE_NO[i,]
  site.row <- test.set[i,2:variable.columns]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- diagnostic.ceramics[c(limit.types),]
  
  date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
  date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
  out.of.date.range <- seq(year.start,year.end,year.duration)
  out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
  
  total.range.predicted <- predictions.SITE_NO[i,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.y',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.y',sep='')))]
  ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,'.y',sep='')]
  normalize.new.range <- normalize(ceramic.range.predicted)
  
  combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
  order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
  
  isolate.range.predictions <- unname(order.range.predictions[,2])
  ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
  
}

# Smooth results with optimal smoothing window
smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,2:ncol(ceramic.informed.occupation.ranges.matrix)],window=window.optimal,tails=T) } )
smoothed.occupation.ranges.matrix <- as.matrix(t(smoothing.range.predictions))
ceramic.smoothed.occupation <- as.data.frame(cbind(ceramic.informed.occupation.ranges.matrix[,1],smoothed.occupation.ranges.matrix))
names(ceramic.smoothed.occupation) <- c('SITE_ID',paste('X',seq(year.start,year.end,year.duration),'.y',sep=''))

# Organize actual/predicted occupations for accuracy calculations
presence.absence.actual <- predictions.SITE_NO[,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.x',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.x',sep='')))]
presence.absence.actual[presence.absence.actual > 0] <- 1
actual <- presence.absence.actual
predictions <- ceramic.smoothed.occupation[,2:ncol(ceramic.smoothed.occupation)]

# Calculate Area Under the Curve
res.roc <- pROC::roc(as.numeric(as.matrix(actual)),as.numeric(as.matrix(predictions)))

##########################################################################################
##########################################################################################
## Begin building Supplemental Figure 1

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Supplemental Figure 1.pdf')
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

# Plotting environment
pROC::plot.roc(res.roc,print.auc=T,main='Area Under the Curve (AUC)')

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################

## Universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.cc.1

## Identify optimal model parameters based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-cc-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

x <- dataset

set.seed(471919)
split <- caTools::sample.split(x,SplitRatio=0.8)
training.set.original <- base::subset(x,split == T)
training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
test.set.original <- base::subset(x,split == F)
test.set.original <- test.set.original[which(!is.na(test.set.original[,1])),]

# Identify columns with predictive variables (ceramic data)
variable.columns <- length(2:which(colnames(test.set.original) == paste('X',year.start,sep='')))

# Remove rows with no instances of predictive variables (no ceramic tallies)
training.set.original <- training.set.original[rowSums(training.set.original[,2:variable.columns]) != 0 , ]
test.set.original <- test.set.original[rowSums(test.set.original[,2:variable.columns]) != 0 , ]

# Normalize the ceramic data across rows in training set - normalizing by SITE_ID
training.normalized.matrix <- matrix(NA,nrow=nrow(training.set.original),ncol=(variable.columns-1))
for(i in 1:nrow(training.set.original)) {
  normalized.row <- as.matrix(normalize(training.set.original[i,2:(variable.columns)]))
  training.normalized.matrix[i,] <- normalized.row
}
training.set <- as.data.frame(cbind(training.set.original[,1],unlist(training.normalized.matrix[,1:(variable.columns-1)]),training.set.original[(variable.columns+1):ncol(training.set.original)]))
colnames(training.set) <- c(base::paste(as.character(colnames(x))))

# Normalize the ceramic data across rows in test set - normalizing by SITE_ID
test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(variable.columns-1))
for(i in 1:nrow(test.set.original)) {
  normalized.row <- as.matrix(normalize(test.set.original[i,2:(variable.columns)]))
  test.normalized.matrix[i,] <- normalized.row
}
test.set <- as.data.frame(cbind(test.set.original[,1],unlist(test.normalized.matrix[,1:(variable.columns-1)]),test.set.original[(variable.columns+1):ncol(test.set.original)]))
colnames(test.set) <- c(base::paste(as.character(colnames(x))))

# Replace any NaNs with zeros
training.set[is.na(training.set)] <- 0
test.set[is.na(test.set)] <- 0

# Load optimal artificial neural network
ann.model <- readRDS('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/cc-artificial-neural-network.rds')

# Use neural network model to calculate site predictions and combine with known cutting dates
predictions <- stats::predict(object=ann.model,newdata=test.set)
predictions.SITE_ID <- base::as.data.frame(base::cbind(as.matrix(test.set[,1:variable.columns]),predictions))
names(predictions.SITE_ID) <- names(test.set.original)
predictions.SITE_NO <- base::merge(tree.rings.aggregated,predictions.SITE_ID,by='SITE_ID')

# Import table of diagnostic ceramic types and corresponding date ranges
nsj.ceramics <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/DATABASE/TABLES/database-nsj-ceramics-date-ranges.csv')
diagnostic.ceramics <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),(ncol(nsj.ceramics)-1):ncol(nsj.ceramics)]
diagnostic.ceramics.names <- nsj.ceramics[which(nsj.ceramics$DIAGNOSTIC == TRUE ),1]
rownames(diagnostic.ceramics) <- diagnostic.ceramics.names

# Create tables to save calculations
ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)
smoothed.accuracy <- matrix(NA,nrow=1,ncol=50)
smoothed.accuracy.balanced <- matrix(NA,nrow=1,ncol=50)

# Bound any extreme site predictions by maximum ceramic ranges of production
for(i in 1:nrow(test.set)) {
  
  site.predictions <- predictions.SITE_NO[i,]
  site.row <- test.set[i,2:variable.columns]
  limit.types <- colnames(site.row[which(site.row[1,1:ncol(site.row)] > 0)])
  type.names <- diagnostic.ceramics[c(limit.types),]
  
  date.range.minimum <- ifelse(plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor) < year.start,year.start,plyr::round_any(min(c(type.names$START,type.names$END)),year.duration,f=floor))
  date.range <- seq(date.range.minimum,plyr::round_any(max(c(type.names$START,type.names$END)),year.duration,f=floor),year.duration)
  out.of.date.range <- seq(year.start,year.end,year.duration)
  out.of.date.range <- out.of.date.range[!out.of.date.range %in% date.range]
  
  total.range.predicted <- predictions.SITE_NO[i,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.y',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.y',sep='')))]
  ceramic.range.predicted <- total.range.predicted[colnames(total.range.predicted) %in% paste('X',date.range,'.y',sep='')]
  normalize.new.range <- normalize(ceramic.range.predicted)
  
  combine.range.predictions <- rbind(cbind(date.range,t(normalize.new.range)),cbind(out.of.date.range,rep(0,times=length(out.of.date.range))))
  order.range.predictions <- combine.range.predictions[order(combine.range.predictions[,1]),]
  
  isolate.range.predictions <- unname(order.range.predictions[,2])
  ceramic.informed.occupation.ranges.matrix[i,] <- c(site.predictions[1,1],isolate.range.predictions)
  
}

# Smooth results with optimal smoothing window
smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,2:ncol(ceramic.informed.occupation.ranges.matrix)],window=window.optimal,tails=T) } )
smoothed.occupation.ranges.matrix <- as.matrix(t(smoothing.range.predictions))
ceramic.smoothed.occupation <- as.data.frame(cbind(ceramic.informed.occupation.ranges.matrix[,1],smoothed.occupation.ranges.matrix))
names(ceramic.smoothed.occupation) <- c('SITE_ID',paste('X',seq(year.start,year.end,year.duration),'.y',sep=''))

# Organize actual/predicted occupations for accuracy calculations
presence.absence.actual <- predictions.SITE_NO[,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.x',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.x',sep='')))]
presence.absence.actual[presence.absence.actual > 0] <- 1
actual <- presence.absence.actual
predictions <- ceramic.smoothed.occupation[,2:ncol(ceramic.smoothed.occupation)]

# Calculate Area Under the Curve
res.roc <- pROC::roc(as.numeric(as.matrix(actual)),as.numeric(as.matrix(predictions)))

##########################################################################################
##########################################################################################
## Begin building Supplemental Figure 2

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Supplemental Figure 2.pdf')
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

# Plotting environment
pROC::plot.roc(res.roc,print.auc=T,main='Area Under the Curve (AUC)')

# Finish building figure
grDevices::dev.off()

##########################################################################################
##########################################################################################
