##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## FIGURES ##

##########################################################################################
##########################################################################################
## Figure 1: Example artificial neural network

example.data <- as.data.frame(utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/figure-1-example-data.csv'))

# Define independent and dependent variables
example.independent <- base::paste(as.character(colnames(example.data[1:5])))
# example.dependent <- base::paste(as.character(colnames(example.data[6:ncol(example.data)])))
example.dependent <- base::paste(as.character(colnames(example.data[6:7])))

# Create formula
example.ann.formula <- stats::as.formula(paste(paste(example.dependent,collapse='+'),' ~ ',paste(example.independent,collapse='+')))

example.ann.model <- neuralnet::neuralnet(formula = example.ann.formula,     
                                          data = example.data,           
                                          threshold = 1,
                                          linear.output = F,
                                          hidden = 3,
                                          stepmax = 100000000,
                                          rep = 1
)

example.ann.model$model.list$response <- c('Result 1','Result 2','Result 3')
example.ann.model$model.list$variables <- c('Input 1','Input 2','Input 3','Input 4','Input 5')

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 1.pdf',height=6,width=6)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

graphics::plot(example.ann.model,rep='best',col.entry='black',col.entry.synapse='black',col.hidden='darkred',col.hidden.synapse='darkred',col.out='darkgreen',col.out.synapse='darkgreen',show.weights=F,fontsize=8,information=F,intercept=F,dimension=6)

grDevices::dev.off()

##########################################################################################
##########################################################################################
## Figure 2: Overview of study area with public lands and Four Corners subplot

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 2.pdf')
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
raster::plot(mvnp,col=scales::alpha('darkgreen',alpha=0.4),border=NA,add=T)
raster::plot(canm,col=scales::alpha('darkgreen',alpha=0.4),border=NA,add=T)

## Four Corners inset and boundary of study region
TeachingDemos::subplot(raster::plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Utah'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Colorado'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'New Mexico'),],border='black',col='gray75'))
TeachingDemos::subplot(raster::plot(state.boundaries[which(state.boundaries$STATE_NAME == 'Arizona'),],border='black',col='gray75'))

TeachingDemos::subplot(raster::plot(state.boundaries,xlim=c(550000,900000),ylim=c(3700000,4650000),col='gray75',border='gray40'),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(raster::plot(study.area.extent,border='black',lwd=1.5,xlim=c(550000,900000),ylim=c(3700000,4650000)),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)
TeachingDemos::subplot(graphics::box(),x=raster::xmin(region.dem),y=raster::ymax(region.dem),size=c(1.25,1.25),vadj=1,hadj=0)

## Labels
graphics::text(726000,4126000,'Mesa Verde National Park',col='darkgreen',cex=0.75)
graphics::text(685000,4145000,'Canyon of the Ancients',col='darkgreen',cex=0.75)
graphics::text(685000,4144000,'National Monument',col='darkgreen',cex=0.75)
graphics::text(raster::xmin(region.dem)+4000,raster::ymax(region.dem)-6000,'UT',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+7500,raster::ymax(region.dem)-6000,'CO',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+4000,raster::ymax(region.dem)-9000,'AZ',cex=0.7,col='gray40')
graphics::text(raster::xmin(region.dem)+7500,raster::ymax(region.dem)-9000,'NM',cex=0.7,col='gray40')

## Scale
graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1000,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1000,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-16000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-16000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-11000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-11000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-6000,raster::ymin(region.dem)+1250,raster::xmax(region.dem)-6000,raster::ymin(region.dem)+750,lwd=1.5)
graphics::text(raster::xmax(region.dem)-15900,raster::ymin(region.dem)+975,'0',cex=0.7,pos=2)
graphics::text(raster::xmax(region.dem)-6100,raster::ymin(region.dem)+975,'10 km',cex=0.7,pos=4)

## North arrow
graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+750,raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,lwd=1.5)
graphics::segments(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+8750,raster::xmax(region.dem)-1750,raster::ymin(region.dem)+6750,lwd=1.5)
graphics::text(raster::xmax(region.dem)-1000,raster::ymin(region.dem)+4750,'N')

graphics::box()

grDevices::dev.off()

##########################################################################################
##########################################################################################
## Figure 3: Neural network accuracy (y-axis) results for all numbers of nodes by year (x-axis), with chosen model highlighted (confidence interval?)

tree.rings.known <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/tree-rings-final-1.csv')

node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-mean/accuracy-ceramics-smoothed-mean.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:100)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]
window.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'/smoothing-windows/accuracy-ceramics-smoothed-',window.optimal,'.csv',sep=''),header=F)
window.iterations.percent <- window.iterations[,2:ncol(window.iterations)] * 100
index.best <- node.optimal.position[1]

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 3.pdf',height=5,width=9)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.5,0.6,0.10,0.6),oma=c(0.5,0.5,0.5,0.5))

graphics::plot(1,type="n",xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,105),xaxs="i",yaxs="i",axes=F,main='')

graphics::abline(v=c(500,700,890,1145,1285),col='gray30',lty=5,xpd=F)

for(p in 1:nrow(window.iterations.percent)) {
  points(seq(year.start,year.end,year.duration),window.iterations.percent[p,],type='l',lwd=0.1,col=scales::alpha('gray',alpha=0.4),add=T)
}

graphics::lines(seq(year.start,year.end,year.duration),window.iterations.percent[index.best,],type='l',col='#9E0142',lwd=2)

graphics::par(new=T)

graphics::plot(1,type="n",xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,105),xaxs="i",yaxs="i",axes=F,main='')

graphics::hist(tree.rings.known$DATE_OUTSIDE,breaks=855,border='#5E4FA2',add=T)

graphics::axis(1,at=seq(500,year.end,100),tick=T,labels=F,col='black')
graphics::mtext(as.character(seq(500,year.end,100)),side=1,line=0.5,at=seq(500,year.end,100),cex=0.75,col='black')
graphics::mtext('Years (AD)',side=1,line=1.5,col='black')

graphics::axis(2,at=seq(0,100,25),tick=T,labels=F,col='#9E0142')
graphics::mtext(as.character(seq(0,100,25)),side=2,line=0.75,at=seq(0,100,25),cex=0.75,las=2,col='#9E0142')
graphics::mtext('Model Accuracy by Year (%)',side=2,line=2.0,col='#9E0142')

graphics::axis(4,at=seq(0,100,25),tick=T,labels=F,col='#5E4FA2')
graphics::mtext(as.character(seq(0,100,25)),side=4,line=0.75,at=seq(0,100,25),cex=0.75,las=2,col='#5E4FA2')
graphics::mtext('Cutting and Near-cutting Dates (n)',side=4,line=2.0,col='#5E4FA2')

graphics::text(500+((700-500)/2),105,'Basketmaker III',xpd=T)
graphics::text(700+((890-700)/2),105,'Pueblo I',xpd=T)
graphics::text(890+((1145-890)/2),105,'Pueblo II',xpd=T)
graphics::text(1145+((1285-1145)/2),105,'Pueblo III',xpd=T)

grDevices::dev.off()

##########################################################################################
##########################################################################################
## Figure 4: Six year example result with density plots across central Mesa Verde region

sample.year.start <- 1060
sample.year.end <- 1065
sample.year.duration <- 1

year.sequence <- seq((year.start-7),1300,1)

occupation.information <- occupation.households
column.start <- which(colnames(occupation.information) == paste('X',sample.year.start,sep=''))
column.end <- which(colnames(occupation.information) == paste('X',sample.year.end,sep=''))
occupation.information[,column.start:column.end] <- round(occupation.information[,column.start:column.end],digits=2)
occupation.data <- base::matrix(NA,nrow=nrow(occupation.information),ncol=2)
occupation.data[,1] <- occupation.information$X_COORDS
occupation.data[,2] <- occupation.information$Y_COORDS

year <- seq(sample.year.start,sample.year.end,sample.year.duration)

density.extremes <- matrix(NA,ncol=2,nrow=4)
q <- 0

for(d in c(seq(column.start,column.end,sample.year.duration))) {
  
  dating.data <- occupation.information
  time.period <- dating.data[which(dating.data[,d] > 0 ),]
  occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
  occupation.coords[,1] <- time.period$X_COORDS
  occupation.coords[,2] <- time.period$Y_COORDS
  occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
  
  occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
  density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
  density.plot <- stats::density(density.points,adjust=0.1)
  
  q <- q+1
  
  density.extremes[q,] <- c(min(density.plot),max(density.plot))
}


grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 4.pdf',height=9,width=6)
graphics::par(mfrow=c(3,2),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

for(i in c(seq(column.start,column.end,sample.year.duration))) {
  
  dating.data <- occupation.information
  time.period <- dating.data[which(dating.data[,i] > 0 ),]
  occupation.coords <- base::matrix(NA,nrow=nrow(time.period),ncol=2)
  occupation.coords[,1] <- time.period$X_COORDS
  occupation.coords[,2] <- time.period$Y_COORDS
  occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)
  
  occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
  density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
  density.plot <- stats::density(density.points,adjust=0.1)
  
  graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
  raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
  raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)
  
  raster::plot(density.plot,zlim=c(min(density.extremes),max(density.extremes)),col=alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
  graphics::contour(density.plot,zlim=c(min(density.extremes),max(density.extremes)),nlevels=4,add=T,axes=F,legend=F,labels='',lwd=0.5)
  
  graphics::text(raster::xmin(region.dem)+1750,raster::ymin(region.dem)+3000,as.character(year.sequence[i]),col='white',pos=4,cex=2,offset=0)
  
  ## Scale
  graphics::segments(raster::xmax(region.dem)-30000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2500,lwd=1)
  graphics::segments(raster::xmax(region.dem)-30000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-30000,raster::ymin(region.dem)+2000,lwd=1)
  graphics::segments(raster::xmax(region.dem)-20000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-20000,raster::ymin(region.dem)+2000,lwd=1)
  graphics::segments(raster::xmax(region.dem)-10000,raster::ymin(region.dem)+3000,raster::xmax(region.dem)-10000,raster::ymin(region.dem)+2000,lwd=1)
  graphics::text(raster::xmax(region.dem)-29000,raster::ymin(region.dem)+2250,'0',cex=0.7,pos=2)
  graphics::text(raster::xmax(region.dem)-11000,raster::ymin(region.dem)+2250,'20 km',cex=0.7,pos=4)
  
  ## North arrow
  graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+2500,raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,lwd=1)
  graphics::segments(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+13500,raster::xmax(region.dem)-2750,raster::ymin(region.dem)+11000,lwd=1)
  graphics::text(raster::xmax(region.dem)-2000,raster::ymin(region.dem)+8000,'N')
  
  graphics::box()
  
}

grDevices::dev.off()

##########################################################################################
##########################################################################################
## Figure 5: Population reconstruction by year

annual.household.population <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/annual-household-population.csv')

grDevices::pdf('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/Figure 5.pdf',height=5,width=9)
graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.5,0.6,0.10,0.6),oma=c(0.5,0.5,0.5,0.5))

graphics::plot(1,type="n",xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,5000),xaxs="i",yaxs="i",axes=F,main='')
graphics::abline(v=c(463,600,770,1035,1205),col='gray',lty=3,lwd=1.5,xpd=F)
graphics::lines(seq(year.start,year.end,year.duration),annual.household.population[,2],col='#9E0142',lwd=1)

graphics::axis(4,at=seq(0,5000,1000),col='#9E0142',tick=T,labels=F)
graphics::mtext(as.character(seq(0,5000,1000)),at=as.character(seq(0,5000,1000)),col='#9E0142',side=4,line=0.75,cex=0.75,las=2)
graphics::mtext('Number of Households',col='#9E0142',side=4,line=2.5)

graphics::par(new=T)

graphics::plot(1,type="n",xlab='',ylab='',xlim=c(year.start,year.end),ylim=c(0,25000),xaxs="i",yaxs="i",axes=F,main='')
graphics::lines(seq(year.start,year.end,year.duration),((annual.household.population[,3] * 5)),col='black',lwd=3)
graphics::polygon(c(annual.household.population[,1],rev(annual.household.population[,1])),c((annual.household.population[,3] * 7),rev((annual.household.population[,3] * 3))),col=alpha('gray40',alpha=0.4),border=NA)

graphics::axis(1,at=seq(500,year.end,100),tick=T,labels=F,col='black')
graphics::mtext(as.character(seq(500,year.end,100)),side=1,line=0.5,at=seq(500,1300,100),cex=0.75,col='black')
graphics::mtext('Years (AD)',side=1,line=1.5,col='black')

graphics::axis(2,at=seq(0,25000,5000),tick=T,labels=F)
graphics::mtext(as.character(seq(0,25000,5000)),at=as.character(seq(0,25000,5000)),side=2,line=0.75,cex=0.75,las=2)
graphics::mtext('Total Population',side=2,line=2.5)

graphics::text(470+((710-470)/2),25000,'Basketmaker III',xpd=T)
graphics::text(710+((890-710)/2),25000,'Pueblo I',xpd=T)
graphics::text(890+((1150-890)/2),25000,'Pueblo II',xpd=T)
graphics::text(1150+((1295-1150)/2),25000,'Pueblo III',xpd=T)

grDevices::dev.off()

##########################################################################################
##########################################################################################
## Supplementary Material 1: Annual residential density from AD 450--1300

year.start <- 450
year.end <- 1300
year.duration <- 1

year.sequence <- seq((year.start-7),1300,1)

occupation.information <- occupation.households
column.start <- which(colnames(occupation.information) == paste('X',year.start,sep=''))
column.end <- which(colnames(occupation.information) == paste('X',year.end,sep=''))
occupation.information[,column.start:column.end] <- round(occupation.information[,column.start:column.end],digits=2)
occupation.data <- base::matrix(NA,nrow=nrow(occupation.information),ncol=2)
occupation.data[,1] <- occupation.information$X_COORDS
occupation.data[,2] <- occupation.information$Y_COORDS

year <- seq(year.start,year.end,year.duration)

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

    grDevices::pdf(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-households/',names(time.period[i]),'.pdf',sep=''))
    graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

    graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
    raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
    raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)

    graphics::rect(raster::xmin(region.dem),raster::ymin(region.dem),raster::xmax(region.dem),raster::ymax(region.dem),col=alpha("#5E4FA2",alpha=0.4))

    graphics::text(raster::xmin(region.dem)-1000,raster::ymin(region.dem)+2000,as.character(year.sequence[i]),col='white',pos=4,cex=4)

    graphics::box()

    grDevices::dev.off()

  }

  else{

    occupation.coords <- sp::SpatialPointsDataFrame(coords=occupation.coords,time.period,proj4string=master.projection)

    occupation.datums <- matrix(coordinates(occupation.coords),ncol=2,byrow=FALSE)
    density.points <- spatstat::as.ppp(occupation.datums,W=spatstat::owin(xrange=c(raster::xmin(region.dem),raster::xmax(region.dem)),yrange=c(raster::ymin(region.dem),raster::ymax(region.dem))))
    density.plot <- stats::density(density.points,adjust=0.1)

    grDevices::pdf(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-households/',names(time.period[i]),'.pdf',sep=''))
    graphics::par(mfrow=c(1,1),bg=NA,mai=c(0.10,0.10,0.10,0.10),oma=c(0.5,0.5,0.5,0.5))

    graphics::plot(1,type="n",xlab='',ylab='',xlim=c(raster::xmin(region.dem),raster::xmax(region.dem)),ylim=c(raster::ymin(region.dem),raster::ymax(region.dem)),xaxs="i",yaxs="i",axes=F,main='')
    raster::plot(region.hillshade,col=colors,legend=F,add=T,axes=F)
    raster::plot(region.dem,col=scales::alpha(colors,alpha=0.40),add=T,axes=F,legend=F)

    raster::plot(density.plot,col=alpha(heatcolors,alpha=0.4),add=T,axes=F,legend=F)
    graphics::contour(density.plot,nlevels=4,add=T,axes=F,legend=F,labels='',lwd=0.5)

    graphics::text(raster::xmin(region.dem)-1000,raster::ymin(region.dem)+2000,as.character(year.sequence[i]),col='white',pos=4,cex=4)

    graphics::box()

    grDevices::dev.off()

  }

}

##########################################################################################
##########################################################################################