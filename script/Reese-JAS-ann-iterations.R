##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## ARTIFICIAL NEURAL NETWORK ITERATIONS ##

annIterations <- function(x,...){ 
  # Split the aggregated dataset into training and test sets
  set.seed(471919)
  split <- caTools::sample.split(x,SplitRatio=0.8)
  training.set.original <- base::subset(x,split == T)
  training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
  test.set.original <- base::subset(x,split == F)
  test.set.original <- test.set.original[which(!is.na(test.set.original[,1])),]
  
  variable.columns <- length(2:which(colnames(test.set.original) == paste('X',year.start,sep='')))
  
  training.set.original <- training.set.original[rowSums(training.set.original[,2:variable.columns]) != 0 , ]
  test.set.original <- test.set.original[rowSums(test.set.original[,2:variable.columns]) != 0 , ]
  
  # Normalize the ceramic data across rows in both training set and test set - normalizing by SITE_ID
  training.normalized.matrix <- matrix(NA,nrow=nrow(training.set.original),ncol=(variable.columns-1))
  for(i in 1:nrow(training.set.original)) {
    normalized.row <- as.matrix(normalize(training.set.original[i,2:(variable.columns)]))
    training.normalized.matrix[i,] <- normalized.row
  }
  
  training.set <- as.data.frame(cbind(training.set.original[,1],unlist(training.normalized.matrix[,1:(variable.columns-1)]),training.set.original[(variable.columns+1):ncol(training.set.original)]))
  colnames(training.set) <- c(base::paste(as.character(colnames(x))))
  
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
  
  # Define independent and dependent variables
  independent <- base::paste(as.character(colnames(x[2:variable.columns])))
  dependent <- base::paste(as.character(colnames(x[(variable.columns+1):ncol(x)])))
  
  # Create formula
  ann.formula <- stats::as.formula(paste(paste(dependent,collapse='+'),' ~ ',paste(independent,collapse='+')))
  
  # Run loop of all potential single layer values, from 1 to sum(length(independent),length(dependent)) variables
  for(h in length(independent):length(dependent)) {
  
    ann.model <- neuralnet::neuralnet(formula = ann.formula,     
                                      data = training.set,           
                                      threshold = 0.0000001,
                                      linear.output = F,
                                      hidden = h,
                                      stepmax = 100000000,
                                      rep = 10
    )
    
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
    
    ceramic.informed.occupation.ranges.matrix <- matrix(NA,nrow=nrow(test.set),ncol=length(seq(year.start,year.end,year.duration))+1)
    mean.smoothed.results <- matrix(NA,nrow=1,ncol=100)
    
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
    
    for(s in 1:100) {
      
      smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { round(smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,],window=s,tails=T)) } )
      smoothed.occupation.ranges.matrix <- as.matrix(t(smoothing.range.predictions))
      
      ceramic.smoothed.occupation <- as.data.frame(smoothed.occupation.ranges.matrix)
      names(ceramic.smoothed.occupation) <- c('SITE_ID',paste('X',seq(year.start,year.end,year.duration),'.y',sep=''))
      
      # Test the resulting output and find optimal smoothing window
      presence.absence.actual <- predictions.SITE_NO[,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.x',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.x',sep='')))]
      presence.absence.actual[presence.absence.actual > 0] <- 1
      results <- data.frame(actual = presence.absence.actual, prediction = ceramic.smoothed.occupation[,2:ncol(ceramic.smoothed.occupation)])
      roundedresults<-sapply(results,round,digits=0)
      roundedresultsdf=data.frame(roundedresults)
      
      cm.of.results.list <- sapply(c(1:ncol(presence.absence.actual)),FUN=function(x,...) { table(roundedresultsdf[,x],roundedresultsdf[,(x+(ncol(presence.absence.actual)-1))]) })
      
      
      percent.of.results.list <- lapply(c(1:ncol(presence.absence.actual)),FUN=function(x,...) { ifelse( dim(cm.of.results.list[[x]])[1] == 2 && dim(cm.of.results.list[[x]])[2] == 2,
                                                                                                         sum(cm.of.results.list[[x]][1,1],cm.of.results.list[[x]][2,2]) / sum(cm.of.results.list[[x]]),
                                                                                                         cm.of.results.list[[x]][1] / sum(cm.of.results.list[[x]]) ) } )
      
      mean.percent.results <- mean(unlist(percent.of.results.list),na.rm=T)
      percent.results <- t(as.data.frame(unlist(percent.of.results.list)))
      
      row.names(percent.results) <- h
      write.table(percent.results,file=paste('./smoothing-windows/accuracy-ceramics-smoothed-',s,'.csv',sep=''),append=T,row.names=T,col.names=F,sep=',')
      
      row.names(mean.smoothed.results) <- h
      mean.smoothed.results[1,s] <- mean.percent.results
      
    }
    
    write.table(mean.smoothed.results,file='./smoothing-mean/accuracy-ceramics-smoothed-mean.csv',append=T,row.names=T,col.names=F,sep=',')
    
  }
  
}
