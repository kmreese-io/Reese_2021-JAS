##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## PREDICTIVE MODEL - ARTIFICIAL NEURAL NETWORK ITERATIONS ##
##########################################################################################
##########################################################################################
annIterations <- function(x,...){ 
  
  # Split the aggregated dataset into training and test sets
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
  
  # Define independent and dependent variables
  independent <- base::paste(as.character(colnames(x[2:variable.columns])))
  dependent <- base::paste(as.character(colnames(x[(variable.columns+1):ncol(x)])))
  
  # Create formula
  ann.formula <- stats::as.formula(paste(paste(dependent,collapse='+'),' ~ ',paste(independent,collapse='+')))
  
  # Run loop of all potential single layer values, from 1 to sum(length(independent),length(dependent)) variables
  # for(h in length(independent):length(dependent)) {
  for(h in 142:length(dependent)) {
    
    # Train artificial neural network model
    ann.model <- neuralnet::neuralnet(formula = ann.formula,     
                                      data = training.set,           
                                      threshold = 0.0000001,
                                      linear.output = F,
                                      hidden = h,
                                      stepmax = 100000000,
                                      rep = 10)
    
    # Save model iteration for re-use when best artificial neural network parameters have been identified
    saveRDS(ann.model,paste('./models/artificial-neural-network-',h,'.rds',sep=''))
    
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
    
    # Smooth results with windows of 1--50 to determine most accurate combination of model and smoothing
    for(s in 1:50) {

      # Smooth predictions
      smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { round(smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,2:ncol(ceramic.informed.occupation.ranges.matrix)],window=s,tails=T)) } )
      smoothed.occupation.ranges.matrix <- as.matrix(t(smoothing.range.predictions))
      ceramic.smoothed.occupation <- as.data.frame(cbind(ceramic.informed.occupation.ranges.matrix[,1],smoothed.occupation.ranges.matrix))
      names(ceramic.smoothed.occupation) <- c('SITE_ID',paste('X',seq(year.start,year.end,year.duration),'.y',sep=''))
      
      # Organize actual/predicted occupations for accuracy calculations
      presence.absence.actual <- predictions.SITE_NO[,(which(colnames(predictions.SITE_NO) == paste('X',year.start,'.x',sep=''))):(which(colnames(predictions.SITE_NO) == paste('X',year.end,'.x',sep='')))]
      presence.absence.actual[presence.absence.actual > 0] <- 1
      actual <- presence.absence.actual
      predictions <- ceramic.smoothed.occupation[,2:ncol(ceramic.smoothed.occupation)]
      
      # Create confusion matrix to calculate overall accuracy and balanced accuracy for model with 1--100 smoothing windows
      cm <- caret::confusionMatrix(data=as.factor(as.numeric(as.matrix(predictions))),reference=as.factor(as.numeric(as.matrix(actual))))
      overall <- as.matrix(cm,'overall')
      classes <- as.matrix(cm,'classes')
      accuracy <- overall[1,1]
      accuracy.balanced <- classes[11,1]
      # Overall accuracy
      row.names(smoothed.accuracy) <- h
      smoothed.accuracy[1,s] <- accuracy
      # Balanced accuracy
      row.names(smoothed.accuracy.balanced) <- h
      smoothed.accuracy.balanced[1,s] <- accuracy.balanced
      
      # Create tables to save annual accuracy, lower confidence interval, and upper confidence interval
      cm.accuracy.annual <- matrix(NA,nrow=1,ncol=year.end-year.start+1)
      cm.lower.annual <- matrix(NA,nrow=1,ncol=year.end-year.start+1)
      cm.upper.annual <- matrix(NA,nrow=1,ncol=year.end-year.start+1)
      
      # Produce confusion matrix by year to calculate annual accuracy, lower confidence interval, and upper confidence interval
      for(c in 1:ncol(predictions)) {

        single.factor <- FALSE
        
        tryCatch({
          
          cm.all <- caret::confusionMatrix(data=as.factor(as.numeric(as.matrix(predictions[,c]))),reference=as.factor(as.numeric(as.matrix(actual[,c])))) },
          
          error = function(e) { single.factor <<- TRUE})
        
        if(single.factor == TRUE) {
          
          occurance <- mean(as.matrix(predictions[,c]) + as.matrix(actual[,c]))
          
          if(occurance == 0 | occurance == 2) {
            
            cm.accuracy <- 1
            cm.lower <- 1
            cm.upper <- 1
            
            cm.accuracy.annual[1,c] <- cm.accuracy
            cm.lower.annual[1,c] <- cm.lower
            cm.upper.annual[1,c] <- cm.upper
            
          }
          
          else{
            
            difference <- abs(as.matrix(predictions[,c]) - as.matrix(actual[,c]))
            st.dev <- stats::sd(difference)
            
            cm.accuracy <- 1 - mean(difference)
            cm.lower <- cm.accuracy - (1.960 * (st.dev / base::sqrt(nrow(difference))))
            cm.upper <- cm.accuracy + (1.960 * (st.dev / base::sqrt(nrow(difference))))
            cm.upper[cm.upper > 1] <- 1
            
            cm.accuracy.annual[1,c] <- cm.accuracy
            cm.lower.annual[1,c] <- cm.lower
            cm.upper.annual[1,c] <- cm.upper
            
          }
          
        }
        
        else{
          
          cm.all <- caret::confusionMatrix(data=as.factor(as.numeric(as.matrix(predictions[,c]))),reference=as.factor(as.numeric(as.matrix(actual[,c]))))
          cm.overall <- as.matrix(cm.all,'overall')
          cm.accuracy <- as.numeric(cm.overall[1,1])
          cm.lower <- as.numeric(cm.overall[3,1])
          cm.upper <- as.numeric(cm.overall[4,1])
          
          cm.accuracy.annual[1,c] <- cm.accuracy
          cm.lower.annual[1,c] <- cm.lower
          cm.upper.annual[1,c] <- cm.upper
          
        }
        
      }
      
      # Export annual accuracy
      row.names(cm.accuracy.annual) <- h
      write.table(cm.accuracy.annual,file=paste('./smoothing-windows/annual/annual-accuracy-',s,'.csv',sep=''),append=T,row.names=T,col.names=F,sep=',')
      
      # Export lower confidence interval
      row.names(cm.lower.annual) <- h
      write.table(cm.lower.annual,file=paste('./smoothing-windows/lower/annual-lower-',s,'.csv',sep=''),append=T,row.names=T,col.names=F,sep=',')
      
      # Export upper confidence interval
      row.names(cm.upper.annual) <- h
      write.table(cm.upper.annual,file=paste('./smoothing-windows/upper/annual-upper-',s,'.csv',sep=''),append=T,row.names=T,col.names=F,sep=',')
      
    }
    
    # Export model overall accuracy and overall balanced accuracy
    write.table(smoothed.accuracy,file='./smoothing-tests/accuracy.csv',append=T,row.names=T,col.names=F,sep=',')
    write.table(smoothed.accuracy.balanced,file='./smoothing-tests/accuracy-balanced.csv',append=T,row.names=T,col.names=F,sep=',')
    
    # Clean workspace to save memory where possible
    gc()
    
  }
  
}

##########################################################################################
##########################################################################################

