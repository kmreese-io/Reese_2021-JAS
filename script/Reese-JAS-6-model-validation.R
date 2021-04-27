##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## PREDICTIVE MODEL - K-FOLD CROSS VALIDATION ##
##########################################################################################
##########################################################################################

## Define working directory and universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.ss.1

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

##########################################################################################
## Identify optimal model parameters based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss','/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
## Choose random number sample for k-fold cross-validation and test model accuracy for all seeds
# random.seeds <- base::sample(x=1:1000000,size=10,replace=F)
random.seeds <- c(782828,397359,931335,125616,714707,509330,540359,566482,49700,469379)

# Create tables to export k-fold results
smoothed.accuracy <- matrix(NA,nrow=5,ncol=10)
smoothed.accuracy.balanced <- matrix(NA,nrow=5,ncol=10)

# Create matrix of values to sample in training and test datasets
kfold.matrix <- matrix(1:(23*5),nrow=5,ncol=23,byrow=TRUE)

for(seed in 1:length(random.seeds)) {
  
  # Split the aggregated dataset into training and test sets
  set.seed(seed)
  randomized.rows <- base::sample(x=1:nrow(dataset),size=nrow(dataset),replace=F)
  
  for(kfold in 1:nrow(kfold.matrix)) {
    
    # Split into training and test datasets
    training.set.original <- dataset[c(randomized.rows[kfold.matrix[kfold,]]),]
    training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
    test.set.original <- dataset[c(randomized.rows[kfold.matrix[-kfold,]]),]
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
    colnames(training.set) <- c(base::paste(as.character(colnames(dataset))))
    
    # Normalize the ceramic data across rows in test set - normalizing by SITE_ID
    test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(variable.columns-1))
    for(i in 1:nrow(test.set.original)) {
      normalized.row <- as.matrix(normalize(test.set.original[i,2:(variable.columns)]))
      test.normalized.matrix[i,] <- normalized.row
    }
    test.set <- as.data.frame(cbind(test.set.original[,1],unlist(test.normalized.matrix[,1:(variable.columns-1)]),test.set.original[(variable.columns+1):ncol(test.set.original)]))
    colnames(test.set) <- c(base::paste(as.character(colnames(dataset))))
    
    # Replace any NaNs with zeros
    training.set[is.na(training.set)] <- 0
    test.set[is.na(test.set)] <- 0
    
    # Define independent and dependent variables
    independent <- base::paste(as.character(colnames(dataset[2:variable.columns])))
    dependent <- base::paste(as.character(colnames(dataset[(variable.columns+1):ncol(dataset)])))
    
    # Create formula
    ann.formula <- stats::as.formula(paste(paste(dependent,collapse='+'),' ~ ',paste(independent,collapse='+')))
    
    # Train artificial neural network on k-fold using node.optimal
    ann.model <- neuralnet::neuralnet(formula = ann.formula,     
                                      data = training.set,           
                                      threshold = 0.0000001,
                                      linear.output = F,
                                      hidden = node.optimal,
                                      stepmax = 100000000,
                                      rep = 10)
    
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
    
    # Smooth results with window.optimal
    smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { round(smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,2:ncol(ceramic.informed.occupation.ranges.matrix)],window=window.optimal,tails=T)) } )
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
    smoothed.accuracy[kfold,seed] <- accuracy
    # Balanced accuracy
    smoothed.accuracy.balanced[kfold,seed] <- accuracy.balanced
    
  }
  
  # Save results
  utils::write.csv(smoothed.accuracy,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/ss-smoothed-accuracy.csv')
  utils::write.csv(smoothed.accuracy.balanced,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/ss-smoothed-accuracy-balanced.csv')
  
}

smoothed.accuracy <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/ss-smoothed-accuracy.csv',row.names=1)

k.fold.cross.validation.ss <- base::mean(as.numeric(unlist(smoothed.accuracy)))

##########################################################################################
##########################################################################################

## Define working directory and universal model parameters
setwd('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/')
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.cc.1

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

##########################################################################################
## Identify optimal model parameters based on accuracy results from iterations with defined universal model parameters
node.iterations <- utils::read.csv(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc','/smoothing-tests/accuracy-balanced.csv',sep=''),header=F)
node.optimal.position <- which(node.iterations == max(node.iterations[,2:ncol(node.iterations)]),arr.ind=T)
node.optimal <- node.iterations[node.optimal.position[1],1]
percent.optimal <- (node.iterations[node.optimal.position[1],node.optimal.position[2]]) * 100
smoothing.windows <- c(1:50)
window.optimal <- smoothing.windows[(node.optimal.position[2] - 1)]

##########################################################################################
## Choose random number sample for k-fold cross-validation and test model accuracy for all seeds
# random.seeds <- base::sample(x=1:1000000,size=10,replace=F)
random.seeds <- c(198322,67104,970070,275428,736783,880657,857185,38502,757613,758876)

# Create tables to export k-fold results
smoothed.accuracy <- matrix(NA,nrow=5,ncol=10)
smoothed.accuracy.balanced <- matrix(NA,nrow=5,ncol=10)

# Create matrix of values to sample in training and test datasets
kfold.matrix <- matrix(1:(23*5),nrow=5,ncol=23,byrow=TRUE)

for(seed in 1:length(random.seeds)) {
  
  # Split the aggregated dataset into training and test sets
  set.seed(seed)
  randomized.rows <- base::sample(x=1:nrow(dataset),size=nrow(dataset),replace=F)
  
  for(kfold in 1:nrow(kfold.matrix)) {
    
    # Split into training and test datasets
    training.set.original <- dataset[c(randomized.rows[kfold.matrix[kfold,]]),]
    training.set.original <- training.set.original[which(!is.na(training.set.original[,1])),]
    test.set.original <- dataset[c(randomized.rows[kfold.matrix[-kfold,]]),]
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
    colnames(training.set) <- c(base::paste(as.character(colnames(dataset))))
    
    # Normalize the ceramic data across rows in test set - normalizing by SITE_ID
    test.normalized.matrix <- matrix(NA,nrow=nrow(test.set.original),ncol=(variable.columns-1))
    for(i in 1:nrow(test.set.original)) {
      normalized.row <- as.matrix(normalize(test.set.original[i,2:(variable.columns)]))
      test.normalized.matrix[i,] <- normalized.row
    }
    test.set <- as.data.frame(cbind(test.set.original[,1],unlist(test.normalized.matrix[,1:(variable.columns-1)]),test.set.original[(variable.columns+1):ncol(test.set.original)]))
    colnames(test.set) <- c(base::paste(as.character(colnames(dataset))))
    
    # Replace any NaNs with zeros
    training.set[is.na(training.set)] <- 0
    test.set[is.na(test.set)] <- 0
    
    # Define independent and dependent variables
    independent <- base::paste(as.character(colnames(dataset[2:variable.columns])))
    dependent <- base::paste(as.character(colnames(dataset[(variable.columns+1):ncol(dataset)])))
    
    # Create formula
    ann.formula <- stats::as.formula(paste(paste(dependent,collapse='+'),' ~ ',paste(independent,collapse='+')))
    
    # Train artificial neural network on k-fold using node.optimal
    ann.model <- neuralnet::neuralnet(formula = ann.formula,     
                                      data = training.set,           
                                      threshold = 0.0000001,
                                      linear.output = F,
                                      hidden = node.optimal,
                                      stepmax = 100000000,
                                      rep = 10)
    
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
    
    # Smooth results with window.optimal
    smoothing.range.predictions <- mapply(c(1:nrow(ceramic.informed.occupation.ranges.matrix)),FUN=function(x,...) { round(smoother::smth.gaussian(ceramic.informed.occupation.ranges.matrix[x,2:ncol(ceramic.informed.occupation.ranges.matrix)],window=window.optimal,tails=T)) } )
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
    smoothed.accuracy[kfold,seed] <- accuracy
    # Balanced accuracy
    smoothed.accuracy.balanced[kfold,seed] <- accuracy.balanced
    
  }
  
  # Save results
  utils::write.csv(smoothed.accuracy,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/cc-smoothed-accuracy.csv')
  utils::write.csv(smoothed.accuracy.balanced,'/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/cc-smoothed-accuracy-balanced.csv')
  
}

smoothed.accuracy <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final/k-fold-cross-validation/cc-smoothed-accuracy.csv',row.names=1)

k.fold.cross.validation.cc <- base::mean(as.numeric(unlist(smoothed.accuracy)))

##########################################################################################
##########################################################################################
