##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2020
##########################################################################################

## MODEL ITERATIONS ##

##########################################################################################
year.duration <- 1
year.start <- 445
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''),showWarnings=F)
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::dir.create('./smoothing-windows',showWarnings=F)
base::dir.create('./smoothing-mean',showWarnings=F)

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)
##########################################################################################
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''),showWarnings=F)
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::dir.create('./smoothing-windows',showWarnings=F)
base::dir.create('./smoothing-mean',showWarnings=F)

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)
##########################################################################################
year.duration <- 1
year.start <- 550
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''),showWarnings=F)
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::dir.create('./smoothing-windows',showWarnings=F)
base::dir.create('./smoothing-mean',showWarnings=F)

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)
##########################################################################################
year.duration <- 10
year.start <- 550
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.10

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-10.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)
##########################################################################################
year.duration <- 20
year.start <- 540
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.20

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-20.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)
##########################################################################################
year.duration <- 30
year.start <- 550
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.30

base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,sep=''))

dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-30.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Start parallel computing
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)

# Run the neural network with input parameters defined above and save results to working directory
annIterations(dataset)

# End parallel computing
parallel::stopCluster(clusters)