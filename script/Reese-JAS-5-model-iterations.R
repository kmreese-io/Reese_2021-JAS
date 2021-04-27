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
##########################################################################################
##########################################################################################
## Set desired model universals: AD 450--1300 x 1 year increments
year.duration <- 1
year.start <- 450
year.end <- utils::tail(seq(year.start,1300,year.duration),n=1)
tree.rings.aggregated <- tree.rings.aggregated.1

# Create directories for saving results based on model universals
base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss',sep=''),showWarnings=F)
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-ss',sep=''))
base::dir.create('./models',showWarnings=F)
base::dir.create('./smoothing-windows/annual',recursive=T,showWarnings=F)
base::dir.create('./smoothing-windows/lower',recursive=T,showWarnings=F)
base::dir.create('./smoothing-windows/upper',recursive=T,showWarnings=F)
base::dir.create('./smoothing-tests',showWarnings=F)

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-ss-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Run the neural network with input parameters defined above and save results to working directory
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)
annIterations(dataset)
parallel::stopCluster(clusters)

##########################################################################################
##########################################################################################
# Create directories for saving results based on model universals
base::dir.create(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc',sep=''),showWarnings=F)
base::setwd(paste('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-iterations/',year.start,'-',year.end,'x',year.duration,'-cc',sep=''))
base::dir.create('./models',showWarnings=F)
base::dir.create('./smoothing-windows/annual',recursive=T,showWarnings=F)
base::dir.create('./smoothing-windows/lower',recursive=T,showWarnings=F)
base::dir.create('./smoothing-windows/upper',recursive=T,showWarnings=F)
base::dir.create('./smoothing-tests',showWarnings=F)

# Combine columns of ceramics with overlapping typologies 
dataset <- utils::read.csv('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products/dataset-aggregated-cc-1.csv')
dataset$BW_MANCOS <- dplyr::coalesce(dataset$BW_MANCOS,dataset$BW_WETHERILL)
dataset <- base::as.data.frame(dataset %>% dplyr::select('SITE_ID','GRAY_CHAPIN','GRAY_MOCCASIN','GRAY_MANCOS','CORRUGATED_DOLORES','CORRUGATED_MESAVERDE','RO_ABAJO','BR_BLUFF','BR_DEADMANS','BW_CHAPIN','BW_PIEDRA','BW_CORTEZ','BW_MANCOS','BW_MCELMO','BW_MESAVERDE',paste('X',as.character(year.start),sep=''):ncol(dataset)))

# Run the neural network with input parameters defined above and save results to working directory
clusters <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(clusters)
annIterations(dataset)
parallel::stopCluster(clusters)

##########################################################################################
##########################################################################################
