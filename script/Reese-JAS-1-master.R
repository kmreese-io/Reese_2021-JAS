##########################################################################################
##########################################################################################
## DEEP LEARNING ARTIFICIAL NEURAL NETWORKS FOR NON-DESTRUCTIVE ARCHAEOLOGICAL SITE DATING
## KELSEY M. REESE
## JOURNAL OF ARCHAEOLOGICAL SCIENCE
## Volume(Issue):PageStart--PageEnd
## Date of Publication 2021
##########################################################################################
##########################################################################################
## MASTER ##
##########################################################################################
##########################################################################################
## R packages for analysis
options(java.parameters = '-Xmx10g' )
packages <-c('RColorBrewer','sp','rgeos','grDevices','tibble','dplyr','tidyr','caTools','caret','neuralnet','doParallel','dismo','randomForest','rJava','spatstat','smoother','MLeval','pROC')
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))
devtools::install_github('bocinsky/paleocar')
library(paleocar)

##########################################################################################
## Create project directories
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/table-products',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/model-final',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/results/final',recursive=T,showWarnings=F)
base::dir.create('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/output/figures/stack-population-density-standardized',recursive=T,showWarnings=F)

##########################################################################################
## Load functions for analysis
base::source('/Users/kmreese/Documents/PROJECTS/SOURCE/FUNCTIONS/normalize.R')
base::source('/Users/kmreese/Documents/PROJECTS/SOURCE/FUNCTIONS/polygonUTM_NAD83.R')

##########################################################################################
## Load all environment variables, run analyses, and produce figures from results
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-2-environment.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-3-dataset-preparation.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-4-model-iterations-function.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-5-model-iterations.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-6-model-validation.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-7-model-final.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-8-model-results.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-9-model-extrapolation.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-10-final-results.R')
base::source('/Users/kmreese/Documents/PROJECTS/CURRENT/Reese-JAS/script/Reese-JAS-11-figures.R')

##########################################################################################
##########################################################################################
