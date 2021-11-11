#===============================================================================
# --------------- ANALYSIS SCRIPT FOR ASYMMETRIC LEARNERS GAME -----------------
#===============================================================================


#===================== SET REPOSITORY AS WORKING DIRECTORY =====================
maindir <- file.path("/Users/philippzahn/Documents/projects/learning/results/")


# set working directory
setwd(maindir)


#=================== DATA DIRECTORY AND SCP DOWNLOAD FILES =====================

# -- Specify local data directory and files of current run --
# prompts for scp download of *most recent* experiment from remote that matches
# user provided pattern match (eg "asymmetricLearners")
# downloads all extendedEndNLines.csv files of all runs in remote directory
# and parameters.csv, state_action_index_*.csv files for all run1s and copies to all other runs
# if no is selected, directory list is returned of runs of most recent "asymmetricLearners"
# experiment in local data directory
# if any other run is needed, specify or download manually


datadir <- file.path("/Users/philippzahn/Documents/projects/learning/results/asymmetricLearners4Phases-ebd4d33e09fec957bfe54529d9c7af9288aba8ef")

## Select the directories inside the hased run
require(stringr)

datadirs <- str_subset(list.files(datadir, full.names = TRUE), "run")


#================================== LIBRARIES ==================================

library(data.table)
library(stringr)

#================================== FUNCTIONS ==================================

# auxiliary functions for asymmetric learner analysis
# includes additional libraries needed
source("asymlearners_aux_funcs.R")


#====================== READ IN DATA FROM DATA DIRECTORY =======================

#------------------------------ grab profit gains ------------------------------
#' profit_gains.get function in auxiliary functions script
#' mreward is mean of reward by match x run, similarly m- and sdprofit_gain

# set benchmark profits. theyre constant over cost parameter changes if a=c+1. faster this way
benchmarks <- data.table(nash_profit = rep(0.2216948, 2), mon_profit = rep(0.3374724, 2),
                         player = c(1,2))

# calculate profit metrics
pgs <- rbindlist(lapply(datadirs, profit_gains.get, benchmarks = benchmarks))

# cleanup
rm(benchmarks)
