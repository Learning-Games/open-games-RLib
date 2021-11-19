#===============================================================================
# --------------- ANALYSIS SCRIPT FOR ASYMMETRIC LEARNERS GAME -----------------
#===============================================================================


#===================== SET REPOSITORY AS WORKING DIRECTORY =====================
maindir <- file.path("/outputs")

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

# input
datadir <- file.path("/experiment")

## Select the directories inside the hased run
require(stringr)

datadirs <- str_subset(list.files(datadir, full.names = TRUE), "run")

#================================== LIBRARIES ==================================

library(data.table)
library(stringr)

#================================== FUNCTIONS ==================================

# auxiliary functions for asymmetric learner analysis
# includes additional libraries needed
source("/Rscripts/asymlearners_aux_funcs.R")


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

# check
pgs[, table(c1,c2)]
pgs[, table(phase, match)]

# when there are no phases specified
# pgs[match %in% c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"), phase := 1
#     ][is.na(phase), phase := 2]

# separate between control matches and rematches across experiments
pgs[c1 == c2, match_type := "control"]
pgs[c1 != c2, match_type := "rematch"]

# check
pgs[, table(match_type, phase)]

# code cost of opponent in previous learning phase in phase 4
pgs[player == 1 & phase == 4, p1phase3oppcost := str_extract(
                                str_extract(match, "p1e[0-9]e[0-9]"), "[0-9]$")]
pgs[player == 2 & phase == 4, p2phase3oppcost := str_extract(
                                str_extract(match, "p2e[0-9]"), "[0-9]$")]
pgs[phase == 4, p1phase3oppcost := max(p1phase3oppcost, na.rm = TRUE), by = match]
pgs[phase == 4, p2phase3oppcost := max(p2phase3oppcost, na.rm = TRUE), by = match]

# --------------------------- find action cycles -------------------------------
#' findRptSqs function in auxiliary functions script finds repeated sequences
#' findRptPlays function applies it to find cycles that explain >=80/100 actions
#' returns the frequently played sequences as transitions. N is the number of
#' times a transition occurs. returns cycling equilibrium of length x if for one
#' player a sequence of length x explains 80/100 actions.
#' *CAUTION:* uses efficient data.table shift function but complex and slow!
#' reduce maxseq to eg 5 for larger runs!

cyc <- rbindlist(lapply(datadirs, maxseq = 15, findRptPlays))

# add levels to eq_type to order
cyc[, eq_type := factor(eq_type, levels = c("sym1", "asym1", "cyc_2", "cyc_3", "cyc_4", "cyc_5",
                                    "cyc_6", "cyc_7", "cyc_8", "cyc_9", "cyc_10", "cyc_11",
                                    "cyc_12", "cyc_13", "cyc_14", "cyc_15", "cyc_16", "cyc_17",
                                    "cyc_18", "cyc_19", "cyc_20", "cyc_21", "cyc_22", "cyc_23",
                                    "cyc_24", "cyc_25", "cyc_26", "cyc_27", "cyc_28", "cyc_29",
                                    "cyc_30", "cyc_31", "cyc_32", "cyc_33", "cyc_34", "cyc_35",
                                    "cyc_36", "cyc_37", "cyc_38", "cyc_39", "cyc_40", "cyc_41",
                                    "cyc_42", "cyc_43", "cyc_44", "cyc_45", "cyc_46", "cyc_47",
                                    "cyc_48", "cyc_49", "cyc_50", "other"))]




# ---------------------- join action to profit data -----------------------------
# join on match, run, and player

setkey(pgs, match, run, phase)
setkey(cyc, match, run, phase)
# pgscyc <- cyc[pgs]

# and add equilibrium type to pgs only data
pgs <- merge(pgs, unique(cyc[, .(match, run, phase, eq_type)]),
             by = c("match", "run", "phase"))


# and cost levels plus match_type to cyc data
cyc <- merge(cyc, unique(pgs[, .(match, run, phase, c1, c2, match_type)]),
             by = c("match", "run", "phase"))

library(xtable)

# means and standard deviations of run means for three metrics, and eq types and N
summarise_vars <- c("mreward", "mcollusion_index")

# per match_type
tab1 <- pgs[,  lapply(.SD, mean), .SDcols = summarise_vars, by = .(match_type, eq_type, phase)]
tab1 <- merge(tab1, pgs[,  lapply(.SD, sd), .SDcols = summarise_vars, by = .(match_type, eq_type, phase)],
              by = c("match_type", "eq_type", "phase"))
setnames(tab1, c("match_type", "eq_type", "phase", "mreward.m",
                 "mcollusion_index.m", "mreward.sd", "mcollusion_index.sd"))
Nruns <- unique(pgs[, .N, by = .(match_type, eq_type, phase)])
tab1 <- merge(tab1, Nruns, by = c("match_type", "eq_type", "phase"))
rm(Nruns)

setcolorder(tab1, c("phase" ,"match_type", "eq_type", "N", "mreward.m", "mreward.sd",
                    "mcollusion_index.m", "mcollusion_index.sd"))
setkey(tab1, phase, match_type, eq_type)
tab1
fwrite(tab1, file.path(maindir, "summary_statistics_matchtype.csv"))
