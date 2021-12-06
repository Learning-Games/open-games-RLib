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
#datadirs <- datadirs[str_detect(datadirs, "run_1")]
pgs <- rbindlist(lapply(datadirs, profit_gains.get, benchmarks = benchmarks))

# cleanup
rm(benchmarks)

# check
pgs[, table(c1,c2)]
pgs[, table(phase, match)]

# separate between control matches and rematches across experiments
pgs[c1 == c2, match_type := "control"]
pgs[c1 != c2, match_type := "rematch"]

# check
pgs[, table(match_type, phase)]

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


#================================== ANALYSIS ====================================


# JPC Matrices

library(ggplot2)
#calculate means and sds
jpc_data <- pgs[, .(mean(mcollusion_index), sd(mcollusion_index)/sqrt(pgs[, .N/uniqueN(match)])),
                by = .(c1, c2, phase)]

jpc_data[, labels := as.character(paste(round(V1, 2), sep = ""))]

jpc_data[, c1 := factor(c1, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))
][, c2 := factor(c2, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))]

jpc_plot <- function(jpc){

  plot <- ggplot(jpc,
         aes(x = c1,
             y = c2, fill = V1)) +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +

    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") +

    labs(x = "Cost level of player 1", y = "Cost level of player 2",
         # caption = paste(as.character(hash)),
         fill = "") +

    geom_text(aes(label = labels), size = 7, color = "black") +

    scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                         limit = c(-0.15,1), space = "Lab")

  return(plot)
}

jpc_collusionindex_phase1 <- jpc_plot(jpc_data[phase == 1,]) +

  labs(title = "Mean collusion index after first learning phase")

jpc_collusionindex_phase2 <- jpc_plot(jpc_data[phase == 2,]) +

  labs(title = "Mean collusion index after second learning phase")

jpc_collusionindex_phase3 <- jpc_plot(jpc_data[phase == 3,]) +

  labs(title = "Mean collusion index after third learning phase")

library(patchwork)

jpc_collusionindex_phase1 / jpc_collusionindex_phase2 / jpc_collusionindex_phase3


# Equilibria
visdata <- unique(cyc[, .(phase, match, match_type, run, eq_type)])
levels(visdata$eq_type)[1:13] <- c("Symmetric","Asymmetric","Cycle 2","Cycle 3",
                                   "Cycle 4", "Cycle 5", "Cycle 6", "Cycle 7",
                                   "Cycle 8", "Cycle 9", "Cycle 10", "Cycle 11",
                                   "Cycle 12")
levels(visdata$eq_type)[52] <- c("Other")

eqtype_frequency_phase3 <-
  ggplot(unique(visdata[phase == 3 & match_type == "control",
                      .N/(uniqueN(match)*250),
                      by = .(phase, eq_type, match_type)]),
       aes(x = eq_type, y = V1)) +
  geom_bar(stat = "identity", color="black", fill='#ff8590') +
  theme_minimal() +
  ylab("Frequency") + xlab("Convergence Type") +
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   margin=margin(0,0,0,0,"pt")))

ggsave(file.path(maindir, "equilibria_frequency_phase3.pdf"),
       eqtype_frequency_phase3,
       device = "pdf", width = 105, height = 105, units = "mm")
rm(eqtype_frequency_phase3, visdata)


# Time to Convergence
convergence_phase3 <- ggplot(
  unique(pgs[phase == 3, .(phase, match, match_type, finaliteration)]),
                             aes(x = finaliteration)) +
  geom_histogram(color="black", fill="#ff8590", aes(y= stat(count) / sum(count))) +
  theme_minimal() + labs(x = "Iterations Until Convergence", y = "Frequency")

ggsave(file.path(maindir, "convergence_phase3.pdf"),
       convergence_phase3,
       device = "pdf", width = 105, height = 90, units = "mm")
