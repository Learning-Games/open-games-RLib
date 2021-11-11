#===============================================================================
# --------------- ANALYSIS SCRIPT FOR ASYMMETRIC LEARNERS GAME -----------------
#===============================================================================


#===================== SET REPOSITORY AS WORKING DIRECTORY =====================
maindir <- file.path("/Users/philippzahn/Documents/projects/PricingGames/src/Analysis/")


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

## Nicolas

datadir <- file.path("C:/Users/Nico/Dropbox/research/learning_games/pricing-games/data/")

if(askYesNo("SCP most recent experiment from remote?", default = TRUE) == TRUE)
{
    exp_pattern <- as.character(readline(prompt = "Provide experiment pattern match, eg asymmetricLearners: "))

    # script for scp download
    source("RScripts/asymlearners_scp_download.R")

    # specify parameters for download
    PARAMS <- list("ip" = "root@168.119.138.173",
                   "res_path" = "learning-work/results",
                   "output_path" = datadir,
                   "exp_pattern" = exp_pattern
                   )

    # specify file names to be downloaded for each run
    FILES <- c("rewardsExtendedEndNLines.csv", "parameters.csv",
                    "state_action_index_1.csv", "state_action_index_2.csv")

    # download specified files for each run for most recent asymmetricLearner experiment
    # from remote and return list of local directories of runs
    datadirs <- download_from_remote(params = PARAMS, files = FILES)

    rm(PARAMS, FILES, download_from_remote, exp_pattern)

} else
{
    # select files from most recent "asymmetricLearners" folder in local data directory with
    # pattern match on provided user string and "run"
    # if a different experiment is needed, specify manually
    require(stringr)

    # asymmetricLearner_dirs <-  str_subset(list.files(datadir, full.names = TRUE), "asymmetricLearners")
    #
    # latest_run_index <- which.max(file.info(asymmetricLearner_dirs)$mtime)
    #
    # datadirs <- list.files(asymmetricLearner_dirs[latest_run_index])

    exp_pattern <- as.character(readline(prompt = "Provide experiment pattern match, eg asymmetricLearners: "))

    datadirs <- as.list(
        str_subset(
        list.files(rownames(file.info(
            str_subset(list.files(datadir, full.names = TRUE),
                       exp_pattern)))[which.max(
                           file.info(str_subset(list.files(datadir, full.names = TRUE),
                                                exp_pattern))$mtime)],
            full.names = TRUE),
        "run")
    )

    rm(exp_pattern)
}


#================================== LIBRARIES ==================================

library(data.table)
library(stringr)

#================================== FUNCTIONS ==================================

# auxiliary functions for asymmetric learner analysis
# includes additional libraries needed
source("RScripts/asymlearners_aux_funcs.R")


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


# cyc[match %in% c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"),
#     phase := 1][is.na(phase), phase := 2]

# separate between control matches and rematches across experiments for cyc too
# cyc[phase == 1, match_type := "control"]
# cyc[phase %in% c(2,3), match_type := ifelse(str_count(match, "e1") == 2 |
#                                          str_count(match, "e2") == 2 |
#                                          str_count(match, "e3") == 2,
#                                      "control", "rematch")]
# cyc[phase == 4, match_type := "control"]


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

# grab hash of experiment
hash <- gsub("/p?[0-9]?e?[0-9]?e?[0-9]?[0-9]?p?[0-9]?e?[0-9]?[0-9]?e[0-9]{1,2}.*",
             "", gsub(".*/data/", "", datadirs[[1]]))

# create results directory for hash
dir.create(file.path(maindir, "output", hash))


# -------------------------- summary statistics ---------------------------------

library(xtable)

# means and standard deviations of run means for three metrics, and eq types and N
summarise_vars <- c("mreward", "mcollusion_index")

# for phase 1 per cost level
tab1 <- pgs[phase == 1, lapply(.SD, mean), .SDcols = summarise_vars, by = .(c1)]
tab1 <- merge(tab1, pgs[phase == 1, lapply(.SD, sd), .SDcols = summarise_vars, by = .(c1)],
              by = "c1")
setnames(tab1, c("c1", "mreward.m", "mcollusion_index.m",
                 "mreward.sd", "mcollusion_index.sd"))
Nruns <- unique(pgs[phase == 1, .N, by = .(c1, eq_type)])
Nruns <- dcast(Nruns, c1 ~ eq_type, value.var = "N")
tab1 <- merge(tab1, Nruns, by = c("c1"))
rm(Nruns)
tab1[, cyc_5plus := sum(cyc_5, cyc_6, cyc_7, cyc_8, cyc_9, cyc_10, na.rm = TRUE), by = c1]
tab1 <- tab1[,c(1,3,5,6,7,8,9,10,17)]
tab1
fwrite(tab1, file.path(maindir, "output", hash, "summary_statistics_phase1_bycost.csv"))

setnames(tab1, c("Cost", "Mean Collusion Index",
                 "SD Collusion Index", "Symmetric", "Asymmetric",
                 "Cycle 2", "Cycle 3", "Cycle 4", "Cycle 5+"))
print(xtable(tab1), include.rownames = FALSE)

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
fwrite(tab1, file.path(maindir, "output", hash, "summary_statistics_matchtype.csv"))


# per match
# means and standard deviation of run means for three metrics, and eq types and N
tab1 <- pgs[,  lapply(.SD, mean), .SDcols = summarise_vars, by = .(match, eq_type, phase)]
tab1 <- merge(tab1, pgs[,  lapply(.SD, sd), .SDcols = summarise_vars, by = .(match, eq_type, phase)],
              by = c("match", "eq_type", "phase"))
setnames(tab1, c("match", "eq_type", "phase", "mreward.m",
                 "mcollusion_index.m", "mreward.sd", "mcollusion_index.sd"))
Nruns <- unique(pgs[, .N, by = .(match, eq_type, phase)])
tab1 <- merge(tab1, Nruns, by = c("match", "eq_type", "phase"))
rm(Nruns)

setcolorder(tab1, c("phase", "match", "eq_type", "N", "mreward.m", "mreward.sd",
                    "mcollusion_index.m", "mcollusion_index.sd"))
setkey(tab1, phase, match, eq_type)
tab1
fwrite(tab1, file.path(maindir, "output", hash, "summary_statistics_match.csv"))

# per match x player
summarise_vars <- c("mreward", "mprofit_gain", "mcollusion_index")
# means and standard deviation of run means for three metrics, and eq types and N
tab1 <- pgs[,  lapply(.SD, mean), .SDcols = summarise_vars, by = .(match, eq_type, phase, player)]
tab1 <- merge(tab1, pgs[,  lapply(.SD, sd), .SDcols = summarise_vars, by = .(match, eq_type, phase, player)],
              by = c("match", "eq_type", "phase", "player"))
setnames(tab1, c("match", "eq_type", "phase", "player" ,"mreward.m", "mprofit_gain.m",
                 "mcollusion_index.m", "mreward.sd", "mprofit_gain.sd", "mcollusion_index.sd"))
Nruns <- unique(pgs[, .N, by = .(match, eq_type, phase)])
tab1 <- merge(tab1, Nruns, by = c("match", "eq_type", "phase"))
rm(Nruns)

setcolorder(tab1, c("phase" ,"match", "eq_type", "player" ,"N", "mreward.m", "mreward.sd", "mprofit_gain.m",
                    "mprofit_gain.sd", "mcollusion_index.m", "mcollusion_index.sd"))
setkey(tab1, phase, match, eq_type)
tab1
fwrite(tab1, file.path(maindir, "output", hash, "summary_statistics_matchxplayer.csv"))

rm(tab1)


# -------------------------- jpc matrices ---------------------------------

# jpc matrix of mean collusion index for the matches
library(ggplot2)
#calculate means and sds
jpc_data <- pgs[, .(mean(mcollusion_index), sd(mcollusion_index)/sqrt(pgs[, .N/uniqueN(match)])),
                     by = .(c1, c2, phase, p1phase3oppcost, p2phase3oppcost)]
#jpc_data[, labels := as.character(paste(paste(round(V1, 2), "\n", "(", as.character(round(V2, 2)), ")", sep = "")))]
jpc_data[, labels := as.character(paste(round(V1, 2), sep = ""))]

jpc_data[, c1 := factor(c1, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))
        ][, c2 := factor(c2, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))]

#pal <- colorRampPalette(c(rgb(0.96,0.96,1), rgb(0.1,0.1,0.9)), space = "rgb")
# matrix for phase 1
jpc_collusionindex_phase1 <- ggplot(jpc_data[phase == 1,],
                                    aes(x = c1,
                                        y = c2, fill = V1)) +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    legend.position = "none") +

    labs(x = "Cost level of player 1", y = "Cost level of player 2",
         # title = "Mean collusion index after first learning phase",
         # caption = paste(as.character(hash)),
         fill = "") +

    geom_text(aes(label = labels), size = 7, color = "black") +

  scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")

# matrix for phase 2
jpc_collusionindex_phase2 <- ggplot(jpc_data[phase == 2,],
                                    aes(x = c1,
                                        y = c2, fill = V1)) +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") +

    labs(x = "Cost level of player 1", y = "Cost level of player 2",
         #title = "Mean collusion index after rematching following the first learning phase",
         #caption = paste(as.character(hash)),
         fill = "") +

    geom_text(aes(label = labels), size = 7, color = "black") +

    scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")

# average proportional loss
(jpc_data[phase == 2 & c1 == c2, mean(V1)] -
        jpc_data[phase == 2 & c1 != c2, mean(V1)])/
    jpc_data[phase == 2 & c1 == c2, mean(V1)]


# matrix for phase 3
jpc_collusionindex_phase3 <- ggplot(jpc_data[phase == 3,],
                                    aes(x = c1,
                                        y = c2, fill = V1)) +

  geom_tile(colour="white", size = 0.25) + theme_minimal() +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") +

  labs(x = "Cost level of player 1", y = "Cost level of player 2",
       #title = "Mean collusion index after rematching following the first learning phase",
       #caption = paste(as.character(hash)),
       fill = "") +

  geom_text(aes(label = labels), size = 7, color = "black") +

  scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")

# matrix for phase 4
jpc_data[phase == 4, yviz := as.numeric(p1phase3oppcost)
         ][phase == 4 & c1 == 1,   yviz := as.numeric(p1phase3oppcost) - 1
         ][phase == 4 & c1 == 1.3 & p1phase3oppcost == 3,
                                   yviz := as.numeric(p1phase3oppcost) - 1]

jpc_collusionindex_phase4 <- ggplot(jpc_data[phase == 4,],
                                    aes(x = c1,
                                        y = yviz,
                                        fill = V1)) +

  geom_tile(colour="white", size = 0.25) + theme_minimal() +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none") +

  labs(x = "Cost level of both players", y = "Rival cost level in previous learning",
       #title = "Mean collusion index after rematching following the first learning phase",
       #caption = paste(as.character(hash)),
       fill = "") +

  geom_text(aes(label = labels), size = 7, color = "black") +

  scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")


# save plot
ggsave(file.path(maindir, "output", hash, "jpc_collusionindex_phase1.pdf"), jpc_collusionindex_phase1, device = "pdf",
       width = 180, height = 175, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_collusionindex_phase2.pdf"), jpc_collusionindex_phase2, device = "pdf",
       width = 180, height = 175, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_collusionindex_phase3.pdf"), jpc_collusionindex_phase3, device = "pdf",
       width = 180, height = 175, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_collusionindex_phase4.pdf"), jpc_collusionindex_phase4, device = "pdf",
       width = 120, height = 80, units = "mm")


# profit gain matrices
#calculate means and sds
jpc_data <- pgs[, .(mean(mprofit_gain), sd(mprofit_gain)/sqrt(pgs[, .N/uniqueN(match)])),
                     by = .(c1, c2, phase, player, p1phase3oppcost)]
#jpc_data[, labels := as.character(paste(paste(round(V1, 2), "\n", "(", as.character(round(V2, 2)), ")", sep = "")))]
jpc_data[, labels := as.character(paste(round(V1, 2), sep = ""))]
jpc_data[, player := factor(ifelse(player == 1, "Player 1", "Player 2"))]

jpc_data[, c1 := factor(c1, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))
         ][, c2 := factor(c2, levels = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7))]

# matrix of phase1
jpc_profitgain_phase1 <- ggplot(jpc_data[phase == 1,], aes(x = c1, y = c2, fill = V1)) +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +

    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position = "none") +

    labs(x = "Cost level of player 1", y = "Cost level of player 2",
         # title = "Mean profit gain after first learning phase",
         # caption = paste(as.character(hash)),
         fill = "") +

    geom_text(aes(label = labels), size = 5, color = "white") +

    scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab") +

    facet_grid(cols = vars(as.factor(player)))

# matrix of phase2
jpc_profitgain_phase2 <- ggplot(jpc_data[phase == 2,], aes(x = c1, y = c2, fill = V1)) +

    geom_tile(colour="white", size = 0.25) + theme_minimal() +

    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none",
          axis.title=element_text(size=18), strip.text.x = element_text(size = 18)) +

    labs(x = "Cost level of player 1", y = "Cost level of player 2",
         #title = "Mean profit gain following rematch after first learning phase",
         #caption = paste(as.character(hash)),
         fill = "") +

    geom_text(aes(label = labels), size = 6, color = "grey20") +

    #scale_fill_manual(values = rev(brewer.pal(11, "YlOrRd"))) +

    facet_grid(cols = vars(as.factor(player))) +

    scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")

# matrix of phase3
jpc_profitgain_phase3 <- ggplot(jpc_data[phase == 3,], aes(x = c1, y = c2, fill = V1)) +

  geom_tile(colour="white", size = 0.25) + theme_minimal() +

  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.title=element_text(size=18), strip.text.x = element_text(size = 18)) +

  labs(x = "Cost level of player 1", y = "Cost level of player 2",
       #title = "Mean profit gain following rematch after first learning phase",
       #caption = paste(as.character(hash)),
       fill = "") +

  geom_text(aes(label = labels), size = 6, color = "grey20") +

  #scale_fill_manual(values = rev(brewer.pal(11, "YlOrRd"))) +

  facet_grid(cols = vars(as.factor(player))) +

  scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")

# matrix of phase4
jpc_data[phase == 4, yviz := as.numeric(p1phase3oppcost)
        ][phase == 4 & c1 == 1,   yviz := as.numeric(p1phase3oppcost) - 1
        ][phase == 4 & c1 == 1.3 & p1phase3oppcost == 3,
          yviz := as.numeric(p1phase3oppcost) - 1]

jpc_profitgain_phase4 <- ggplot(jpc_data[phase == 4,],
                                aes(x = c1,
                                    y = yviz,
                                    fill = V1)) +

  geom_tile(colour="white", size = 0.25) + theme_minimal() +

  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.title=element_text(size=18), strip.text.x = element_text(size = 18)) +

  labs(x = "Cost level of both players", y = "Rival cost in previous learning",
       #title = "Mean profit gain following rematch after first learning phase",
       #caption = paste(as.character(hash)),
       fill = "") +

  geom_text(aes(label = labels), size = 6, color = "grey20") +

  #scale_fill_manual(values = rev(brewer.pal(11, "YlOrRd"))) +

  facet_grid(cols = vars(as.factor(player))) +

  scale_fill_gradient2(low = "blue4", midpoint = 0, high = "#c2133b", mid = "white",
                       limit = c(-0.15,1), space = "Lab")


# save plot
ggsave(file.path(maindir, "output", hash, "jpc_profitgain_phase1.pdf"), jpc_profitgain_phase1, device = "pdf",
       width = 290, height = 145, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_profitgain_phase2.pdf"), jpc_profitgain_phase2, device = "pdf",
       width = 290, height = 145, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_profitgain_phase3.pdf"), jpc_profitgain_phase3, device = "pdf",
       width = 290, height = 145, units = "mm")

# save plot
ggsave(file.path(maindir, "output", hash, "jpc_profitgain_phase4.pdf"), jpc_profitgain_phase4, device = "pdf",
       width = 210, height = 120, units = "mm")


# average proportional loss
# total
(jpc_data[phase == 2 & c1 == c2, mean(V1)] - jpc_data[phase == 2 & c1 != c2, mean(V1)])/
    jpc_data[phase == 2 & c1 == c2, mean(V1)]
# player1
(jpc_data[player == "Player 1" & phase == 2 & c1 == c2, mean(V1)] -
    jpc_data[player == "Player 1" & phase == 2 & c1 != c2, mean(V1)])/
  jpc_data[player == "Player 1" & phase == 2 & c1 == c2, mean(V1)]
# player2
(jpc_data[player == "Player 2" & phase == 2 & c1 == c2, mean(V1)] -
    jpc_data[player == "Player 2" & phase == 2 & c1 != c2, mean(V1)])/
  jpc_data[player == "Player 2" & phase == 2 & c1 == c2, mean(V1)]


# average efficiency cost
# cost are used as factors, so grab them again as numeric
jpc_data <- pgs[, .(mean(mprofit_gain), sd(mprofit_gain)/sqrt(pgs[, .N/uniqueN(match)])),
                by = .(c1, c2, phase, player)]
# phase 2
# total
(jpc_data[phase == 2 & (c1 < c2 & player == 1) | (c2 < c1 & player ==2), mean(V1)] -
    jpc_data[phase == 2 & (c1 > c2 & player == 1) | (c2 > c1 & player ==2), mean(V1)])/
  jpc_data[phase == 2 & (c1 < c2 & player == 1) | (c2 < c1 & player ==2), mean(V1)]
# player 1
(jpc_data[phase == 2 & c1 < c2 & player == 1, mean(V1)] -
    jpc_data[phase == 2 & c1 > c2 & player == 1, mean(V1)])/
  jpc_data[phase == 2 & (c1 < c2 & player == 1), mean(V1)]
# player 2
(jpc_data[phase == 2 & c2 < c1 & player ==2, mean(V1)] -
    jpc_data[phase == 2 & c2 > c1 & player ==2, mean(V1)])/
  jpc_data[phase == 2 & c2 < c1 & player ==2, mean(V1)]
# total for c-difference = 0.7
(jpc_data[phase == 2 & (c1 < c2 & c2 - c1 == 0.7 & player == 1) | (c2 < c1 & c1 - c2 == 0.7 & player ==2), mean(V1)] -
    jpc_data[phase == 2 & (c1 > c2 & c1 - c2 == 0.7 & player == 1) | (c2 > c1 & c2 - c1 == 0.7 & player ==2), mean(V1)])/
  jpc_data[phase == 2 & (c1 < c2 & c2 - c1 == 0.7 & player == 1) | (c2 < c1 & c1 - c2 == 0.7 & player ==2), mean(V1)]

# phase 3
# total
(jpc_data[phase == 3 & (c1 < c2 & player == 1) | (c2 < c1 & player ==2), mean(V1)] -
    jpc_data[phase == 3 & (c1 > c2 & player == 1) | (c2 > c1 & player ==2), mean(V1)])/
  jpc_data[phase == 3 & (c1 < c2 & player == 1) | (c2 < c1 & player ==2), mean(V1)]
# player 1
(jpc_data[phase == 3 & c1 < c2 & player == 1, mean(V1)] -
    jpc_data[phase == 3 & c1 > c2 & player == 1, mean(V1)])/
  jpc_data[phase == 3 & (c1 < c2 & player == 1), mean(V1)]
# player 2
(jpc_data[phase == 3 & c2 < c1 & player ==2, mean(V1)] -
    jpc_data[phase == 3 & c2 > c1 & player ==2, mean(V1)])/
  jpc_data[phase == 3 & c2 < c1 & player ==2, mean(V1)]
# total for c-difference = 0.7
(jpc_data[phase == 3 & (c1 < c2 & c2 - c1 == 0.7 & player == 1) | (c2 < c1 & c1 - c2 == 0.7 & player ==2), mean(V1)] -
    jpc_data[phase == 3 & (c1 > c2 & c1 - c2 == 0.7 & player == 1) | (c2 > c1 & c2 - c1 == 0.7 & player ==2), mean(V1)])/
  jpc_data[phase == 3 & (c1 < c2 & c2 - c1 == 0.7 & player == 1) | (c2 < c1 & c1 - c2 == 0.7 & player ==2), mean(V1)]



rm(jpc_collusionindex_phase1, jpc_collusionindex_phase2, jpc_collusionindex_phase3,
   jpc_collusionindex_phase4,
   jpc_profitgain_phase1, jpc_profitgain_phase2, jpc_profitgain_phase3,
   jpc_profitgain_phase4,
   jpc_data)


# -------------------------- equilibria frequencies ---------------------------------

# across all phases and matches
visdata <- unique(cyc[, .(phase, match, match_type, run, eq_type)])

eqtype_frequency <- ggplot(unique(visdata[, .N, by = eq_type]), aes(x = eq_type, y = N)) +
  geom_bar(stat = "identity", color="black", fill="#ff8590") + theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   margin=margin(0,0,0,0,"pt")))


ggsave(file.path(maindir, "output", hash, "equilibria_frequency.pdf"), eqtype_frequency,
       device = "pdf", width = 210, height = 210, units = "mm")

# for phase 2 by match_type
eqtype_frequency <- ggplot(unique(visdata[phase == 2, .N, by = .(phase, eq_type, match_type)]),
                           aes(x = eq_type, y = N)) +
    geom_bar(stat = "identity", color="black", fill='#ff8590') +
    facet_grid(cols = vars(as.factor(match_type))) + theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   margin=margin(0,0,0,0,"pt")))

ggsave(file.path(maindir, "output", hash, "equilibria_frequency_phase2.pdf"), eqtype_frequency,
       device = "pdf", width = 210, height = 290, units = "mm")

# for phase 3 by match_type
eqtype_frequency <- ggplot(unique(visdata[phase == 3, .N, by = .(phase, eq_type, match_type)]),
                           aes(x = eq_type, y = N)) +
  geom_bar(stat = "identity", color="black", fill='#ff8590') +
  facet_grid(cols = vars(as.factor(match_type)))  + theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   margin=margin(0,0,0,0,"pt")))

ggsave(file.path(maindir, "output", hash, "equilibria_frequency_phase3.pdf"), eqtype_frequency,
       device = "pdf", width = 210, height = 290, units = "mm")


# by phase and match_type
eqtype_frequency <- ggplot(unique(visdata[, .N, by = .(phase, eq_type, match_type)]),
       aes(x = eq_type, y = N)) +
  geom_bar(stat = "identity", color="black", fill='#ff8590') +
  facet_grid(cols = vars(as.factor(match_type)), rows = vars(as.factor(phase))) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                   margin=margin(0,0,0,0,"pt")))

ggsave(file.path(maindir, "output", hash, "equilibria_frequency_3phases.pdf"), eqtype_frequency,
       device = "pdf", width = 290, height = 290, units = "mm")

# for phase 2 by match
visdata <- unique(cyc[, .(phase, match, match_type, run, eq_type, c1, c2)])
visdata[, c2 := factor(c2, levels = c(1.7,1.6,1.5,1.4,1.3,1.2,1.1, 1))] #to reverse oder in plot and align with jpc plot order
eqtype_frequency <- ggplot(unique(visdata[phase == 2, .N, by = .(eq_type, c1, c2)]),
                           aes(x = eq_type, y = N)) +
    geom_bar(stat = "identity", color="black", fill="#ff8590") +
    facet_grid(rows = vars(as.factor(c2)),
               cols = vars(as.factor(c1)),
               switch = "both", drop = TRUE,) +
    labs(x = "Player 1 cost level", y = "Player 2 cost level") +
    theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                            axis.text.x = element_blank(), axis.text.y = element_blank(),
                            axis.ticks = element_blank(),
                            axis.title = element_text(size = 14))

                            # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                            #                            margin=margin(0,0,0,0,"pt")))

ggsave(file.path(maindir, "output", hash, "equilibria_frequency_bymatch_phase2.pdf"), eqtype_frequency,
       device = "pdf", width = 210, height = 210, units = "mm")

# for phase 3 by match
eqtype_frequency <- ggplot(unique(visdata[phase == 3, .N, by = .(eq_type, c1, c2)]),
                           aes(x = eq_type, y = N)) +
  geom_bar(stat = "identity", color="black", fill="#ff8590") +
  facet_grid(rows = vars(as.factor(c2)),
             cols = vars(as.factor(c1)),
             switch = "both", drop = TRUE,) +
  labs(x = "Player 1 cost level", y = "Player 2 cost level") +
  theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                          axis.text.x = element_blank(), axis.text.y = element_blank(),
                          axis.ticks = element_blank(),
                          axis.title = element_text(size = 14))

ggsave(file.path(maindir, "output", hash, "equilibria_frequency_bymatch_phase3.pdf"), eqtype_frequency,
       device = "pdf", width = 210, height = 210, units = "mm")


rm(visdata, eqtype_frequency)

# ----------------------------- time to convergence ----------------------------------

convergence_phase1 <- ggplot(unique(pgs[phase == 1, .(phase, match, match_type, finaliteration)]),
                          aes(x = finaliteration)) +
    geom_histogram(color="black", fill="#ff8590") +
    theme_minimal() + labs(x = "Final iteration", y = "N") +
    xlim(800000, 5580000)

convergence_phase3 <- ggplot(unique(pgs[phase == 3, .(phase, match, match_type, finaliteration)]),
                             aes(x = finaliteration)) +
  geom_histogram(color="black", fill="#ff8590") +
  theme_minimal() + labs(x = "Final iteration", y = "N") +
  xlim(800000, 5580000)


ggsave(file.path(maindir, "output", hash, "convergence_phase1.pdf"), convergence_phase1,
       device = "pdf", width = 210, height = 140, units = "mm")

ggsave(file.path(maindir, "output", hash, "convergence_phase3.pdf"), convergence_phase3,
       device = "pdf", width = 210, height = 140, units = "mm")


rm(convergence_phase1, convergence_phase3)


# ----------------------- frequency of individual actions ----------------------------

# NOTE: the function excludes the first 2 iterations for all runs! I do this to exclude the
# first random round and one period of reaction to it. Can vary or delete of course
actsplyd <- rbindlist(lapply(datadirs, actions.get))

# add cost levels from pgs
actsplyd <- merge(actsplyd, unique(pgs[, .(phase, match, run, c1, c2)]),
                  by = c("phase", "match", "run"))

# look at total number of times each price is played
actsplyd[, N := sum(nplayed), by = .(phase, match, action)]
actsplyd[, Nplayer := sum(nplayed), by = .(phase, match, action, player)]
visdata <- unique(actsplyd[, .(phase, match, action, N, c1, c2)])

visdata[, c2 := factor(c2, levels = c(1.7,1.6,1.5,1.4,1.3,1.2,1.1, 1))] #to reverse oder in plot and align with jpc plot order
ggplot(visdata[phase == 1, ], aes(x = as.factor(action), y = N)) +
    geom_bar(stat = "identity", color = "grey20", fill = "lightblue", size = 0.01) +
    facet_grid(cols = vars(c1), rows = vars(c2)) +
    labs(x = "Player 1 from experiment", y = "Player 2 from experiment",
         title = "Frequency of prices",
         # caption = paste(as.character(hash)),
         fill = "reward") +
    theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                                       margin=margin(0,0,0,0,"pt")))

# look at number of times each price is played by player
visdata <- unique(actsplyd[, .(phase, match, action, player, Nplayer, c1, c2)])

linedata <- data.frame(c1 = as.factor(c(1, 1, 1.3, 1.3, 1.7, 1.7)),
                       c2 = as.factor(c(1, 1, 1.3, 1.3, 1.7, 1.7)),
                       prices = c(1.47, 1.92, 1.77, 2.22, 2.17, 2.62),
                       lines = c("Nash", "Max", "Nash", "Max", "Nash", "Max"),
                       color = c("red", "blue", "red", "blue", "red", "blue"))

visdata[, c2 := factor(c2, levels = c(1.7, 1.3, 1))] #to reverse oder in plot and align with jpc plot order

actions_frequency_phase1 <- ggplot(visdata[phase == 1,],
                                   aes(x = action, y = Nplayer, fill = as.factor(player))) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(c1), rows = vars(c2), switch = "both") +
    geom_vline(data = linedata, aes(xintercept = prices, color = lines), alpha = 0.7, linetype = "dashed") +
    labs(x = "Player 1 cost level", y = "Player 2 cost level",
         # title = "Frequency of prices",
         # caption = paste(as.character(hash)),
         fill = "Player",
         color = "Prices") +
    theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                            axis.text.x = element_blank()) +
    scale_color_manual(values = linedata$color)

actions_frequency_phase2 <- ggplot(visdata[phase == 2,],
                                   aes(x = action, y = Nplayer, fill = as.factor(player))) +
    geom_bar(stat = "identity") +
    facet_grid(cols = vars(c1), rows = vars(c2), switch = "both") +
    geom_vline(data = linedata, aes(xintercept = prices, color = lines), alpha = 0.7, linetype = "dashed") +
    labs(x = "Player 1 cost level", y = "Player 2 cost level",
         # title = "Frequency of prices",
         # caption = paste(as.character(hash)),
         fill = "Player",
         color = "Prices") +
    theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                            axis.text.x = element_blank()) +
    scale_color_manual(values = linedata$color)


actions_frequency_phase3 <- ggplot(visdata[phase == 3,],
                                   aes(x = action, y = Nplayer, fill = as.factor(player))) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(c1), rows = vars(c2), switch = "both") +
  geom_vline(data = linedata, aes(xintercept = prices, color = lines), alpha = 0.7, linetype = "dashed") +
  labs(x = "Player 1 cost level", y = "Player 2 cost level",
       # title = "Frequency of prices",
       # caption = paste(as.character(hash)),
       fill = "Player",
       color = "Prices") +
  theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                          axis.text.x = element_blank()) +
  scale_color_manual(values = linedata$color)

actions_frequency_phase4 <- ggplot(visdata[phase == 4,],
                                   aes(x = action, y = Nplayer, fill = as.factor(player))) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(c1), rows = vars(c2), switch = "both") +
  geom_vline(data = linedata, aes(xintercept = prices, color = lines), alpha = 0.7, linetype = "dashed") +
  labs(x = "Player 1 cost level", y = "Player 2 cost level",
       # title = "Frequency of prices",
       # caption = paste(as.character(hash)),
       fill = "Player",
       color = "Prices") +
  theme_minimal() + theme(panel.spacing = unit(1, "lines"),
                          axis.text.x = element_blank()) +
  scale_color_manual(values = linedata$color)



ggsave(file.path(maindir, "output", hash, "actions_frequency_phase1.pdf"), actions_frequency_phase1,
       device = "pdf", width = 210, height = 210, units = "mm")
ggsave(file.path(maindir, "output", hash, "actions_frequency_phase2.pdf"), actions_frequency_phase2,
       device = "pdf", width = 210, height = 210, units = "mm")
ggsave(file.path(maindir, "output", hash, "actions_frequency_phase3.pdf"), actions_frequency_phase3,
       device = "pdf", width = 210, height = 210, units = "mm")
ggsave(file.path(maindir, "output", hash, "actions_frequency_phase4.pdf"), actions_frequency_phase4,
       device = "pdf", width = 210, height = 210, units = "mm")

rm(actions_frequency_phase1, actions_frequency_phase2, actions_frequency_phase3,
   actions_frequency_phase4, linedata, visdata)


# ---- code prices based on position on grid relative to players' nash price
pgrid <- as.data.table(unique(actsplyd$action))
setorder(pgrid)
pgrid <- as.data.table(rbind(pgrid, pgrid, pgrid, pgrid, pgrid, pgrid, pgrid, pgrid)) #repeat by no of cost levels
pgrid[c(1:20), priceposition := as.factor(str_subset(seq(-2, 18, by = 1), "^0", negate = TRUE))
      ][c(1:20), c := 1] #p^N = 1.47
pgrid[c(61:80), priceposition := as.factor(str_subset(seq(-6, 14, by = 1), "^0", negate = TRUE))
      ][c(61:80), c := 1.3] #p^N = 1.77
pgrid[c(141:160), priceposition := as.factor(str_subset(seq(-12, 8, by = 1), "^0", negate = TRUE))
      ][c(141:160), c := 1.7] #p^N = 2.17

visdata <- unique(actsplyd[, .(phase, match, action, player, Nplayer, c1, c2)])
visdata <- rbind(merge(visdata[player == 1,], pgrid, by.x = c("action","c1"), by.y = c("V1","c")),
                 merge(visdata[player == 2,], pgrid, by.x = c("action","c2"), by.y = c("V1","c")))

visdata[, costdifference := as.factor(abs(round(c1 - c2, digits = 2)))]
visdata[, N := sum(Nplayer), by = .(phase, priceposition, costdifference)]
visdata[, Nmatches := uniqueN(match), by = .(phase,costdifference)]
visdata[, N := N/(Nmatches)]

visdata <- unique(visdata[, .(phase, priceposition, N, costdifference)])
visdata[, priceposition := factor(priceposition, levels = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,
                                                          1,2,3,4,5,6,7,8,9,10,11,12,
                                                          13,14,15,16,17,18))]

actions_frequency_reltonash_phase2 <- ggplot(visdata[phase == 2 & costdifference != 0, ],
       aes (x = priceposition, y = N)) +
  xlab("Price position on grid relative to players Nash price") +
  ylab("N") + labs(title = "Prices played by absolute cost difference") +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  facet_grid(rows=vars(costdifference))

actions_frequency_reltonash_phase4 <- ggplot(visdata[phase == 4, ],
                                      aes (x = priceposition, y = N)) +
  xlab("Price position on grid relative to players Nash price") +
  ylab("N") + labs(title = "Prices played by absolute cost difference") +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  facet_grid(rows=vars(costdifference))


ggsave(file.path(maindir, "output", hash, "actions_frequency_reltonash_phase2.pdf"),
       actions_frequency_reltonash_phase2,
       device = "pdf", width = 210, height = 210, units = "mm")
ggsave(file.path(maindir, "output", hash, "actions_frequency_reltonash_phase4.pdf"),
       actions_frequency_reltonash_phase4,
       device = "pdf", width = 210, height = 210, units = "mm")


visdata <- data.frame(visdata[costdifference != 0,])
visdata <- as.data.table(visdata[rep(row.names(visdata), visdata$N), 1:4])



library(latex2exp)
actions_frequency_reltonash2 <- ggplot(data=visdata[phase==2,],
                                       aes(x=priceposition, group=costdifference)) +
  geom_histogram(stat = "count", alpha=0.5, width = 1, size = 0.5,
                 fill = '#ff8590', color = "black") +
  theme_minimal() +
  theme(legend.position = "none", strip.text.x = element_text(size = 12),
        panel.spacing = unit(0.8, "lines")) +
  theme(axis.text.x = element_blank(), axis.title = element_blank()) +
  coord_cartesian(ylim=c(-0,8000), clip = "off") +
  scale_x_discrete(breaks = c(-9,-7,-5,-3,-1,2,4,6,8,10,12,14,16,18)) +
  facet_wrap(~costdifference, nrow = 2) +
  geom_segment(aes(x = 7.5, y = -50, xend = 7.5, yend = 8000),
               size = 1, linetype = "dashed", color = "blue") +
  annotate(geom="text",x = 7.9, y = -500,
           label = TeX("$p^N$", output="character"), parse = TRUE, size = 4)

ggsave(file.path(maindir, "output", hash, "actions_frequency_reltonash_phase2_2.pdf"),
       actions_frequency_reltonash2,
       device = "pdf", width = 210, height = 140, units = "mm")


rm(actions_frequency_reltonash_phase2, actions_frequency_reltonash_phase4,
   actions_frequency_reltonash2, visdata, pgrid)

# ----------------------- rank frequency of price cycles -----------------------------

# test <- merge(cyc[player == 1,], pgrid, by.x = c("from","c1"), by.y = c("V1","c"))
# test <- rbind(test, merge(cyc[player == 2,], pgrid, by.x = c("from","c2"), by.y = c("V1","c")))
# setkey(test, phase, match, run, from, to, player)
#
# test[, matchrunid := .GRP, by = .(match, run)]
# for(i in unique(test$matchrunid))
# {
#   test[matchrunid == i & player == 1, p1.concatenatecycle := paste(priceposition, collapse = "")
#   ][matchrunid == i & player == 2, p2.concatenatecycle := paste(priceposition, collapse = "")]
# }
# # copy cycles from one player to obs of the other
# test[,p1.concatenatecycle := p1.concatenatecycle[!is.na(p1.concatenatecycle)][1L], by = matchrunid
# ][,p2.concatenatecycle := p2.concatenatecycle[!is.na(p2.concatenatecycle)][1L], by = matchrunid]
#
# test[, cycleid := .GRP, by = .(p1.concatenatecycle, p2.concatenatecycle)]
# test[, cycleN := uniqueN(matchrunid), by = cycleid]
# unique(test[match_type == "control", .(cycleid, cycleN, eq_type)])[order(-cycleN)]
# unique(test[match_type == "rematch", .(cycleid, cycleN, eq_type)])[order(-cycleN)]
#
# #look at most frequent cycle from rematched play
# test[cycleid == test[match_type == "rematch" & !(eq_type %in% c("sym1","asym1","other")),
#   ][order(-cycleN)][1]$cycleid, .(from, to, N, player)]
#
# # begin plotting it
# require(network)
# require(sna)
# require(ggnetwork)
#
# cycle_network <- ggnetwork(as.network(cyc[cycleid == cyc[match_type == "rematch" &
#                                                            !(eq_type %in% c("sym1","asym1","other")),
# ][order(-cycleN)][1]$cycleid,
# .(from, to, N, player)],
# directed = TRUE,
# loops = TRUE,
# matrix.type = "edgelist"),
# layout = "kamadakawai")
#
# cyclevis <- cyc[cycleid == cyc[match_type == "rematch" &
#                                  !(eq_type %in% c("sym1","asym1","other")),
# ][order(-cycleN)][1]$cycleid,
# .(from, to, N, player)]
#
# cycle_edge_df <- data.frame(from = edge1,
#                             to   = edge2)
#
# cycle_network <- ggnetwork(as.network(cyclevis[, .(from, to, player, N)],
#                                       vertices = data.frame(name = unique(cyclevis$from)),
#                                       directed = TRUE, loops = TRUE))
#
# ggplot(cycle_network, aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_edges(color = "black", size = 2) +
#   geom_nodes(color = "black", size = 10) +
#   geom_nodelabel(aes(label = vertex.names)) +
#   #geom_edgetext(aes(label = ), color = "black")
#   theme_blank()

#============================= SAVE SCRIPTS FOR HASH ================================
# NOTE: this code only runs in Rstudio! not aware of an equivalent for base R
# condition on path so it doesnt copy if run from inside hash directory later
if(str_detect(rstudioapi::getSourceEditorContext()$path, "rscripts") == TRUE)
{
file.copy(from = rstudioapi::getSourceEditorContext()$path,
          to = file.path(maindir, "output", hash,
                         str_replace(str_extract(rstudioapi::getSourceEditorContext()$path, "\\/[^\\/]*$"),
                                     "/", "")),
          overwrite = TRUE)
file.copy(from = "RScripts/asymlearners_aux_funcs.R",
          to = file.path(maindir, "output", hash, "asymlearners_aux_funcs.R"),
          overwrite = TRUE)
file.copy(from = "RScripts/asymlearners_scp_download.R",
          to = file.path(maindir, "output", hash, "asymlearners_scp_download.R"),
          overwrite = TRUE)
}


#------------- redo calvano table 1 with a calculation of sd for full pupulation ------------------
# may not be necessary

# calculate correct standard deviation over population of all subsamples (runs) as weighted
# average of subsample sds and correction term. weight per var is nobs-1 = 99
# varsubsample <- pgs_control[, unique(sdprofit_gain), by = .(player, match, run)]$V1 *
#                 pgs_control[, unique(sdprofit_gain), by = .(player, match, run)]$V1
# mtotal       <- pgs_control[, mean(mprofit_gain), by = player]$V1
# msubsample   <- pgs_control[, mean(mprofit_gain), by = .(match, run, player)]$V1
# sdtotal_p1   <- sqrt((99 * sum(varsubsample[seq(1, length(varsubsample), 2)]) +
#                       sum(100 * (msubsample[seq(1, length(msubsample), 2)] - mtotal[1]) *
#                           (msubsample[seq(1, length(msubsample), 2)] - mtotal[1]) *
#                            nrow(unique(pgs_control[,.(match,run)]))))/
#                      (100 * nrow(unique(pgs_control[,.(match,run)])) - 1)
#                     )
# sdtotal_p2   <- sqrt((99 * sum(varsubsample[seq(2, length(varsubsample), 2)]) +
#                           sum(100 * (msubsample[seq(2, length(msubsample), 2)] - mtotal[2]) *
#                                   (msubsample[seq(2, length(msubsample), 2)] - mtotal[2]) *
#                                   nrow(unique(pgs_control[,.(match,run)]))))/
#                          (100 * nrow(unique(pgs_control[,.(match,run)])) - 1)
#                     )
# sds <- cbind(c("all"), rbind(c(1),c(2)), rbind(sdtotal_p1, sdtotal_p2))
# rm(varsubsample, mtotal, msubsample, sdtotal_p1, sdtotal_p2)
# # and for each equilibrium type
# for(i in unique(pgs_control$type)){
#     p <- pgs_control[type == i,]
#     varsubsample <- p[, unique(sdprofit_gain), by = .(player, match, run)]$V1 *
#                     p[, unique(sdprofit_gain), by = .(player, match, run)]$V1
#     mtotal       <- p[, mean(mprofit_gain), by = player]$V1
#     msubsample   <- p[, mean(mprofit_gain), by = .(match, run, player)]$V1
#     sdtotal_p1   <- sqrt((99 * sum(varsubsample[seq(1, length(varsubsample), 2)]) +
#                               sum(100 * (msubsample[seq(1, length(msubsample), 2)] - mtotal[1]) *
#                                       (msubsample[seq(1, length(msubsample), 2)] - mtotal[1]) *
#                                       nrow(unique(p[,.(match,run)]))))/
#                              (100 * nrow(unique(p[,.(match,run)])) - 1)
#     )
#     sdtotal_p2   <- sqrt((99 * sum(varsubsample[seq(2, length(varsubsample), 2)]) +
#                               sum(100 * (msubsample[seq(2, length(msubsample), 2)] - mtotal[2]) *
#                                       (msubsample[seq(2, length(msubsample), 2)] - mtotal[2]) *
#                                       nrow(unique(p[,.(match,run)]))))/
#                              (100 * nrow(unique(p[,.(match,run)])) - 1)
#     )
#     sds <- rbind(sds, cbind(c(paste(i)), rbind(c(1), c(2)), rbind(sdtotal_p1, sdtotal_p2)))
# }
# rm(i, p, varsubsample, mtotal, msubsample, sdtotal_p1, sdtotal_p2)
# sds <- as.data.table(sds)
# setnames(sds, c("type", "player", "sd"))
# sds[, player := as.integer(player)]
# setkey(sds, type, player)
# # mean of profit gains
# ms <- pgs_control[, mean(mprofit_gain), by = player]
# ms <- cbind(c("all"), ms)
# ms <- as.data.table(rbind(pgs_control[, mean(mprofit_gain), by = .(type, player)], ms, use.names = FALSE))
# setnames(ms, c("type", "player", "mean"))
# setkey(ms, type, player)
#
# # summary table
# sumtable_control <- sds[ms]
# rm(sds, ms)