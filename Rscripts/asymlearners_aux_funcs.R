require(stringr)

# calculate demand for given price vector and logit demand function
logit_demand <- function(params, price_vec){
  #' Returns a quantity vector for a given price vector when for logit demand.
  diff_vec <- as.numeric(t(params)[str_detect(names(params), "expA[:digit:]")])
  price_vec <-  c(price_vec, 0) # zero for outside option. Ensure this is last in diff_vec.

  return((exp((diff_vec - price_vec)/params$expMu)/
            sum(exp((diff_vec - price_vec)/params$expMu), na.rm = TRUE))
         [1:(length(diff_vec)-1)]) #dont return outside good quantity
}

require(data.table)

# calculate profit metrics for one run
profit_gains.get <- function(datadir, benchmarks){

  # grab param values
  params <- fread(file.path(datadir, "parameters.csv"))

  # get rewards data
  r <- fread(file.path(datadir, paste("rewardsExtendedEndNLines.csv")))
  setnames(r, c("iteration","player","state_action_index","action_choice","explore_rate","reward"))

  # add final iteration to study time to convergence
  r[, finaliteration := max(r$iteration)]

  # Add indicator if there was any randomness during the last runs
  r[, any_random := any(r$action_choice != "Exploitation")]

  # Calculate profit metrics: mean reward, profit delta + mean and sd, collusion index, surplus share
  # chaining data table assignments is like ifelse but faster
  r[, mreward               := mean(reward), by = player]
  r[, profit_gain           := (reward - unique(benchmarks$nash_profit))/
                                (unique(benchmarks$mon_profit) - unique(benchmarks$nash_profit))]

  r[, mprofit_gain          := mean(profit_gain), by = player]
  r[, sdprofit_gain         := sd(profit_gain),   by = player]

  r[, collusion_index       := (sum(reward) - sum(benchmarks$nash_profit))/
      (sum(benchmarks$mon_profit) - sum(benchmarks$nash_profit)), by = iteration]
  r[, mcollusion_index      := mean(collusion_index), by = player]
  r[, sdcollusion_index     := sd(collusion_index), by = player]

  r[, surplus_frac_player   := (reward - unique(benchmarks$nash_profit))/
                                (sum(reward) - sum(benchmarks$nash_profit)),
    by = iteration]

  r[, msurplus_frac_player := mean(surplus_frac_player), by = player]

  # return only mean reward, mean profit gain, sd profit gain, collusion index, surplus share
  r <- unique(r[, .(player, finaliteration,
                    mreward, mprofit_gain, sdprofit_gain, mcollusion_index,
                    sdcollusion_index, msurplus_frac_player)])

  # find match and add to the returned output
  r[, match := as.character(sub("_", "", str_extract(datadir,
                pattern = "p?[0-9]?e?[0-9]?e?[0-9]?[0-9]?p?[0-9]?e?[0-9]?[0-9]?e[0-9]{1,2}_")))]

  # find run number and add to returned output
  r[, run := as.integer(str_extract(str_extract(datadir, pattern = "run_?[0-9]+"), pattern = "[0-9]+"))]

  # find phase number
  r[, phase := as.integer(str_extract(str_extract(datadir, pattern = "phase[0-9]{1,2}"), pattern = "[0-9]{1,2}"))]

  # add alpha and beta for apg matrix over these two parameters
  r[, beta  := params[["expBeta"]]]
  r[, alpha := params[["expLearningRate"]]]

  # add economic parameters for jpc matrix
  r[, c1  := params[["expC1"]]]
  r[, c2  := params[["expC2"]]]
  r[, a1  := params[["expA1"]]]
  r[, a2  := params[["expA2"]]]

  return(r)

}


# Calculate metrics across runs

summary_table <- function(pg.dt, outcome_vars, group = c()){
  #' This function calls summary_table_component twice to generate both means
  #' and sds of outcome_vars, by group. Both sets of variables should be in pg.dt.
  pg.dt_m <- summary_table_component(pg.dt, outcome_vars, mean, group)
  pg.dt_sd <- summary_table_component(pg.dt, outcome_vars, sd, group)

  pg.dt_summary <- merge(pg.dt_m, pg.dt_sd, by.x = group, by.y = group,
                         suffixes = c(".m", ".sd"))
  return(pg.dt_summary)
}

summary_table_component <- function(pg.dt, outcome_vars, summary_function, group = c()){
  #' @param pg.dt, a data.table consisting of at least the variables in outcome_vars.
  #' @param outcome_vars a vector of variable names on which the summary_function will be applied.
  #' @param summary_function a aggregating function to apply on the outcome_vars.
  #' @param group a grouping vector. Give variable names at which level results will be grouped.
  #' @returns pg.dt_summary, a data.table containing the summary statistics by
  #' the groups in the parameter group.

  pg.dt_summary <- pg.dt[,  lapply(.SD, summary_function), by = group, .SDcols = outcome_vars]

  return(pg.dt_summary)
}

# obtain state action choices played
actions.get <- function(datadir){

    # grab rewards data
    d <- fread(file.path(datadir, paste("rewardsExtendedEndNLines.csv")))
    setnames(d, c("iteration","player","state_action_index","action_choice","explore_rate","reward"))

    # grab state action index data
    # player 1
    p <- fread(file.path(datadir, paste("state_action_index_1.csv")))
    m <- merge(d[player == 1,], unique(p[,.(action_index, state, action)]),
               by.x = c("state_action_index"),
               by.y = c("action_index"))
    # player 2
    p <- fread(file.path(datadir, paste("state_action_index_2.csv")))
    m <- rbind(m, merge(d[player ==2,], unique(p[,.(action_index, state, action)]),
                        by.x = c("state_action_index"),
                        by.y = c("action_index")))
    setkey(m, iteration, player)

    # #exclude random start and 1 following action. can also only exclude first round? TODO
    # m  <- m[iteration > 2,]

    # add number of times an action is played for each action x player combination
    m[iteration > 2, nplayed := .N, by = .(player, action)] # NOTE: changed from "state_action_index" to "action"

    #keep only actions x player
    m <- unique(m[iteration > 2, .(player, action, nplayed)])


    # m[, nactions  := uniqueN(action)]
    # m[, lengthcyc := uniqueN(action), by = .(player)]
    #
    # m[, type := "cyc3_+"
    #   ][max(lengthcyc) == 1 & nactions == 2, type := "asy1"
    #     ][lengthcyc == 2 & nactions <= 4, type := "cyc2"] # TODO: should both be at most length 2?

    # find match and add to the returned output
    m[, match := as.character(sub("_", "", str_extract(datadir,
                  pattern = "p?[0-9]?e?[0-9]?e?[0-9]?[0-9]?p?[0-9]?e?[0-9]?[0-9]?e[0-9]{1,2}_")))]

    # find run number and add to returned output
    m[, run := as.integer(str_extract(str_extract(datadir, pattern = "run_?[0-9]+"), pattern = "[0-9]+"))]

    # find phase number
    m[, phase := as.integer(str_extract(str_extract(datadir, pattern = "phase[0-9]{1,2}_"), pattern = "[0-9]{1,2}"))]

    # add indicator if exploration still occurs
    m[, any_random := any(d$action_choice != "Exploitation")]

    # return only unique combination
    m <- unique(m[,.(player, action, nplayed, any_random, phase, match, run)])

    return(m)
}

#
# eq.get <- function(cyc){
#
#   cyc[, nactions  := uniqueN(action), by = .(match, run)]
#   cyc[, lengthcyc := uniqueN(action), by = .(match, run, player)]
#
#   # label convergenc type as asy1, cyc2, cyc3+
#   # code below works like ifelse but faster
#   cyc[lengthcyc == 1 & nactions == 1, eq_type := "sym1"
#       ][lengthcyc == 1 & nactions == 2, eq_type := "asym1"
#         ][nactions == 3, eq_type := "cyc2"
#           ][nactions == 4, eq_type := "cyc2"
#            ][is.na(eq_type), eq_type := "cyc3_+"]
#   cyc[, tag := uniqueN(eq_type), by = .(match, run)] #hack to get code to work. previous version not correct
#   cyc[tag == 2, eq_type := "cyc3_+"]
#   cyc[, tag := NULL]
#
#   # cyc[, type := "cyc3_+"][max(lengthcyc) == 1 & nactions == 2, type := "asy1", by = .(match, run)
#   #                        ][max(lengthcyc) == 1 & nactions == 1, type := "sym1", by = .(match, run)
#   #                         ][lengthcyc == 2, type := "cyc2", by = run]
#
#   return(cyc)
# }



# find repeated sequence of numbers where the length of the sequence is specified as N
# with efficient data table shift function. input is a vector
findRptSeqs <- function(x, N)
{
    # split input vector into groups
    splits <- ceiling(seq_along(x) / N)
    # use data.table shift to create overlapping windows
    w <- lapply(data.table::shift(x, 0:(N-1), type = "lead"), function(x) {
                res <- split(x, splits)
                res[lengths(res) == N]
                })
    w <- data.table(na.omit(t(as.data.frame(w)))) #final row has an NA by construction
    # use data.table by to count repeated rows
    w <- w[, .N, by = names(w)]
    return(w[N>1,]) # N>1 just in case
}

# use findRptSeqs function to iterate over sequences length from 2 to maxseq for player 1
# stop if sequences found explain 80 of the iterations and determine the action cycle
# then match player 2s actions. if no sequence found, try player 2. if none found, return NA
# unwieldy code, needs to account for many exceptions. in particular, random start to play
# causes runs that actually converged to look like cycle2 or cycle3
findRptPlays <- function(datadir, maxseq)
{
    # read in reward data
    d <- fread(file.path(datadir, paste("rewardsExtendedEndNLines.csv")))
    setnames(d, c("iteration","player","state_action_index","action_choice","explore_rate","reward"))

    # grab state action index data
    # player 1
    p <- fread(file.path(datadir, paste("state_action_index_1.csv")))
    m <- merge(d[player == 1,], unique(p[,.(action_index, state, action)]),
               by.x = c("state_action_index"),
               by.y = c("action_index"))
    # player 2
    p <- fread(file.path(datadir, paste("state_action_index_2.csv")))
    m <- rbind(m, merge(d[player == 2,], unique(p[,.(action_index, state, action)]),
                        by.x = c("state_action_index"),
                        by.y = c("action_index")))
    setkey(m, iteration, player)

    # find sym1 and asym1 equilibria
    # random start can cause more than one distinct action despite convergence. condition for it
    # if players' most common actions are equal, symmetric equilibrium, otherwise asymmetric
    if(m[player == 1, .N, by = action][order(-N)][1,2] >= 80 &
       m[player == 2, .N, by = action][order(-N)][1,2] >= 80) {

        if(m[player == 1, .N, by = action][order(-N)][1,1] ==
           m[player == 2, .N, by = action][order(-N)][1,1]) {
            idcycle <- cbind(rbind(cbind("from" = m[player == 1, .N, by = action][order(-N)][1,1],
                                         "to"   = m[player == 1, .N, by = action][order(-N)][1,1],
                                         "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                         "player" = 1),
                                   cbind("from" = m[player == 2, .N, by = action][order(-N)][1,1],
                                         "to"   = m[player == 2, .N, by = action][order(-N)][1,1],
                                         "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                         "player" = 2)),
                             "eq_type"   = "sym1",
                             "p1.cycle"  = NA,
                             "p2.cycle"  = NA,
                             "equalcycle" = NA)
        } else {
            idcycle <- cbind(rbind(cbind("from" = m[player == 1, .N, by = action][order(-N)][1,1],
                                         "to"   = m[player == 1, .N, by = action][order(-N)][1,1],
                                         "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                         "player" = 1),
                                   cbind("from" = m[player == 2, .N, by = action][order(-N)][1,1],
                                         "to"   = m[player == 2, .N, by = action][order(-N)][1,1],
                                         "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                         "player" = 2)),
                                        "eq_type"   = "asym1",
                             "p1.cycle"  = NA,
                             "p2.cycle"  = NA,
                             "equalcycle" = NA)
        }
    } else { # otherwise start looking for repeated sequences of player 1
        windowsizes <- seq(2, maxseq, by = 1)
        rptseqs <- data.table()
        for(i in windowsizes) {
            rptseqs <- rbind(rptseqs,
                             na.omit(cbind(findRptSeqs(m[player == 1]$action, N = i), "seqlength" = i)),
                             fill = TRUE)
            # end loop if those sequences that show up more than rarely explain >=80 of actions
            # N>7 should be fine for all windowsizes <= 15, which is already very large
            if(rptseqs[seqlength == i & N>7, sum(N)] >= 80) {
                break
            }
        }

        # exclude sequences that only show up rarely. see above
        rptseqs <- rptseqs[N>7,]

        # check if it really is a cycle, i.e. sequences are repeated (almost) the same number of times. allow
        # for margin because of random start. if yes, grab the cycle
        if(rptseqs[seqlength == i, sum(N)] >= 80 & !(nrow(rptseqs) == 1) & rptseqs[, max(N) - min(N)] <= 2 & rptseqs[,.N] != 0) {
            rptseqs <- rptseqs[, .(V1, V2, N)]
            setnames(rptseqs, c("from", "to", "N"))
            rptseqs[, player := 1]
            # run findRptSeqs for p2 and see if the opposing players actions correspond
            rptseqs2 <- findRptSeqs(m[player == 2,]$action, N = 2)
            rptseqs2 <- rptseqs2[N>7,]
            setnames(rptseqs2, c("from", "to", "N"))
            rptseqs2[, player := 2]
            # if they do, then return them as idcycle
            if(nrow(rptseqs) == nrow(rptseqs2)) {
                idcycle <- cbind(rbind(rptseqs, rptseqs2),
                                 "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                 "p1.cycle" = TRUE,
                                 "p2.cycle" = TRUE,
                                 "equalcycle" = TRUE)
            } else if(rptseqs2[, sum(N)] >= 80 & !(nrow(rptseqs2) == 1) & rptseqs2[, max(N) - min(N)] <= 2 & rptseqs[,.N] != 0) {
            #^if they dont, but p2 is cycling too
                idcycle <- cbind(rbind(rptseqs, rptseqs2),
                                 "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                 "p1.cycle" = TRUE,
                                 "p2.cycle" = TRUE,
                                 "equalcycle" = FALSE)
            } else {
            #^ if they dont either, return them both

                # # add p2s actions first time the cycle is played
                # # use match function to find first time sequence occurs
                # p2 <- cbind("from" = m[(2*match(rptseqs$from, m[player == 1, ]$action)[1]-1):
                #                        (2*match(rptseqs$from, m[player == 1, ]$action)[1]-1 + 2*nrow(rptseqs)-1),
                #                          .(player, action)][player == 2]$action,
                #             "to"   = m[(2*match(rptseqs$to, m[player == 1, ]$action)[1]-1):
                #                        (2*match(rptseqs$to, m[player == 1, ]$action)[1]-1 + 2*nrow(rptseqs)-1),
                #                          .(player, action)][player == 2]$action,
                #             "N"      = rptseqs$N,
                #             "player" = 2
                #             )
                idcycle <- cbind(rbind(rptseqs, rptseqs2),
                                 "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                 "p1.cycle" = TRUE,
                                 "p2.cycle" = FALSE,
                                 "equalcycle" = FALSE)
            }
        # sometimes, only a single sequence is contained which causes an error above. else if below to catch it
        # check to make sure that the other player also converged. p2 cycles are searched for below
        } else if(nrow(rptseqs) == 1 & m[player == 1 & action == rptseqs[order(-N)][1]$V1, .N] >= 80 &
                m[player == 2, .N, by = action][order(-N)][1,2] >= 80) {
                #^random play can lead to two distinct actions but players actually converged

                #if symmetric convergence
                if(m[player == 2, .N, by = action][order(-N)][1,1] == rptseqs$V1) {
                    idcycle <- cbind(rbind(cbind("from" = m[player == 1, .N, by = action][order(-N)][1,1],
                                                 "to"   = m[player == 1, .N, by = action][order(-N)][1,1],
                                                 "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                                 "player" = 1),
                                           cbind("from" = m[player == 2, .N, by = action][order(-N)][1,1],
                                                 "to"   = m[player == 2, .N, by = action][order(-N)][1,1],
                                                 "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                                 "player" = 2)),
                                     "eq_type"   = "sym1",
                                     "p1.cycle"  = NA,
                                     "p2.cycle"  = NA,
                                     "equalcycle" = NA)
                } else { #if asymmetric convergence
                    idcycle <- cbind(rbind(cbind("from" = m[player == 1, .N, by = action][order(-N)][1,1],
                                                 "to"   = m[player == 1, .N, by = action][order(-N)][1,1],
                                                 "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                                 "player" = 1),
                                           cbind("from" = m[player == 2, .N, by = action][order(-N)][1,1],
                                                 "to"   = m[player == 2, .N, by = action][order(-N)][1,1],
                                                 "N"    = m[player == 1, .N, by = action][order(-N)][1,2],
                                                 "player" = 2)),
                                     "eq_type"   = "asym1",
                                     "p1.cycle"  = NA,
                                     "p2.cycle"  = NA,
                                     "equalcycle" = NA)
                }
        } else {# if we have not identified any cycling, try player 2
            windowsizes <- seq(2, maxseq, by = 1)
            rptseqs <- data.table()
            for(i in windowsizes) {
                rptseqs <- rbind(rptseqs,
                                 na.omit(cbind(findRptSeqs(m[player == 2]$action, N = i), "seqlength" = i)),
                                 fill = TRUE)
                # end loop if identified sequences explain 90/100 actions
                if(rptseqs[seqlength == i & N>7, sum(N)] >= 80) {
                    break
                }
            }

            # exclude sequences that only show up rarely. see above
            rptseqs <- rptseqs[N>7, ]

            # check if it really is a cycle, i.e. sequences are repeated the same number of times. allow for small
            # margin because of random start
            if(rptseqs[seqlength == i, sum(N)] >= 80 & !(nrow(rptseqs) == 1) & rptseqs[, max(N) - min(N)] <= 2 & rptseqs[,.N] != 0) {
                rptseqs <- rptseqs[, .(V1, V2, N)]
                setnames(rptseqs, c("from", "to", "N"))
                rptseqs[, player := 2]
                # run findRptSeqs for p2 and see if the opposing players actions correspond
                rptseqs2 <- findRptSeqs(m[player == 1,]$action, N = 2)
                rptseqs2 <- rptseqs2[N>7,]
                setnames(rptseqs2, c("from", "to", "N"))
                rptseqs2[, player := 1]

                # rptseqs <- cbind(findRptSeqs(m[player == 2,]$action, N = nrow(rptseqs)),
                #                  "seqlength" = nrow(rptseqs))[1,][, .SD, .SDcols = !c("N","seqlength")]

                # in case only NA is reported. not sure why this case has come up
                if(isTRUE(unique(rowSums(is.na(rptseqs)) == ncol(rptseqs)))) {
                    idcycle <- cbind(rbind(cbind("from" = NA,
                                                 "to"   = NA,
                                                 "N"    = NA,
                                                 "player" = 1),
                                           cbind("from" = NA,
                                                 "to"   = NA,
                                                 "N"    = NA,
                                                 "player" = 2)),
                                     "eq_type"   = "other",
                                     "p1.cycle"  = NA,
                                     "p2.cycle"  = NA,
                                     "equalcycle" = NA)
                }

                # if they both cycle, then return them as idcycle
                if(nrow(rptseqs) == nrow(rptseqs2)) {
                    idcycle <- cbind(rbind(rptseqs2, rptseqs),
                                     "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                     "p1.cycle" = TRUE,
                                     "p2.cycle" = TRUE,
                                     "equalcycle" = TRUE)
                } else if(rptseqs[, sum(N)] >= 80 & !(nrow(rptseqs) == 1) & rptseqs[, max(N) - min(N)] <= 2) {
                    #^if they dont cycle equally, but p2 is cycling too
                    idcycle <- cbind(rbind(rptseqs2, rptseqs),
                                     "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                     "p1.cycle" = TRUE,
                                     "p2.cycle" = TRUE,
                                     "equalcycle" = FALSE)
                } else {# otherwise return both most frequent transitions
                    # p2 <- cbind("from" = m[(2*match(rptseqs$from, m[player == 2, ]$action)[1]-1):
                    #                            (2*match(rptseqs$from, m[player == 2, ]$action)[1]-1 + 2*nrow(rptseqs)-1),
                    #                        .(player, action)][player == 1]$action,
                    #             "to"   = m[(2*match(rptseqs$to, m[player == 2, ]$action)[1]-1):
                    #                            (2*match(rptseqs$to, m[player == 2, ]$action)[1]-1 + 2*nrow(rptseqs)-1),
                    #                        .(player, action)][player == 1]$action,
                    #             "N"      = rptseqs$N,
                    #             "player" = 1
                    # )
                    idcycle <- cbind(rbind(rptseqs2, rptseqs),
                                     "eq_type"  = paste0("cyc_", nrow(rptseqs)),
                                     "p1.cycle" = FALSE,
                                     "p2.cycle" = TRUE,
                                     "equalcycle" = FALSE)
                }
            } else {# if again no cycling is identified, return list of transitions and N, plus eq_type other
                rptseqs  <- findRptSeqs(m[player == 1,]$action, N = 2)
                setnames(rptseqs, c("from", "to", "N"))
                rptseqs[, player := 1]
                rptseqs2 <- findRptSeqs(m[player == 2,]$action, N = 2)
                setnames(rptseqs2, c("from", "to", "N"))
                rptseqs2[, player := 2]

                idcycle <- cbind(rbind(rptseqs, rptseqs2),
                                 "eq_type"  = "other",
                                 "p1.cycle" = FALSE,
                                 "p2.cycle" = FALSE,
                                 "equalcycle" = FALSE)
                }
        }
    }

    # for safety, so that code below runs and colnames are identical across cases
    idcycle <- data.table(idcycle)
    setnames(idcycle, c("from", "to", "N", "player", "eq_type", "p1.cycle", "p2.cycle", "equalcycle"))

    # find match and add to the returned output
    idcycle[, match := as.character(sub("_", "", str_extract(datadir, pattern = "p?[0-9]?e?[0-9]?e?[0-9]?[0-9]?p?[0-9]?e?[0-9]?[0-9]?e[0-9]{1,2}_")))]

    # find run number and add to returned output
    idcycle[, run := as.integer(str_extract(str_extract(datadir, pattern = "run_?[0-9]+"), pattern = "[0-9]+"))]

    # find phase number
    idcycle[, phase := as.integer(str_extract(str_extract(datadir, pattern = "phase[0-9]"), pattern = "[0-9]"))]

    # str(idcycle)
    return(idcycle)
}




#
#
#
#
# d <- fread(file.path(datadirs[[201]], paste("rewardsExtendedEndNLines.csv")))
# setnames(d, c("iteration","player","state_action_index","action_choice","explore_rate","reward"))
#
# # grab state action index data
# # player 1
# p <- fread(file.path(datadirs[[201]], paste("state_action_index_1.csv")))
# m <- merge(d[player == 1,], unique(p[,.(action_index, state, action)]),
#            by.x = c("state_action_index"),
#            by.y = c("action_index"))
# # player 2
# p <- fread(file.path(datadirs[[201]], paste("state_action_index_2.csv")))
# m <- rbind(m, merge(d[player ==2,], unique(p[,.(action_index, state, action)]),
#                     by.x = c("state_action_index"),
#                     by.y = c("action_index")))
# rm(p,d)
# test <- na.omit(as.data.table(#final row has NA element
#                 cbind(m[player == 1,]$action,
#                 data.table::shift(m[player == 1,]$action, 1, type = "lead"))))
# # test <- test[duplicated(test)]
# setnames(test, c("a1", "a1shift"))
# test[, n := .N, by = .(a1, a1shift)]
# splits <- ceiling(seq_along(m[player == 1,]$action) / 2)
# # use data.table shift to create overlapping windows
# w <- lapply(data.table::shift(m[player == 1,]$action, 0:1, type = "lead"), function(x) {
#             res <- split(m[player == 1,]$action, splits)
#             res[lengths(res) == 2]
# })
# w <- w[duplicated(w)]
# w <- t(as.data.frame(w))
# test <- unique(test)


#
# ## convert to graph, and get group membership ids
# library(igraph)
# graph <- graph_from_data_frame(test[n>1,.(a1, a1shift)])
# idgroups <- components(graph)$membership
# groups(components(graph))
