require(ssh)
require(stringr)

download_from_remote <- function(params, files){
  #' Connects to ip address 'params[["ip"]]'. 
  #' finds latest experiment in params[["res_path"]] 
  #' matching on user provided pattern[["exp_pattern"]]
  #' downloads all rewardsExtendedEndNLines files
  #' plus all other files for all run1s only
  #' into the local folder params[["output_path"]]
  #' and copies the other files into all run folders.
  
  # Start ssh session
  session <- ssh_connect(params[["ip"]])

  # List directories on remote in path_to_results, sort by date, keep if pattern match on
  # asymmetricLearners, pick the first one in the list, and extract file path
  res_dir <- str_extract(str_subset(capture.output(
                    result <- ssh_exec_wait(session, command = paste0("cd ", params[["res_path"]], " \n ls -lhat") )), 
                    pattern = params[["exp_pattern"]])[1],
                    pattern = paste0(exp_pattern, ".*"))
  
  # Use file path to create full path to results
  res_dir <- file.path(params[["res_path"]], res_dir)
  
  # List runs in results path on remote and create file paths to runs
  runs_to_get <- capture.output(result <- ssh_exec_wait(
      session, command = paste0("cd ", res_dir, " \n ls")))
  runs_to_get <- runs_to_get[!runs_to_get %in% c("stats", "stderr", "stdout", "script.sh")]
  runs_to_get <- apply(expand.grid(res_dir, runs_to_get), 1, function(x) paste0(x, collapse="/"))
  
  # Create the path to the runs folders locally
  output_paths <- file.path(params[["output_path"]], 
                           str_extract(res_dir, pattern = paste0(exp_pattern, ".*")), 
                           str_extract(runs_to_get, pattern = "p?[0-9]?e?[0-9]?e?[0-9]?p?[0-9]?e?[0-9]?e[0-9]{1,2}_?(phase[0-9]{1,2}_)?run.*"))
  
  # If local directory of latest experiment doesnt exist, create it
  if(!isTRUE(file.exists(file.path(params[["output_path"]], str_extract(res_dir, pattern = paste0(exp_pattern, ".*")))))){
      dir.create(file.path(params[["output_path"]], str_extract(res_dir, pattern = paste0(exp_pattern, ".*"))))
      
  } 
  
  # check for each run folder if it exists locally
  output_paths_new <- lapply(output_paths, function(x) {if(!isTRUE(dir.exists(x))){x}})
  
  # keep row ids of run folders that dont exist locally
  names(output_paths_new) <- seq_along(output_paths_new)
  output_paths_seq <- names(output_paths_new[lapply(output_paths_new, length) > 0])
  
  # check if any runs havent been downloaded, create local run folders and scp download
  if(length(output_paths_seq) > 0)
    {
      # create run folders locally
      lapply(output_paths_new[lapply(output_paths_new, length) > 0], dir.create)
      
      # generate list of remote file paths to be downloaded for reward.csv only
      files_to_get <- apply(expand.grid(runs_to_get, files[1]), 1, function(x) paste0(x, collapse="/"))
      
      # navigate to results folder with the latest entries, scp download rewardsExtendedEndNLines.csv files 
      # by iterating over remote and local full file path lists
      lapply(output_paths_seq, 
             function(x) {scp_download(session,
                                       files = files_to_get[as.numeric(x)],
                                       to    = output_paths_new[[as.numeric(x)]])
                 }
             )
      
      # repeat the scp dpwnload above for the parameters and state_action_index files for each *run1 only*
      # then copy from run1 to all other runs per match
      # specify file paths 
      files_to_get <- apply(expand.grid(str_subset(runs_to_get, "run_?1$"), 
                                        files[files != "rewardsExtendedEndNLines.csv"]), 
                                        1, function(x) paste0(x, collapse="/"))
      
      # repeat the output runs directory list of run1 by the number of files downloaded per run
      output_paths_run1 <- rep(str_subset(output_paths, "run_?1$"),
                               times = length(files[files != "rewardsExtendedEndNLines.csv"]))
      
      # download parameters.csv and state_action_index_*.csv files
      lapply(seq_along(files_to_get), 
             function(x) {scp_download(session,
                                       files = files_to_get[x],
                                       to    = output_paths_run1[x])
             }
      )
      
      # copy files to all other runs per pattern pxexpyey match
      files_to_copy <- str_subset(list.files(unique(output_paths_run1), full.names = TRUE), 
                                  "rewardsExtendedEndNLines", negate = TRUE)
      
      # but only to directories that had to be created before
      output_paths_new <- output_paths_new[lengths(output_paths_new) != 0]
      
      lapply(str_subset(output_paths_new, "run_?1$", negate = TRUE), function(x) {
          pattern = str_extract(x, "/p?[0-9]?e?[0-9]?e?[0-9]?p?[0-9]?e?[0-9]?e[0-9]{1,2}_?(phase[0-9]{1,2}_)?run.*")
          file.copy(from = str_subset(files_to_copy, pattern = sub("run_?[0-9]{1,3}", "", pattern)),
                    to = x)
      }
          
      )
  }
  
  # Disconnect session
  ssh_disconnect(session)
  
  datadirs <- unique(output_paths)
  
  return(datadirs)
}