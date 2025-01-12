#==============================================================================#
# PDP1: Model Comparisons based on LOO-ELPD
#==============================================================================#

# > FH

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Slightly adapted to the current dataset with missing responses.

# Script iteratively fits all the models considered and clears the workspace,
# to have all log-likelyhoods needed to compare models in the same session.

#==============================================================================#

#--------------------# 
# Path and packages
#--------------------#

# Path and software packages
suppressMessages(suppressWarnings(library(tictoc)))      
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(writexl)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(rstan)))
suppressMessages(suppressWarnings(library(loo)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()

# Data file
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data)
all_trials <- subset(DF, chosen %in% c("blue","red"))
test_trials <- subset(all_trials, type == "test")

#--------------------# 
# Data List passed onto Stan
#--------------------#

n_subj <- 
  length(unique(test_trials$subject))

n_trials_subjs <- 
  test_trials %>%
  group_by(subject) %>%
  dplyr::summarise(n=n())  %>%
  pull(n)

n_trials_max <- max(n_trials_subjs)

choices <- 
  test_trials %>%
  dplyr::select(subject, choice) %>%
  group_by(subject) %>%
  dplyr::mutate(trial_no= 1:n()) %>%
  ungroup()%>%
  pivot_wider(names_from = trial_no, values_from = choice) %>%
  dplyr::select(-subject) %>%
  replace(is.na(.), 0) %>%
  simplify2array()

rewards <- 
  test_trials %>%
  dplyr::select(subject, reward) %>%
  group_by(subject) %>%
  dplyr::mutate(trial_no= 1:n()) %>%
  ungroup()%>%
  pivot_wider(names_from = trial_no, values_from = reward) %>%
  dplyr::select(-subject) %>%
  replace(is.na(.), 0) %>%
  simplify2array()

stan_list <- 
  list(
    nS = n_subj,
    nT_max = n_trials_max,
    nT_subj = n_trials_subjs,
    choice = choices,
    reward = rewards
  )

#--------------------# 
# Iteratively re-fits each models to have the the log-lik matrix in a common R session
#--------------------#
model_names <- c("M_wsls.stan", "M_RL_rw.stan", "M_RL_rp.stan", "M_hmm0.stan", "M_hmm0_rp.stan")

# takes a while
tic()
for (n in 1:5) {
  
  # Getting the right model
  model_path <- file.path(analysis_dir, model_names[n])
  rstan_options("auto_write" = T) 
  model <- rstan::stan_model(model_path)
  
  # Fitting
  Model <- 
    sampling(
      model,
      data = stan_list,
      chains = 4,
      warmup = 15000,
      iter = 20000,
      cores = 4,
      refresh = 100
    )
  
  if (n == 1) {
    log_lik_wsls = loo::extract_log_lik(Model, 'log_lik')
    print("WSLS now fitted")
  }
    else if (n == 2) {
      log_lik_delta = loo::extract_log_lik(Model, 'log_lik')
      print("DELTA now fitted")
    }
    else if (n == 3) {
      log_lik_diffdelta = loo::extract_log_lik(Model, 'log_lik')
      print("DIFF DELTA now fitted")
    }
    else if (n == 4) {
      log_lik_hmm = loo::extract_log_lik(Model, 'log_lik')
      print("HMM now fitted")
    }
    else if (n == 5) {
      log_lik_diffhmm = loo::extract_log_lik(Model, 'log_lik')
      print("DIFF HMM now fitted")
    }
    else {
      print("Houston we have a problem !!!")
    }
}
toc()

#--------------------# 
# Comparing models based on the ELPD difference
#--------------------#

# Get the LOOIC to check if matches with fitting routine
# wsls
loo_wsls <- loo(log_lik_wsls)
print(loo_wsls)  # 3950.4 - 36.7
# delta
loo_delta <- loo(log_lik_delta)
print(loo_delta)   # 3706.8 - 108.6
# diff-delta
loo_diffdelta <- loo(log_lik_diffdelta)
print(loo_diffdelta)  # 3681.5 - 116.5
# hmm
loo_hmm <- loo(log_lik_hmm)
print(loo_hmm)   # 3678.9 - 106.4
# diff-hmm
loo_diffhmm <- loo(log_lik_diffhmm)
print(loo_diffhmm)   # 3646.5 - 128.4

# return the difference in expected log point wise predictive density
elpd_comp <- loo_compare(loo_wsls, loo_delta, loo_diffdelta, loo_hmm, loo_diffhmm)
print(elpd_comp)

# Sigma significance heuristic
elpd_df <- as.data.frame(elpd_comp)
elpd_df$sigma <- elpd_df$elpd_diff / elpd_df$se_diff

# Final model comparison results
print(elpd_df)

#==============================================================================#