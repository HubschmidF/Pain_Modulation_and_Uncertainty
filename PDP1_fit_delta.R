#==============================================================================#
# PDP1: "Delta" Q-learning model
#==============================================================================#

# > FH, SD

# Script fits single model to observed data

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Adapted to the current dataset with missing responses.

# 2. "M_RL_rw": RL Rescorla-Wagner model (Delta learning model)

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
# Model pre-compilation
#--------------------#

rw_model_path <- file.path(analysis_dir, 'M_RL_rw.stan')
rstan_options("auto_write" = T) 
rw_model <- rstan::stan_model(rw_model_path)

#--------------------# 
# Model fit
#--------------------#

tic()
Model2 <- 
  sampling(
    rw_model,
    data = stan_list,
    chains = 4,
    warmup = 15000,
    iter = 20000,
    cores = 4,
    refresh = 100
  )
toc()

#--------------------#
# Model diagnostics
#--------------------#

# Model Hyper-Parameters
list_of_draws <- extract(Model2)
print(names(list_of_draws))
HPs <- c("alpha_mu_pr", "beta_mu_pr",  "alpha_sd",  "beta_sd")

# Rhat Chain Convergence Diagnostic
summ <- summary(Model2)
overview <- summ$summary
overviewDF <- as.data.frame(overview)
Rhat <- na.omit(overviewDF$Rhat)
print(max(Rhat))                                              #Rhat < 1.003316

# Write Stan Fit Summary to xcel sheet for further use
col1 <- row.names(overviewDF)
overviewDF$parameters <- col1
write_xlsx(overviewDF,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\Summary_Model2.xlsx",  # adapt
           col_names = TRUE)

# Log-Likelihood
log_lik = loo::extract_log_lik(Model2, 'log_lik')

# WAIC
print(loo::waic(log_lik))                                     #WAIC = 3687.4 / 110.8

# LOOIC
print(loo::loo(log_lik))                                      #LOOIC = 3707.0 / 107.9

# Caterpillar Plots of Hyper-parameters draws
chainplot <- plot(Model2, plotfun = "trace", inc_warmup = FALSE, pars = HPs)
chainplot

#--------------------# 
# Saved Outputs for post-processing
#--------------------#

# .xl sheet of posterior draws
drawsDF <- as.data.frame(list_of_draws)
alpha <- drawsDF$alpha_mu_pr
alpha_sd <- drawsDF$alpha_sd
beta <- drawsDF$beta_mu_pr
beta_sd <- drawsDF$beta_sd
HyperDraws <- data.frame(alpha, alpha_sd, beta, beta_sd)
write_xlsx(HyperDraws,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\M2_draws.xlsx",   # adapt 
           col_names = TRUE)

# Posterior Predictive Checks
PPC <- extract(Model2, pars = 'y_pred')
PPC_DF <- as.data.frame(PPC)
write_xlsx(PPC_DF,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\M2_PPC_raw.xlsx", # adapt
           col_names = TRUE)

#==============================================================================#