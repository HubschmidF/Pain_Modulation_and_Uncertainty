#==============================================================================#
# PDP1: Inferential Hidden Markov Model
#==============================================================================#

# > FH, SD

# Script fits single model to observed data

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Adapted to the current dataset with missing responses.

# 4. "M_hmm0: Bayesian Hidden Markov Model

#==============================================================================#

#--------------------# 
# Path and packages
#--------------------#

# Path and software packages
analysis_dir <- getwd()
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

HMM_model_path <- file.path(analysis_dir, 'M_hmm0.stan')
rstan_options("auto_write" = T) 
HMM_model <- rstan::stan_model(HMM_model_path)

#--------------------# 
# Model fit
#--------------------#

tic()
Model4 <- 
  sampling(
    HMM_model,
    data = stan_list,
    chains = 4,
    warmup = 15000,
    iter = 20000,
    cores = 4,
    refresh = 100
  )
toc()

#--------------------# 
# Model Diagnostics
#--------------------#

# Model Hyper-Parameters
list_of_draws <- extract(Model4)
print(names(list_of_draws))
HPs <- c("gamma_mu_pr", "c_mu_pr",  "gamma_sd", "c_sd")  #Character vector of pars

# Rhat Chain Convergence Diagnostic
summ <- summary(Model4)
overview <- summ$summary
overviewDF <- as.data.frame(overview)
Rhat <- na.omit(overviewDF$Rhat)
print(max(Rhat))                                              #Rhat < 1.057547

# Write Stan Fit Summary to xcel sheet
col1 <- row.names(overviewDF)
overviewDF$parameters <- col1
write_xlsx(overviewDF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\Summary_Model4.xlsx",  # adapt
           col_names = TRUE)

# Log-Likelihood
log_lik = loo::extract_log_lik(Model4, 'log_lik')

# WAIC
print(loo::waic(log_lik))                                     #WAIC = 3663.6 / 109.6

# LOOIC
print(loo::loo(log_lik))                                      #LOOIC = 3681.5 / 105.8

# Caterpillar Plots of Hyper-parameters draws
chainplot <- plot(Model4, plotfun = "trace", inc_warmup = FALSE, pars = HPs)
chainplot

#--------------------# 
# Saved Outputs for post-processing
#--------------------#

# .xl sheet of posterior draws
drawsDF <- as.data.frame(list_of_draws)
GAMMA <- drawsDF$gamma_mu_pr
GAMMA_sd <- drawsDF$gamma_sd
C <- drawsDF$c_mu_pr
C_sd <- drawsDF$c_sd
HyperDraws <- data.frame(GAMMA, GAMMA_sd, C, C_sd)
write_xlsx(HyperDraws,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M4_draws.xlsx",  # adapt
           col_names = TRUE)

# Posterior Predictive Checks
PPC <- extract(Model4, pars = 'y_pred')
PPC_DF <- as.data.frame(PPC)
write_xlsx(PPC_DF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M4_PPC_raw.xlsx",  # adapt
           col_names = TRUE)

#==============================================================================#