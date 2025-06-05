#==============================================================================#
# PDP1: Differential Inferential Hidden Markov Model
#==============================================================================#

# > FH, SD

# Script fits single model to observed data

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Adapted to the current dataset with missing responses.

# 5. "M_hmm0_rp: Outcome sensitive Bayesian Hidden Markov Model

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
# Model pre-compilation
#--------------------#

HMMrp_model_path <- file.path(analysis_dir, 'M_hmm0_rp.stan')
rstan_options("auto_write" = T) 
HMMrp_model <- rstan::stan_model(HMMrp_model_path)

#--------------------# 
# Model fit
#--------------------#

tic()
Model5 <- 
  sampling(
    HMMrp_model,
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
list_of_draws <- extract(Model5)
print(names(list_of_draws))
HPs <- c("gamma_mu_pr", "c_mu_pr", "d_mu_pr",  "gamma_sd", "c_sd", "d_sd")  #Character vector of pars

# Rhat Chain Convergence Diagnostic
summ <- summary(Model5)
overview <- summ$summary
overviewDF <- as.data.frame(overview)
Rhat <- na.omit(overviewDF$Rhat)
print(max(Rhat))                                              #Rhat < 1.014407

# Write Stan Fit Summary to xcel sheet
col1 <- row.names(overviewDF)
overviewDF$parameters <- col1
write_xlsx(overviewDF,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\Summary_Model5.xlsx",  # adapt
           col_names = TRUE)

# Log-Likelihood
log_lik = loo::extract_log_lik(Model5, 'log_lik')

# WAIC
print(loo::waic(log_lik))                                     #WAIC =  3632.9 - 130.8

# LOOIC
print(loo::loo(log_lik))                                      #LOOIC = 3654.7 - 124.9

# Caterpillar Plots of Hyper-parameters draws
chainplot <- plot(Model5, plotfun = "trace", inc_warmup = FALSE, pars = HPs)
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
D <- drawsDF$d_mu_pr
D_sd <- drawsDF$d_sd
HyperDraws <- data.frame(GAMMA, GAMMA_sd, C, C_sd, D, D_sd)
write_xlsx(HyperDraws,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\M5_draws.xlsx",  # adapt
           col_names = TRUE)

# Posterior Predictive Checks
PPC <- extract(Model5, pars = 'y_pred')
PPC_DF <- as.data.frame(PPC)
write_xlsx(PPC_DF,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\M5_PPC_raw.xlsx",  # adapt
           col_names = TRUE)

#==============================================================================#
# As we perform parameter recovery analyses for this model, we additionally
# need the parameter estimates and simulated choices

rows_of_interest <- floor(runif(10, min=1, max=20000))

# Parameter values
param_values <- drawsDF[rows_of_interest,]
colA <- row.names(param_values)
param_values$sample <- colA
#
write_xlsx(param_values,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\generating_values_hmm.xlsx",  # adapt
           col_names = TRUE)

# Simulated choice
simulated_choices <- PPC_DF[rows_of_interest,]
colB <- row.names(simulated_choices)
simulated_choices$sample <- colB
#
write_xlsx(simulated_choices,
           "PATH\\TO\\YOUR\\ANALYSIS\\FOLDER\\simulated_choices_hmm.xlsx",  # adapt
           col_names = TRUE)

# Just in case
print(rows_of_interest)

# 14526 10094  1335  5409  5852 19724  5413  1407  3379  9206   # rows of the params used for simulation

#==============================================================================#