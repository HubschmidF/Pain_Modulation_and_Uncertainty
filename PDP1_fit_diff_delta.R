#==============================================================================#
# PDP1: Differential "Delta" Q-learning model
#==============================================================================#

# > FH, SD

# Script fits single model to observed data

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Adapted to the current dataset with missing responses.

# 3. "M_RL_rp": RL differential model for reward and punishments

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

rp_model_path <- file.path(analysis_dir, 'M_RL_rp.stan')
rstan_options("auto_write" = T) 
rp_model <- rstan::stan_model(rp_model_path)

#--------------------# 
# Model fit
#--------------------#

tic()
Model3 <- 
  sampling(
    rp_model,
    data = stan_list,
    chains = 4,
    warmup = 150,
    iter = 200,
    cores = 4,
    refresh = 100
  )
toc()

#--------------------# 
# Model Diagnostics
#--------------------#

# Model Hyper-Parameters
list_of_draws <- extract(Model3)
print(names(list_of_draws))
HPs <- c("alpha_pos_mu_pr", "alpha_neg_mu_pr",  "beta_mu_pr", "alpha_pos_sd", "alpha_neg_sd",  "beta_sd")  #Character vector of pars

# Rhat Chain Convergence Diagnostic
summ <- summary(Model3)
overview <- summ$summary
overviewDF <- as.data.frame(overview)
Rhat <- na.omit(overviewDF$Rhat)
print(max(Rhat))                                              #Rhat < 1.001378

# Write Stan Fit Summary to xcel sheet
col1 <- row.names(overviewDF)
overviewDF$parameters <- col1
write_xlsx(overviewDF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\Summary_Model3.xlsx",  # adapt
           col_names = TRUE)

# Log-Likelihood
log_lik = loo::extract_log_lik(Model3, 'log_lik')

# WAIC
print(loo::waic(log_lik))                                     #WAIC = 3659.2 / 120.7

# LOOIC
print(loo::loo(log_lik))                                      #LOOIC = 3679.3 / 117.5

# Caterpillar Plots of Hyper-parameters draws
chainplot <- plot(Model3, plotfun = "trace", inc_warmup = FALSE, pars = HPs)
chainplot

#--------------------# 
# Saved Outputs for post-processing
#--------------------#

# .xl sheet of posterior draws
drawsDF <- as.data.frame(list_of_draws)
alpha_pos <- drawsDF$alpha_pos_mu_pr
alpha_pos_sd <- drawsDF$alpha_pos_sd
alpha_neg <- drawsDF$alpha_neg_mu_pr
alpha_neg_sd <- drawsDF$alpha_neg_sd
beta <- drawsDF$beta_mu_pr
beta_sd <- drawsDF$beta_sd
HyperDraws <- data.frame(alpha_pos, alpha_pos_sd, alpha_neg, alpha_neg_sd, beta, beta_sd)
write_xlsx(HyperDraws,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M3_draws.xlsx",  # adapt
           col_names = TRUE)

# Posterior Predictive Checks
PPC <- extract(Model3, pars = 'y_pred')
PPC_DF <- as.data.frame(PPC)
write_xlsx(PPC_DF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M3_PPC_raw.xlsx",  # adapt
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
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\generating_values_delta.xlsx",  # adapt
           col_names = TRUE)

# Simulated choice
simulated_choices <- PPC_DF[rows_of_interest,]
colB <- row.names(simulated_choices)
simulated_choices$sample <- colB
#
write_xlsx(simulated_choices,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\simulated_choices_delta.xlsx",  # adapt
           col_names = TRUE)

# Just in case
print(rows_of_interest)

# 4249  4541 14868  7505  8178  4855   616 12963   904  1761  # Parameters used for simulation

#==============================================================================#