#==============================================================================#
# PDP1: Fit Win-Stay Lose-Switch model
#==============================================================================#

# > FH, SD

# Script fits single model to observed data

# Stan files were adapted from the STAN files of Kreis. et al. 2022.
# Available online at https://osf.io/6xab2/
# Adapted to the current dataset with missing responses.

# 1. "M_wsls" : Win Stay Lose Switch model

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

wsls_model_path <- file.path(analysis_dir, 'M_wsls.stan')
rstan_options("auto_write" = T) 
wsls_model <- rstan::stan_model(wsls_model_path)

#--------------------# 
# Model fit
#--------------------#

tic()
Model1 <- 
  sampling(
    wsls_model,
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
list_of_draws <- extract(Model1)
print(names(list_of_draws))
HPs <- c("beta_mu_pr", "beta_sd")

# Rhat Chain Convergence Diagnostic
summ <- summary(Model1)
overview <- summ$summary
overviewDF <- as.data.frame(overview)
Rhat <- na.omit(overviewDF$Rhat)
print(max(Rhat))                                              #Rhat < 1.000546 

# Write Stan Fit Summary to xcel sheet for further use
col1 <- row.names(overviewDF)
overviewDF$parameters <- col1
write_xlsx(overviewDF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\Summary_Model1.xlsx", # adapt
           col_names = TRUE)

# Log-Likelihood
log_lik = loo::extract_log_lik(Model1, 'log_lik')

# WAIC
print(loo::waic(log_lik))                                     #WAIC = 3945.1 / 37.0

# LOOIC
print(loo::loo(log_lik))                                      #LOOIC = 3951.4 / 36.7

# Caterpillar Plots of Hyper-parameters draws
chainplot <- plot(Model1, plotfun = "trace", inc_warmup = FALSE, pars = HPs)
chainplot

#--------------------# 
# Saved Outputs for post-processing
#--------------------#

# .xl sheet of posterior draws
drawsDF <- as.data.frame(list_of_draws)
beta <- drawsDF$beta_mu_pr
beta_sd <- drawsDF$beta_sd
HyperDraws <- data.frame(beta, beta_sd)
write_xlsx(HyperDraws,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M1_draws.xlsx",  # adapt
           col_names = TRUE)

# Posterior Predictive Checks
PPC <- extract(Model1, pars = 'y_pred')
PPC_DF <- as.data.frame(PPC)
write_xlsx(PPC_DF,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\M1_PPC_raw.xlsx",  # adapt
           col_names = TRUE)

#==============================================================================#