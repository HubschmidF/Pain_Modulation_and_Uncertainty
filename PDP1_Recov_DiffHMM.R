#==============================================================================#
# PDP1: Recovery´analyses for diff_hmm
#==============================================================================#

# > FH

# Scripts takes the forward sampled choices (10 simulated datasets) and
# iteratively refits the data to the same model specs used for fitting and then 
# saves recovered parameter values at the hyper-parameter level for diff HMM

#==============================================================================#

#--------------------# 
# Path and packages
#--------------------#

suppressMessages(suppressWarnings(library(tictoc)))      
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(writexl)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(rstan)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# to cope with the stochastic nature of MCMC chains
set.seed(23081997)

#--------------------# 
# Original data
#--------------------#

# to get the sequence of outcomes
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data)
all_trials <- subset(DF, chosen %in% c("blue","red"))
empirical_data <- subset(all_trials, type == "test")

# simulated data from Diff-Delta
hmm_file <- file.path(analysis_dir, 'simulated_choices_hmm.xlsx')
hmm_data <- read_xlsx(hmm_file)
hmm_DF <- as.data.frame(hmm_data)

# harmonics at which data from same participant are separated
sample_size <- 30 # given how the output is organized two simulated choices from the same subject is separated by the number of subjects
harmonic <- vector()
for (k in 0:100) {
  K <- k*sample_size
  print(K)   # just to be sure 
  harmonic <- c(harmonic,K)
}

#--------------------# 
# Simulated choices sequences diff-delta
#--------------------#

tic()
# loops over simulations
for (simulation in 1:10) {
  hmm_sequence <- vector()
  
  # loops over participants 
  for (participant in 1:30) {
    
    # loops over trials
    for (trial in 1:100) {
      choice <- hmm_DF[simulation,c(participant + harmonic[trial])]
      hmm_sequence <- c(hmm_sequence, choice)
      hmm_sequence <- subset(hmm_sequence, hmm_sequence %in% c("1","2"))
      
    } # ends at trial 100
  } # ends at participant 30
  
  empirical_data <- data.frame(empirical_data, hmm_sequence) # appends simulated sequence
  
} # ends at simulation 10
toc()

#--------------------# 
# Template of the list passed onto stan
#--------------------#

n_subj <- 
  length(unique(empirical_data$subject))

n_trials_subjs <- 
  empirical_data %>%
  group_by(subject) %>%
  dplyr::summarise(n=n())  %>%
  pull(n)

n_trials_max <- max(n_trials_subjs)

rewards <- 
  empirical_data %>%
  dplyr::select(subject, reward) %>%
  group_by(subject) %>%
  dplyr::mutate(trial_no= 1:n()) %>%
  ungroup()%>%
  pivot_wider(names_from = trial_no, values_from = reward) %>%
  dplyr::select(-subject) %>%
  replace(is.na(.), 0) %>%
  simplify2array()

#--------------------# 
# Pre-compile the model to gain time
#--------------------#

HMMrp_model_path <- file.path(analysis_dir, 'M_hmm0_rp.stan')
rstan_options("auto_write" = T) 
HMMrp_model <- rstan::stan_model(HMMrp_model_path)

#--------------------# 
# Recovery fitting
#--------------------#

# creates dataframes that can append the output of the recovery fit
posterior_draws <- 1:20000                                         
group_recovery <- as.data.frame(posterior_draws)

# iteratively fits the simulated data
tic()   # will take ages, go get a coffee
for (recovery in 1:10) {
  
  # simulated choices are on collumns 21 to 30
  choice_collumn <- recovery + 20
  choice <- as.integer(empirical_data[,choice_collumn])  
  empirical_data$choice <- choice
  
  # find corresponding choice collumn
  choices <- 
    empirical_data %>%
    dplyr::select(subject, choice) %>%
    group_by(subject) %>%
    dplyr::mutate(trial_no= 1:n()) %>%
    ungroup()%>%
    pivot_wider(names_from = trial_no, values_from = choice) %>%
    dplyr::select(-subject) %>%
    replace(is.na(.), 0) %>%
    simplify2array()
  
  # final list passed onto stan
  stan_list <- 
    list(
      nS = n_subj,
      nT_max = n_trials_max,
      nT_subj = n_trials_subjs,
      choice = choices,
      reward = rewards
    )
  
  # fits that one recovery simulation
  model_fit <- 
    sampling(
      HMMrp_model,
      data = stan_list,
      chains = 4,
      warmup = 15000,   
      iter = 20000,     
      cores = 4,
      refresh = 100
    )
  
  # mean posterior parameter estimates
  summ <- summary(model_fit)
  overview <- summ$summary
  overviewDF <- as.data.frame(overview)
  col1 <- row.names(overviewDF)
  overviewDF$parameters <- col1
  
  # at the group level we need the posterior draws of hyper parameters
  list_of_draws <- extract(model_fit)
  drawsDF <- as.data.frame(list_of_draws)
  GAMMA <- drawsDF$gamma_mu_pr
  GAMMA_sd <- drawsDF$gamma_sd
  C <- drawsDF$c_mu_pr
  C_sd <- drawsDF$c_sd
  D <- drawsDF$d_mu_pr
  D_sd <- drawsDF$d_sd
  
  # saves the information for group level recovery iteratively
  group_recovery <- data.frame(group_recovery,
                               GAMMA,
                               GAMMA_sd,
                               C,
                               C_sd,
                               D,
                               D_sd)
  
  # clears the environment of heavy objects to not overload working memory when running
  rm("summ")
  rm("model_fit")
  rm("list_of_draws")
  rm("drawsDF")
  
  # progress update
  where_sim <- recovery *10
  where_string <- as.character(where_sim)
  progress_string <- "% are now done!"
  now_update <- paste(where_string, progress_string)
  print(now_update)
  
}
toc()     

#--------------------# 
# Save the output of the recovery fit for further analysis
#--------------------#

# hyper
write_xlsx(group_recovery,
           "C:\\Users\\Hubsc\\OneDrive\\Desktop\\PDPain_Study1\\Final_Analysis\\Results\\4_model_results\\HMM_group_recovery.xlsx",   # adapt
           col_names = TRUE)

#==============================================================================#