#==============================================================================#
# PDP1: Results of the modelling procedure
#==============================================================================#

# > FH

# Script used for posterior inference on model parameters, comparison of models
# based on ELPD to ELPD-SD ratio and plotting of model related data

#==============================================================================#

#--------------------# 
# Utility
#--------------------#

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(readxl))) 
suppressMessages(suppressWarnings(library(writexl)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(car)))
suppressMessages(suppressWarnings(library(ggplot2)))     
suppressMessages(suppressWarnings(library(ggpubr)))
suppressMessages(suppressWarnings(library(ggridges)))
suppressMessages(suppressWarnings(library(boot)))
suppressMessages(suppressWarnings(library(grid))) 
suppressMessages(suppressWarnings(library(HDInterval)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()

#--------------------# 
# Datasets
#--------------------#

# Task Data
data_file <- file.path(analysis_dir, 'Task_Data.csv')
data <- read.csv(data_file, header = T, sep = ";") 
DF <- as.data.frame(data)
all_trials <- subset(DF, chosen %in% c("blue","red", "black"))  

# Model Posterior draws
#M5 R-P HMM
data_fileM5 <- file.path(analysis_dir, 'M5_Draws.xlsx')
dataM5 <- read_xlsx(data_fileM5) 
DFM5 <- as.data.frame(dataM5)

# Model extracted quantities

# Data file
data_file2 <- file.path(analysis_dir, "Extracted_Quantities.csv")
data2 <- read.csv(data_file2, header = T, sep = ";")
DF2 <- as.data.frame(data2) 

outcomeWIN <- subset(DF2, outcome == "win")
outcomeLOSE <- subset(DF2, outcome == "lose")
#
outcomeWIN$end_mod <- outcomeWIN$VAS - outcomeWIN$ctrl_win 
outcomeLOSE$end_mod <- outcomeLOSE$VAS - outcomeLOSE$ctrl_lose
#
mod_modulation <- c(outcomeWIN$end_mod, outcomeLOSE$end_mod)
mod_subj_IDX <- c(outcomeWIN$subject, outcomeLOSE$subject)
mod_outcome <- c(outcomeWIN$outcome, outcomeLOSE$outcome)
mod_trial <- c(outcomeWIN$test_trial, outcomeLOSE$test_trial)
mod_since_rev <- c(outcomeWIN$since_rev, outcomeLOSE$since_rev)
mod_bloc <- c(outcomeWIN$bloc, outcomeLOSE$bloc)
mod_pe <- c(outcomeWIN$pe, outcomeLOSE$pe)
mod_state <- c(outcomeWIN$state_belief, outcomeLOSE$state_belief)
mod_entropy <- c(outcomeWIN$entropy, outcomeLOSE$entropy)
mod_surprise <- c(outcomeWIN$surprise, outcomeLOSE$surprise)
#
data_modulation <- data.frame(mod_subj_IDX,
                              mod_outcome,
                              mod_trial,
                              mod_modulation,
                              mod_since_rev,
                              mod_bloc,
                              mod_pe,
                              mod_state,
                              mod_entropy,
                              mod_surprise)
str(data_modulation)
data_modulation$mod_outcome <- as.factor(data_modulation$mod_outcome)
data_modulation$mod_outcome <- relevel(data_modulation$mod_outcome, ref = "win")
data_modulation$mod_bloc <- as.factor(data_modulation$mod_bloc)
data_modulation$mod_outcome <- relevel(data_modulation$mod_outcome, ref = 1)
str(data_modulation)

#--------------------# 
# Describtive plot, choices  # not used in final figure  
#--------------------#

allTEST <- subset(all_trials, type == "test")
subject_id <- allTEST$subject
choooooose <- allTEST$choice
outcoooome <- allTEST$reward
trial_id <- allTEST$test_trial
DFCH <- data.frame(subject_id,choooooose, outcoooome, trial_id)
DFCH$subject_id <- as.factor(DFCH$subject_id)

# Choices 

plZ <- ggplot(data = DFCH, aes(x = trial_id , y = subject_id)) +
              geom_tile(aes(fill = choooooose)) +
              scale_fill_gradient(low = "#4c9ae9", high = "#bf2ec2") +   # Exact RGB value of the buttons
              labs(y = 'Stimulus Choice', x = 'Test Trial n°') +
              xlim(0,100) +
              geom_vline(aes(xintercept = 21), color = 'black', linewidth = 1) +
              geom_vline(aes(xintercept = 38), color = 'black', linewidth = 1) + 
              geom_vline(aes(xintercept = 59), color = 'black', linewidth = 1) +
              geom_vline(aes(xintercept = 76), color = 'black', linewidth = 1) +
              geom_vline(aes(xintercept = 93), color = 'black', linewidth = 1) +
              theme_pubr() +
              theme(legend.position='none')
plZ

#--------------------# 
# Boxplot, relative model fit
#--------------------#

#
Family <- c("HMM", "HMM", "RL", "RL", "Logistic Regression")
Models <- c("HMM-Diff", "HMM", "Delta-Diff", "Delta", "WSLS")
ELPD_diff <- c(0, -16.238, -17.529, -30.179, -151.999)           # Data previously acquired in PDP1_Comparison_ELPD_models
SD_diff <- c(0, 18.292, 19.672, 27.713, 56.337)
Sigmas <- SD_diff/ELPD_diff
LowBounds <- ELPD_diff - SD_diff
HighBounds <- ELPD_diff + SD_diff
#
DF_fits <- data.frame(Family, Models, ELPD_diff, SD_diff, LowBounds, HighBounds)
DF_fits
# annotations
sigWSLS <- grobTree(textGrob("σ = -2.7", x=0.25,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=10)))   # SIGMA heuristic not kept in final version
sigHMM <- grobTree(textGrob("σ = -0.9", x=0.85,  y=0.57, hjust=0, gp=gpar(col="black", fontsize=10)))
sig_dd <- grobTree(textGrob("σ = -0.9", x=0.85,  y=0.37, hjust=0, gp=gpar(col="black", fontsize=10)))
sig_d <- grobTree(textGrob("σ = -1.1", x=0.8,  y=0.17, hjust=0, gp=gpar(col="black", fontsize=10)))
#
pl0 <- ggplot(data = DF_fits, aes(x=ELPD_diff, y = Models, fill= Family, color = Family)) +
              geom_point(size=5) +
              geom_errorbar(aes(xmin  = LowBounds,xmax  = HighBounds), width = 0, size  = 2) +
              scale_fill_manual(values = c("red4", "darkgrey", "chocolate3")) +
              scale_color_manual(values = c("red4", "darkgrey", "chocolate3")) +
              geom_vline(aes(xintercept=0), size=0.5, linetype="dashed") +
              labs(y="Models", x="ELPD Difference") +
              #annotation_custom(sigWSLS) +
              #annotation_custom(sigHMM) +
              #annotation_custom(sig_dd) +
              #annotation_custom(sig_d) +
              theme_pubr()
pl0

#--------------------# 
# Model Hyperparameters 
#--------------------#

# Outcome Sens- HMM

transition_GAMMA <- pnorm(DFM5$GAMMA)
emission_C <- pnorm(DFM5$C) * 0.5 + 0.5
emission_D <- pnorm(DFM5$D) * 0.5 + 0.5
mean(emission_C)
mean(emission_D)
mean(transition_GAMMA)

#Annotations
hoo <- grobTree(textGrob("From Model5:", x=0.7,  y=0.95, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
hoo2 <- grobTree(textGrob("Diff-HMM", x=0.7,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=10, fontface="italic")))

# Transition
mean(transition_GAMMA) #0.1490256
Gamma <- rep(1,n_draws)
Parameter <- as.factor(Gamma)
levels(Parameter) <- c('Gamma')
TransitionM7 <- data.frame(Gamma, Parameter)

pl1 <- ggplot(TransitionM7, aes(x = transition_GAMMA, color = Parameter, fill = Parameter)) +
              geom_density(alpha = 0.2) +
              geom_vline(aes(xintercept = 0.1490256), linetype = 'dashed', color = 'forestgreen') +
              labs(y = 'Density', x = 'Transition Probability') +
              scale_fill_manual(values=c('forestgreen')) +
              scale_color_manual(values=c('forestgreen')) +
              theme_classic() +
              annotation_custom(hoo) +
              annotation_custom(hoo2) +
              theme(legend.position='right')
pl1                                                 # NOT KEPT IN FINAL VERSION

# Emission
pars_1 <- rep(1,n_draws)
pars_2 <- rep(2,n_draws)
pars <- c(pars_1, pars_2)
Parameter <- as.factor(pars)
levels(Parameter) <- c('c (Relief)', 'd (Punishments)')

#DF used for plotting
emission <- c(emission_C, emission_D)
emissionM7 <- data.frame(Parameter, emission)
mu2 <- ddply(emissionM7, "Parameter", summarise, grp.mean=mean(emission))

pl2 <- ggplot(emissionM7, aes(x = emission, color = Parameter, fill = Parameter)) +
              geom_density(alpha = 0.2) +
              geom_vline(data=mu2, aes(xintercept=grp.mean, color=Parameter), linetype = 'dashed') +
              labs(y = 'Density', x = 'Emission Probabilities') +
              scale_fill_manual(values=c('blue', 'red')) +
              scale_color_manual(values=c('blue', 'red')) +
              theme_pubr() +
              #annotation_custom(hoo) +
              #annotation_custom(hoo2) +
              theme(legend.position='top')
pl2

# Difference density for emissions
emm_diff <- emission_C - emission_D
mean(emm_diff)
HDInterval::hdi(emm_diff, credMass = .95)

 #mean=0.092
 #HDI= lower -0.011 upper 0.216

DF_diff_hmm <- tibble::tibble(emm_diff)

pl3 <- ggplot(DF_diff_hmm, aes(x = emm_diff, y = 0, fill = stat(quantile))) + 
              geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) +
              geom_vline(xintercept = 0, color = 'red', size = 1) +
              labs(y = 'Difference Density', x = 'c (Relief) - d (Punishment)') +
              scale_fill_manual(values = alpha(c("transparent", "chocolate1", "transparent"), 0.5), guide = "none") +
              #annotation_custom(hoo) +
              #annotation_custom(hoo2) +
              theme_pubr()
pl3

#==============================================================================#