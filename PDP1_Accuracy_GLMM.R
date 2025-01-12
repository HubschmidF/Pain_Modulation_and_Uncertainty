#==============================================================================#
# PDP1: GLM accuracy
#==============================================================================#

# > FH

# > Script using for fitting the GLM assessing if accuracy does increase as 
# as function of time spent with a similar reversal bloc.

#==============================================================================#

#--------------------# 
# Path, Packages and data curation
#--------------------#

# Path and software packages
suppressMessages(suppressWarnings(library(lme4)))
suppressMessages(suppressWarnings(library(effects)))
suppressMessages(suppressWarnings(library(lmerTest)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()

# Data file
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data)

# to get only test trials where a choice has been performed
# i.e. only those ones could be "relatively" accurate
trials_with_choices <- subset(DF, chosen %in% c("red","blue"))

#--------------------# 
# GLM fit
#--------------------#

# Fit
MLR_accuracy <- glmer(accuracy ~ since_rev + (1 | subject),
                      data = trials_with_choices,
                      family = binomial(link='logit'))
# Results
summary(MLR_accuracy)
plot(allEffects(MLR_accuracy))   # Visualization only
# 95% confidence intervals
confint(MLR_accuracy)       

#==============================================================================#