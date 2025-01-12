#==============================================================================#
# PDP1: Mixed effects analysis: task properties on extent of modulation
#==============================================================================#

# > FH

# Script to fit the LMMs assessing the effect of objective task properties
# on the trial-by-trial extent of modulation

#==============================================================================#

# ------------------ # 
# Path, Packages and data curation
# ------------------ #

# Path and software packages
suppressMessages(suppressWarnings(library(lme4)))
suppressMessages(suppressWarnings(library(effects))) #visualisation only
suppressMessages(suppressWarnings(library(lmerTest)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(grid)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# Data file
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data)
# Function to compare model evidence as suggested by E-J, Wagenmakers (2007) 
# (Not kept in final version ( as it says the same than the BIC))
bic_2_bayes <- function(null_BIC, alternative_BIC) {
                        model_bf <- exp((null_BIC - alternative_BIC) / 2)
                        return(model_bf) 
}

# ------------------ # 
# Extent of Modulation (EM_score) datasets used for analysis
# ------------------ #

# Removal of missed responses (no response in the 2s 2AFC)
none <- subset(DF, chosen == "none")
nrow(none)
trials <- subset(DF, chosen %in% c("red","blue", "black"))

# Estimation of individual "baseline perception" from control trials with outcomes
# (no neutral outcomes trials accounted)
ctrl <- subset(trials, type == "ctrl")                         
ctrlWIN <- subset(ctrl, outcome == "win")    #trials with stim decrease (as logged)
ctrlLOSE <- subset(ctrl, outcome == "lose")  #trials with stim increase (as logged)
ctrlWINagg  <- aggregate(x = ctrlWIN[, colnames(ctrlWIN) != "subject"],
                         by = list(ctrlWIN$subject),
                         FUN = mean)
ctrlLOSEagg <- aggregate(x = ctrlLOSE[, colnames(ctrlLOSE) != "subject"],
                         by = list(ctrlLOSE$subject),
                         FUN = mean)
controlWIN <- ctrlWINagg$VAS
controlLOSE <- ctrlLOSEagg$VAS

# For 30 subjects completing 204 trials and added in original DF
trials_no <- rep(204, times = 30)
ctrl_win <- rep(controlWIN, times = trials_no)
ctrl_lose <- rep(controlLOSE, times = trials_no)
DF$ctrl_win <- ctrl_win                                      
DF$ctrl_lose <- ctrl_lose

# Re-removing missed responses in the updated DF
all_trials <- subset(DF, chosen %in% c("red","blue", "black"))

# Keeping only test trials (that have a pain modulatory effect induced)
test_trials <- subset(all_trials, type == "test")   

# Spointing into two useable datasets for the mixed effect analysis:
relief_data <- subset(test_trials, outcome == "win")       #pool of trials with reward/relief
punishment_data <- subset(test_trials, outcome == "lose")  #pool of trials with punishment
relief_data$EM_score <- relief_data$VAS - relief_data$ctrl_win 
punishment_data$EM_score <- punishment_data$VAS - punishment_data$ctrl_lose

# ------------------ # 
# Mixed Effects: Task properties on Extent of modulation in RELIEF TRIALS
# ------------------ #

# Fitting routine
relief.M0 <- lmer(EM_score ~ 1 + (1 | subject), data = relief_data, REML = FALSE) #null model
relief.M1 <- lmer(EM_score ~ since_patch + (1 | subject), data = relief_data, REML = FALSE) #sensitization model
relief.M2 <- lmer(EM_score ~ since_rev + (1 | subject), data = relief_data, REML = FALSE) #time in contingency model
relief.M3 <- lmer(EM_score ~ since_patch + since_rev + (1 | subject), data = relief_data, REML = FALSE) #full model
# Model Comparisons
print(fit_relief.M0 <- BIC(relief.M0))
print(fit_relief.M1 <- BIC(relief.M1))
print(fit_relief.M2 <- BIC(relief.M2))
print(fit_relief.M3 <- BIC(relief.M3))
# BFmodel
print(rel_BF_m1 <- bic_2_bayes(fit_relief.M0, fit_relief.M1))
print(rel_BF_m2 <- bic_2_bayes(fit_relief.M0, fit_relief.M2))
print(rel_BF_m3 <- bic_2_bayes(fit_relief.M0, fit_relief.M3))
# Best Fitting model from the relief trial pool
relief_model <- lmer(EM_score ~ since_rev + (1 | subject), data = relief_data, REML = FALSE)
summary(relief_model)
confint(relief_model)

# ------------------ # 
# Mixed Effects: Task properties on Extent of modulation in PUNISHMENT TRIALS
# ------------------ #

# Fitting routine
punishment.M0 <- lmer(EM_score ~ 1 + (1 | subject), data = punishment_data, REML = FALSE) #null model
punishment.M1 <- lmer(EM_score ~ since_patch + (1 | subject), data = punishment_data, REML = FALSE) #sensitization model
punishment.M2 <- lmer(EM_score ~ since_rev + (1 | subject), data = punishment_data, REML = FALSE) #time in contingency model
punishment.M3 <- lmer(EM_score ~ since_patch + since_rev + (1 | subject), data = punishment_data, REML = FALSE) #full model
# Model Comparisons
print(fit_punishment.M0 <- BIC(punishment.M0))
print(fit_punishment.M1 <- BIC(punishment.M1))
print(fit_punishment.M2 <- BIC(punishment.M2))
print(fit_punishment.M3 <- BIC(punishment.M3))
# BFmodel
print(pun_BF_m1 <- bic_2_bayes(fit_punishment.M0, fit_punishment.M1))
print(pun_BF_m2 <- bic_2_bayes(fit_punishment.M0, fit_punishment.M2))
print(pun_BF_m3 <- bic_2_bayes(fit_punishment.M0, fit_punishment.M3))
# Best Fitting model from the punishment trial pool
punishment_model <- lmer(EM_score ~ since_patch + (1 | subject), data = punishment_data, REML = FALSE)
summary(punishment_model)
confint(punishment_model)

# ------------------ # 
# Plots
# ------------------ #

# Manual annotations
inh <- grobTree(textGrob("Pain Inhibition", x=0.02,  y=0.10, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
fac <- grobTree(textGrob("Pain Facilitation", x=0.02,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
sig1 <- grobTree(textGrob("***", x=0.5,  y=0.35, hjust=0, gp=gpar(col="blue", fontsize=35, fontface="bold")))
sig2 <- grobTree(textGrob("***", x=0.5,  y=0.6, hjust=0, gp=gpar(col="red", fontsize=35, fontface="bold")))

# Subjects and marginal intercepts for best fitting punishment model
pl1 <- ggplot(punishment_data, aes(x=since_patch, y=EM_score, color = outcome)) +
              #geom_point(alpha = 0.1) +
              geom_line(aes(y=predict(punishment_model, re.form = NA), group=subject), size = 3) +
              geom_line(aes(y=predict(punishment_model, re.form = NULL), group=subject), size = 0.7, alpha = 0.3) +
              scale_color_manual(values = c("red")) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Trials Within Skin Patch') +
              theme_pubr() +
              #annotation_custom(inh) +
              #annotation_custom(fac) +
              annotation_custom(sig2) +
              theme(legend.position='none')
pl1

# Subjects and marginal intercepts for best fitting relief model
pl2 <- ggplot(relief_data, aes(x=since_rev, y=EM_score, color = outcome)) +
              #geom_point(alpha = 0.1) +
              geom_line(aes(y=predict(relief_model, re.form = NA), group=subject), size = 3) +
              geom_line(aes(y=predict(relief_model, re.form = NULL), group=subject), size = 0.7, alpha = 0.3) +
              scale_color_manual(values = c("blue")) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Trials Within Reversal Bloc') +
              theme_pubr() +
              #annotation_custom(inh) +
              #annotation_custom(fac) +
              annotation_custom(sig1) +
              theme(legend.position='none')
pl2

#==============================================================================#