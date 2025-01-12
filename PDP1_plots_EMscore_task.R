#==============================================================================#
# PDP1: Plots task and Extent of modulation
#==============================================================================#

# > FH

# Script used for plotting some of the subplots of the extent of modulation
# figure.

#==============================================================================#

#--------------------# 
# Path and packages
#--------------------#

# Path and software packages
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(ggpubr)))
suppressMessages(suppressWarnings(library(ggridges)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# Data file
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data)

#--------------------#
# Curation
#--------------------# 

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
outcomeWIN <- subset(test_trials, outcome == "win")
outcomeLOSE <- subset(test_trials, outcome == "lose")

# EM score
outcomeWIN$end_mod <- outcomeWIN$VAS - outcomeWIN$ctrl_win 
outcomeLOSE$end_mod <- outcomeLOSE$VAS - outcomeLOSE$ctrl_lose

# Variables of interest for plotting
mod_modulation <- c(outcomeWIN$end_mod, outcomeLOSE$end_mod)
mod_subj_IDX <- c(outcomeWIN$subject, outcomeLOSE$subject)
mod_outcome <- c(outcomeWIN$outcome, outcomeLOSE$outcome)
mod_trial <- c(outcomeWIN$test_trial, outcomeLOSE$test_trial)
mod_since_rev <- c(outcomeWIN$since_rev, outcomeLOSE$since_rev)
mod_since_patch <- c(outcomeWIN$since_patch, outcomeLOSE$since_patch)

# New DF with only collumnds of interest and EM score
data_modulation <- data.frame(mod_subj_IDX,mod_outcome,mod_trial, mod_modulation, mod_since_rev, mod_since_patch)
str(data_modulation)
data_modulation$mod_outcome <- as.factor(data_modulation$mod_outcome)
data_modulation$mod_outcome <- relevel(data_modulation$mod_outcome, ref = "win")
str(data_modulation)

#--------------------# 
# Plots
#--------------------# 

# Better name
data_modulation$Outcome <- data_modulation$mod_outcome
levels(data_modulation$Outcome) <- c("Relief", "Punishment")

# Scatterplot, Extent of modulation and time since contingency change
inh <- grobTree(textGrob("Pain Inhibition", x=0.02,  y=0.10, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
fac <- grobTree(textGrob("Pain Facilitation", x=0.02,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
sig1 <- grobTree(textGrob("***", x=0.5,  y=0.35, hjust=0, gp=gpar(col="blue", fontsize=35, fontface="bold")))
sig2 <- grobTree(textGrob("***", x=0.5,  y=0.6, hjust=0, gp=gpar(col="red", fontsize=35, fontface="bold")))

pl1 <- ggplot(data_modulation, aes(x = mod_since_rev, y = mod_modulation, color = Outcome)) +
              geom_point(alpha = 0.1) +
              geom_smooth(method = lm) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Trials Within Reversal Bloc') +
              scale_color_manual(values = c('blue', 'red')) +
              ylim(-50, 50) +
              annotation_custom(inh) +
              annotation_custom(fac) +
              #annotation_custom(sig1) +
              theme_pubr() +
              theme(legend.position='none')
pl1

# Scatterplot, Extent of modulation and time since patch (sensitization)
pl2 <- ggplot(data_modulation, aes(x = mod_since_patch, y = mod_modulation, color = Outcome)) +
              geom_point(alpha = 0.1) +
              geom_smooth(method = lm) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Trials Within Skin Patch') +
              scale_color_manual(values = c('blue', 'red')) +
              ylim(-50, 50) +
              annotation_custom(inh) +
              annotation_custom(fac) +
              #annotation_custom(sig2) +
              theme_pubr() +
              theme(legend.position='none')
pl2

# Density plot, Extent of modulation score in trials with relief and punishments
mu1 <- ddply(data_modulation, "Outcome", summarise, grp.mean=mean(mod_modulation))

INH <- grobTree(textGrob("< Pain Inhibition", x=0.1,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=12, fontface="bold")))
FAC <- grobTree(textGrob("Pain Facilitation >", x=0.75,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=12, fontface="bold")))

pl3 <- ggplot(data_modulation, aes(x = mod_modulation, color = Outcome, fill = Outcome)) +
              geom_density(alpha = 0.2) +
              labs(y = 'Density', x = 'Extent of Modulation (a.u)') +
              geom_vline(data=mu1, aes(xintercept=grp.mean, color=Outcome), linetype = "dashed") +
              geom_vline(aes(xintercept = 0)) +
              scale_fill_manual(values=c('blue', 'red')) +
              scale_color_manual(values=c('blue', 'red')) +
              xlim(-75, 75) +
              annotation_custom(INH) +
              annotation_custom(FAC) +
              theme_pubr() +
              theme(legend.position='top')
pl3

#==============================================================================#