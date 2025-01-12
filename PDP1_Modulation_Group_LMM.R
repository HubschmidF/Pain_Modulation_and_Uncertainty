#==============================================================================#
# PDP1: Endogenous pain modulation at group level (LMM)
#==============================================================================#

# > FH 

# Script for assessment of pain modulatory effects at the group level +
# Plotting of modulation at group level.

#==============================================================================#

#--------------------# 
# Path, Packages and data
#--------------------#

# Path and software packages
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(lme4)))
suppressMessages(suppressWarnings(library(effects)))
suppressMessages(suppressWarnings(library(lmerTest)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ggpubr)))
suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(multcomp)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# Data file
data_file <- file.path(analysis_dir, "Task_Data.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data) 

#--------------------# 
# Data Curation
#--------------------#

# Removal of trials with no responses (logged as = 'none')
all_trials <- subset(DF, chosen %in% c("red","blue", "black"))
# Removal of neutral trials
all_trials$outcome <- as.factor(all_trials$outcome)
levels(all_trials$outcome) <- c("increase", "neutral", "decrease")
trials_no_neutral <- subset(all_trials, type %in% c("test", "ctrl"))
# Removal of trials with neutral (stable) outcomes
trials_no_neutral <- subset(trials_no_neutral, outcome %in% c("increase","decrease"))

#--------------------# 
# LMM fit
#--------------------#

Group_LMM_VAS <- lmer(VAS ~ type  * outcome + (1 | subject), data = trials_no_neutral)
summary(Group_LMM_VAS)
plot(allEffects(Group_LMM_VAS))  # For rapid visualization only
confint(Group_LMM_VAS)

#--------------------# 
# Tukey corrected contrast analyses
#--------------------#

trials_no_neutral$tall<-with(trials_no_neutral,interaction(type,outcome,sep = "x"))
modelall<-lmer(VAS ~ tall+ (1 | subject),data = trials_no_neutral)
contrasts_model<-glht(modelall, linfct = mcp(tall = "Tukey"))
summary(contrasts_model)
allEffects(modelall)

# Group means
means <- ddply(trials_no_neutral, c("type", "outcome"), summarise, grp.means=mean(VAS))
print(means)

# Group standart deviations
sds <- ddply(trials_no_neutral, c("type", "outcome"), summarise, grp.sd=sd(VAS))
print(sds)

#--------------------#
# Plot Pain modulation test vs control trials
#--------------------#

# Renaming of variables for plotting
levels(all_trials$outcome) <- c("Increase", "neutral", "Decrease")
no_neutral <- subset(all_trials, type %in% c("ctrl","test"))        
no_neutral <- subset(all_trials, outcome %in% c("Increase","Decrease"))
no_neutral$Type <- no_neutral$type
no_neutral$Type <- as.factor(no_neutral$Type)
levels(no_neutral$Type) <- c("Control", "Test")
# Retrieve means to manually add to plot
mu1 <- ddply(no_neutral, c("Type", "outcome"), summarise, grp.median=median(VAS))
print(mu1)

#  Type    outcome       grp.median
#1 Control Increase      153
#2 Control Decrease      64
#3    Test Increase      157
#4    Test Decrease      58

# Annotations
inh <- grobTree(textGrob("< Pain Inhibition", x=0.17,  y=0.5, hjust=0, gp=gpar(col="blue", fontsize=10, fontface = "bold")))
fac <- grobTree(textGrob("Pain Facilitation >", x=0.79,  y=0.5, hjust=0, gp=gpar(col="red", fontsize=10, fontface = "bold")))
# Significance annotations (retrieved from the contrasts analyses)
sig1 <- grobTree(textGrob("***", x=0.26,  y=0.96, hjust=0, gp=gpar(col="blue", fontsize=20, fontface = "bold")))
sig2 <- grobTree(textGrob("*", x=0.77,  y=0.96, hjust=0, gp=gpar(col="red", fontsize=20, fontface = "bold")))

# Plot
plot_GroupModulation <- ggplot(no_neutral, aes(x = VAS, y=outcome, fill=Type)) +
                               geom_jitter(aes(color = Type), alpha = 0.05) +
                               geom_vline(aes(xintercept = 153)) +
                               geom_vline(aes(xintercept = 64)) +
                               geom_vline(aes(xintercept = 58), linetype = "dashed", color = "blue") +
                               geom_vline(aes(xintercept = 157), linetype = "dashed", color = "red") +
                               geom_boxplot(notch = T, outlier.size = 1.5)  +
                               labs(y = "Stimulation Outcome", x = "Pain Intensity Rating (VAS)") +
                               scale_fill_manual(values=c( "grey", "chocolate1")) +
                               scale_color_manual(values=c("black", "chocolate1")) +
                               annotation_custom(inh) +
                               annotation_custom(fac) +
                               annotation_custom(sig1) +
                               annotation_custom(sig2) +
                               theme_pubr() +
                               theme(legend.position="bottom")
plot_GroupModulation

#==============================================================================#