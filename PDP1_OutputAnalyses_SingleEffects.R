#==============================================================================#
# PDP1: analyses of single effect of model extracted quantities
#==============================================================================#

# AUTHORS:     > FH

# Script that assesses the fixed effect of modelled quantities on the 
# extent of modulation. Namely the entropy of the state belief, the Bayesian 
# surprise (both from the best fitting HMM model), and the RL prediction-error#
# from the best fitting RL model.

#==============================================================================#

#--------------------# 
# Path, Packages and data 
#--------------------#

# Path and software package
analysis_dir <- getwd()
suppressMessages(suppressWarnings(library(lme4))) 
suppressMessages(suppressWarnings(library(effects))) 
suppressMessages(suppressWarnings(library(lmerTest))) 
suppressMessages(suppressWarnings(library(ggplot2)))  
suppressMessages(suppressWarnings(library(ggpubr)))   
suppressMessages(suppressWarnings(library(grid)))  
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# Data file
data_file <- file.path(analysis_dir, "Extracted_Quantities.csv")   
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data) 

#--------------------# 
# Curation
#--------------------#

outcomeWIN <- subset(DF, outcome == "win")
outcomeLOSE <- subset(DF, outcome == "lose")
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
# Fixed effects in trials with pain relief
#--------------------#

# Fixed effect of RL prediction error

Rew_PE_model <- lmer(end_mod ~ pe + (1|subject), data = outcomeWIN)
summary(Rew_PE_model)
plot(allEffects(Rew_PE_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ pe + (1 | subject)
#   Data: outcomeWIN

#REML criterion at convergence: 13298.9

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.7820 -0.5262 -0.0180  0.5090  4.5951 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  67.96    8.244  
# Residual             429.15   20.716  
#Number of obs: 1488, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error      df t value Pr(>|t|)  
#(Intercept)   -6.875      3.070  91.231   -2.24   0.0275 *
#pe             2.226      3.534 182.092    0.63   0.5296  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#   (Intr)
#pe -0.854

confint(Rew_PE_model)

#2.5 %     97.5 %
#.sig01        6.080944 10.8917831
#.sigma       19.982069 21.4869246
#(Intercept) -12.866117 -0.8554947
#pe           -4.686739  9.1358630

# Fixed effect of Entropy (uncertainty)

Rew_ENTROPY_model <- lmer(end_mod ~ entropy + (1|subject), data = outcomeWIN)
summary(Rew_ENTROPY_model)
plot(allEffects(Rew_ENTROPY_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ entropy + (1 | subject)
#   Data: outcomeWIN

#REML criterion at convergence: 13292

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.7784 -0.5392 -0.0193  0.5064  4.6163 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  81.3     9.017  
# Residual             426.1    20.643  
#Number of obs: 1488, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  -13.476      3.677 101.457  -3.665 0.000396 ***
#entropy       15.899      6.254 193.176   2.542 0.011806 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#        (Intr)
#entropy -0.882

confint(Rew_ENTROPY_model)

#2.5 %    97.5 %
#.sig01        6.585067 12.001137
#.sigma       19.911451 21.412711
#(Intercept) -21.019376 -6.041449
#entropy       2.986885 28.713699

# Fixed effect of Bayesian Surprise (Model update)

Rew_SURPRISE_model <- lmer(end_mod ~ surprise + (1|subject), data = outcomeWIN)
summary(Rew_SURPRISE_model)
plot(allEffects(Rew_SURPRISE_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ surprise + (1 | subject)
#   Data: outcomeWIN

#REML criterion at convergence: 13291.6

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.7801 -0.5377 -0.0202  0.5105  4.6193 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  67.21    8.198  
# Residual             427.25   20.670  
#Number of obs: 1488, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   -6.314      1.642   32.540  -3.845 0.000529 ***
#surprise      12.365      4.612 1422.172   2.681 0.007420 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#         (Intr)
#surprise -0.247

confint(Rew_SURPRISE_model)

#2.5 %    97.5 %
#.sig01       6.094362 10.897551
#.sigma      19.935381 21.436656
#(Intercept) -9.572805 -3.058313
#surprise     3.332566 21.400659

#--------------------# 
# Fixed effects in trials with pain punishments
#--------------------#

# Fixed effect of RL prediction error

Pun_PE_model <- lmer(end_mod ~ pe + (1|subject), data = outcomeLOSE)
summary(Pun_PE_model)
plot(allEffects(Pun_PE_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ pe + (1 | subject)
#   Data: outcomeLOSE

#REML criterion at convergence: 12391.6

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-8.0176 -0.4372  0.1351  0.5494  3.3998 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  26.02    5.101  
# Residual             428.46   20.699  
#Number of obs: 1389, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error       df t value Pr(>|t|)
#(Intercept)  -0.9541     3.2198 119.5061  -0.296    0.768
#pe           -3.9953     2.7665 157.2503  -1.444    0.151

#Correlation of Fixed Effects:
#   (Intr)
#pe 0.941 

confint(Pun_PE_model)

#2.5 %    97.5 %
#.sig01       3.452689  7.001744
#.sigma      19.940539 21.497530
#(Intercept) -7.239959  5.331262
#pe          -9.394528  1.403713

# Fixed effect of Entropy (uncertainty) 

Pun_ENTROPY_model <- lmer(end_mod ~ entropy + (1|subject), data = outcomeLOSE)
summary(Pun_ENTROPY_model)
plot(allEffects(Pun_ENTROPY_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ entropy + (1 | subject)
#   Data: outcomeLOSE

#REML criterion at convergence: 12390.5

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-8.0028 -0.4371  0.1323  0.5573  3.3923 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  23.53    4.851  
# Residual             429.38   20.722  
#Number of obs: 1389, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error      df t value Pr(>|t|)  
#(Intercept)    8.722      4.648 136.625   1.876   0.0627 .
#entropy       -8.575      7.328 152.975  -1.170   0.2438  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#        (Intr)
#entropy -0.974

confint(Pun_ENTROPY_model)

#2.5 %    97.5 %
#.sig01        3.1642521  6.773597
#.sigma       19.9621489 21.522189
#(Intercept)  -0.7323042 18.120934
#entropy     -23.4434904  6.331084

# Fixed effect of Bayesian Surprise (Model update)

Pun_SURPRISE_model <- lmer(end_mod ~ surprise + (1|subject), data = outcomeLOSE)
summary(Pun_SURPRISE_model)
plot(allEffects(Pun_SURPRISE_model))

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: end_mod ~ surprise + (1 | subject)
#   Data: outcomeLOSE

#REML criterion at convergence: 12386.8

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-8.0076 -0.4385  0.1407  0.5535  3.3190 

#Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept)  26.79    5.175  
# Residual             428.75   20.706  
#Number of obs: 1389, groups:  subject, 30

#Fixed effects:
#            Estimate Std. Error      df t value Pr(>|t|)   
#(Intercept)    4.078      1.416  35.380   2.881  0.00669 **
#surprise     -47.676     65.084  51.210  -0.733  0.46718   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#         (Intr)
#surprise -0.631

confint(Pun_SURPRISE_model)

#2.5 %    97.5 %
#.sig01         3.478094  7.047980
#.sigma        19.949665 21.507458
#(Intercept)    1.301069  6.846401
#surprise    -174.510841 79.612880


#--------------------# 
# Plots
#--------------------#

data_modulation$Outcome <- data_modulation$mod_outcome

# Utility
levels(data_modulation$Outcome) <- c('Relief', 'Punishment')

# Annotations
inh <- grobTree(textGrob("Pain Inhibition", x=0.02,  y=0.10, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))
fac <- grobTree(textGrob("Pain Facilitation", x=0.02,  y=0.90, hjust=0, gp=gpar(col="black", fontsize=10, fontface="bold")))

# Modulation ~ Prediction Error (not kept in final manuscript version - no significant effect)

pl1 <- ggplot(data_modulation, aes(x = mod_pe, y = mod_modulation, color = Outcome)) +
              geom_point(alpha = 0.1) +
              geom_smooth(method = lm) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'RL: Prediction Error') +
              scale_color_manual(values = c('blue', 'red')) +
              ylim(-40, 40) +
              annotation_custom(inh) +
              annotation_custom(fac) +
              theme_classic() +
              theme(legend.position='none')
pl1

# Modulation ~ Entropy

sig1 <- grobTree(textGrob("*", x=0.4,  y=0.35, hjust=0, gp=gpar(col="blue", fontsize=30, fontface="bold")))

pl2 <- ggplot(data_modulation, aes(x = mod_entropy, y = mod_modulation, color = Outcome)) +
              geom_point(alpha = 0.1) +
              geom_smooth(method = lm) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Entropy (Uncertainty)') +
              scale_color_manual(values = c('blue', 'red')) +
              ylim(-40, 40) +
              annotation_custom(inh) +
              annotation_custom(fac) +
              #annotation_custom(sig1) +
              theme_pubr() +
              theme(legend.position='none')
pl2

# Modulation ~ Surprise

sig2 <- grobTree(textGrob("**", x=0.4,  y=0.35, hjust=0, gp=gpar(col="blue", fontsize=30, fontface="bold")))

pl3 <- ggplot(data_modulation, aes(x = mod_surprise, y = mod_modulation, color = Outcome)) +
              geom_point(alpha = 0.1) +
              geom_smooth(method = lm) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Bayesian Surprise (Belief Updating)') +
              scale_color_manual(values = c('blue', 'red')) +
              ylim(-40, 40) +
              annotation_custom(inh) +
              annotation_custom(fac) +
              #annotation_custom(sig2) +
              theme_pubr() +
              theme(legend.position='none')
pl3

# Fixed Effect Uncertainty

pla <- ggplot(outcomeWIN, aes(x=entropy, y=end_mod, color = outcome)) +
              #geom_point(alpha = 0.1) +
              geom_line(aes(y=predict(Rew_ENTROPY_model, re.form = NA), group=subject), size = 3) +
              geom_line(aes(y=predict(Rew_ENTROPY_model, re.form = NULL), group=subject), size = 0.7, alpha = 0.3) +
              scale_color_manual(values = c("blue")) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Entropy (Uncertainty)') +
              theme_pubr() +
              #annotation_custom(inh) +
              #annotation_custom(fac) +
              annotation_custom(sig1) +
              theme(legend.position='none')
pla

# Fixed Effect Bayesian Surprise

plb <- ggplot(outcomeWIN, aes(x=surprise, y=end_mod, color = outcome)) +
              #geom_point(alpha = 0.1) +
              geom_line(aes(y=predict(Rew_SURPRISE_model, re.form = NA), group=subject), size = 3) +
              geom_line(aes(y=predict(Rew_SURPRISE_model, re.form = NULL), group=subject), size = 0.7, alpha = 0.3) +
              scale_color_manual(values = c("blue")) +
              geom_hline(aes(yintercept = 0)) +
              labs(y = 'Extent of Modulation (a.u.)', x = 'Bayesian Surprise (Belief Update)') +
              theme_pubr() +
              #annotation_custom(inh) +
              #annotation_custom(fac) +
              annotation_custom(sig2) +
              theme(legend.position='none')
plb

#==============================================================================#
#------------------------------------------------------------------------------#
#==============================================================================#