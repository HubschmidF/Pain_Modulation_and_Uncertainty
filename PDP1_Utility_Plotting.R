#==============================================================================#
# PDP1: Utility script for plotting
#==============================================================================#

# > FH

# Script just used for plotting, the evolution of global volatility over the 
# sequence and the modelled evolution of the state belief over the sequence
# for each participant

#==============================================================================#

analysis_dir <- getwd()
suppressMessages(suppressWarnings(library(ggplot2)))  
suppressMessages(suppressWarnings(library(grid)))  
suppressMessages(suppressWarnings(library(ggpubr)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()
# Data file
data_file <- file.path(analysis_dir, "Extracted_Quantities.csv")
data <- read.csv(data_file, header = T, sep = ";")
DF <- as.data.frame(data) 
#

# ===== #
# Volatility plot
# ===== #

bl <- DF[393:492, 8]  # random participant that did no miss a single trial
tt <- 1:100
volatility <- (2*0.75)/bl     # global volatility shared across actions
volDF <- data.frame(tt, volatility)

ggplot(data=volDF, aes(x=tt, y=volatility)) +
        geom_line()+
        geom_point() +
        labs(y = 'Volatility', x = 'Test Trial n°') +
        theme_classic() 

plot(tt, volatility)

# ===== #
# Plot with belief and true task state
# ===== #

plot(DF$test_trial, DF$state_belief)

# Annotations
B1 <- grobTree(textGrob("____________", x=0.03,  y=1, hjust=0, gp=gpar(col="#4c9ae9", fontsize=25, fontface="bold")))
B2 <- grobTree(textGrob("_________", x=0.237,  y=0.10, hjust=0, gp=gpar(col="#bf2ec2", fontsize=25, fontface="bold")))
B3 <- grobTree(textGrob("___________", x=0.395,  y=1, hjust=0, gp=gpar(col="#4c9ae9", fontsize=25, fontface="bold")))
B4 <- grobTree(textGrob("_________", x=0.585,  y=0.10, hjust=0, gp=gpar(col="#bf2ec2", fontsize=25, fontface="bold")))
B5 <- grobTree(textGrob("_________", x=0.74,  y=1, hjust=0, gp=gpar(col="#4c9ae9", fontsize=25, fontface="bold")))
B6 <- grobTree(textGrob("____", x=0.9,  y=0.10, hjust=0, gp=gpar(col="#bf2ec2", fontsize=25, fontface="bold")))

# Plot 
pl <- ggplot(DF, aes(x = test_trial, y = state_belief, group = subject)) +
              geom_line(alpha = 0.9, col = "darkgrey") +
              geom_vline(aes(xintercept = 21), color = 'black', linewidth = 0.5) +
              geom_vline(aes(xintercept = 38), color = 'black', linewidth = 0.5) + 
              geom_vline(aes(xintercept = 59), color = 'black', linewidth = 0.5) +
              geom_vline(aes(xintercept = 76), color = 'black', linewidth = 0.5) +
              geom_vline(aes(xintercept = 93), color = 'black', linewidth = 0.5) +
              labs(y = 'Pr(state) = Blue', x = 'Test Trial n°') +
              annotation_custom(B1) +
              annotation_custom(B2) +
              annotation_custom(B3) +
              annotation_custom(B4) +
              annotation_custom(B5) +
              annotation_custom(B6) +
              theme_pubr() +
              theme(legend.position='none')
pl

#==============================================================================#