#==============================================================================#
# Manipulation Checks
#==============================================================================#

# AUTHOR:   > FH

# Script used to generate the plots for the manipulation checks figure.

#==============================================================================#

# ============= #
# Packages
# ============= #

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(plyr)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(lme4)))    
suppressMessages(suppressWarnings(library(effects))) 
suppressMessages(suppressWarnings(library(lmerTest)))
suppressMessages(suppressWarnings(library(lmtest)))  
suppressMessages(suppressWarnings(library(ggplot2)))     
suppressMessages(suppressWarnings(library(ggpubr)))
suppressMessages(suppressWarnings(library(grid)))      

analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()


# ============= #
# Data-sets 
# ============= #

# Task Data
data_File <- file.path(analysis_dir, 'Task_Data.csv')
data <- read.csv(data_File, sep = ";", header = T) 
DF <- as.data.frame(data)

# ============= #
# Bayesian T-Tests results 
# ============= #

# Results come JASP
# But could also have been obtained with the BayesFactor Package or else

# =============
# Thresholds

# Comp: Pr1 - Pr2 (AB)
Tr_AB <- 4.75
# Comp: Pr1 - Pr3 (AC)
Tr_AC <- 4.72
# Comp: Pr1 - Pr4 (AD)
Tr_AD <- 3.555
# Comp: Pr1 - Po1 (AE)
Tr_AE <- 0.231
# Comp: Pr1 - Po2 (AF)
Tr_AF <- -0.105
# Comp: Pr1 - Po3 (AG)
Tr_AG <-  3.552
# Comp: Pr1 - Po4 (AH)
Tr_AH <- -0.748
# Comp: Pr2 - Pr3 (BC)
Tr_BC <- -0.936
# Comp: Pr2 - Pr4 (BD)
Tr_BD <- -1.599
# Comp: Pr2 - Po1 (BE)
Tr_BE <- -1.415
# Comp: Pr2 - Po2 (BF)
Tr_BF <- -1.512
# Comp: Pr2 - Po3 (BG)
Tr_BG <- -1.540
# Comp: Pr2 - Po4 (BH)
Tr_BH <- -0.867
# Comp: Pr3 - Pr4 (CD)
Tr_CD <- -1.038
# Comp: Pr3 - Po1 (CE)
Tr_CE <- -1.637
# Comp: Pr3 - Po2 (CF)
Tr_CF <- -1.633
# Comp: Pr3 - Po3 (CG)
Tr_CG <- -0.646
# Comp: Pr3 - Po4 (CH)
Tr_CH <- -1.410
# Comp: Pr4 - Po1 (DE)
Tr_DE <- -1.504
# Comp: Pr4 - Po2 (DF)
Tr_DF <- -1.561
# Comp: Pr4 - Po3 (DG)
Tr_DG <- -1.400
# Comp: Pr4 - Po4 (DH)
Tr_DH <- -0.960
# Comp: Po1 - Po2 (EF)
Tr_EF <- -1.628
# Comp: Po1 - Po3 (EG)
Tr_EG <- -0.334
# Comp: Po1 - Po4 (EH)
Tr_EH <- -1.387
# Comp: Po2 - Po3 (FG)
Tr_FG <- -0.429
# Comp: Po2 - Po4 (FH)
Tr_FH <- -0.742
# Comp: Po3 - Po4 (GH)
Tr_GH <- 1.981

# =============
# Tolerences

# Comp: Pr1 - Pr2 (AB)
To_AB <- 4.911
# Comp: Pr1 - Pr3 (AC)
To_AC <- 3.598
# Comp: Pr1 - Pr4 (AD)
To_AD <- 4.139
# Comp: Pr1 - Po1 (AE)
To_AE <- 1.900
# Comp: Pr1 - Po2 (AF)
To_AF <- 0.994
# Comp: Pr1 - Po3 (AG)
To_AG <- 5.574
# Comp: Pr1 - Po4 (AH)
To_AH <- 0.676
# Comp: Pr2 - Pr3 (BC)
To_BC <- -1.510
# Comp: Pr2 - Pr4 (BD)
To_BD <- -1.636
# Comp: Pr2 - Po1 (BE)
To_BE <- -1.601
# Comp: Pr2 - Po2 (BF)
To_BF <- -1.633
# Comp: Pr2 - Po3 (BG)
To_BG <- -0.298
# Comp: Pr2 - Po4 (BH)
To_BH <- -1.540
# Comp: Pr3 - Pr4 (CD)
To_CD <- -1.420
# Comp: Pr3 - Po1 (CE)
To_CE <- -1.443
# Comp: Pr3 - Po2 (CF)
To_CF <- -1.530
# Comp: Pr3 - Po3 (CG)
To_CG <- -0.979
# Comp: Pr3 - Po4 (CH)
To_CH <- -1.373
# Comp: Pr4 - Po1 (DE)
To_DE <- -1.613
# Comp: Pr4 - Po2 (DF)
To_DF <- -1.636
# Comp: Pr4 - Po3 (DG)
To_DG <- -0.292
# Comp: Pr4 - Po4 (DH)
To_DH <- -1.549
# Comp: Po1 - Po2 (EF)
To_EF <- -1.619
# Comp: Po1 - Po3 (EG)
To_EG <- 1.609
# Comp: Po1 - Po4 (EH)
To_EH <- -1.603
# Comp: Po2 - Po3 (FG)
To_FG <- 0.237
# Comp: Po2 - Po4 (FH)
To_FH <- -1.495
# Comp: Po3 - Po4 (GH)
To_GH <- 2.876

# =============
# Neutral trials

# Comp: neutr1 - neutr2 (AB)
n_12 <- -0.595
# Comp: neutr1 - neutr3 (AC)
n_13 <- -1.329
# Comp: neutr1 - neutr4 (AD)
n_14 <- -0.016
# Comp: neutr2 - neutr3 (AE)
n_23 <- -1.346
# Comp: neutr2 - neutr4 (AF)
n_24 <- -1.205
# Comp: neutr3 - neutr4 (AG)
n_34 <- -0.451

# ============= #
# Heatplots
# ============= #

x <- c(
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4"
)
y <- c(
  "A. Pre-1", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "B. Pre-2", "B. Pre-2", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "C. Pre-3", "C. Pre-3", "C. Pre-3", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "D. Pre-4", "D. Pre-4", "D. Pre-4", "D. Pre-4", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "E. Post-1", "E. Post-1", "E. Post-1", "E. Post-1", "E. Post-1", "F. Post-2", "G. Post-3", "H. Post-4",
  "F. Post-2", "F. Post-2", "F. Post-2", "F. Post-2", "F. Post-2", "F. Post-2", "G. Post-3", "H. Post-4",
  "G. Post-3", "G. Post-3", "G. Post-3", "G. Post-3", "G. Post-3", "G. Post-3", "G. Post-3", "H. Post-4",
  "H. Post-4", "H. Post-4", "H. Post-4", "H. Post-4", "H. Post-4", "H. Post-4", "H. Post-4", "H. Post-4"
)

#Thresholds

log_BF <- c(                           # Maybe better way to do it but this seem to slice the matrix in half
            0,0,0,0,0,0,0,0,
            Tr_AB,0,0,0,0,0,0,0,
            Tr_AC,Tr_BC,0,0,0,0,0,0,
            Tr_AD,Tr_BD,Tr_CD,0,0,0,0,0,
            Tr_AE,Tr_BE,Tr_CE,Tr_DE,0,0,0,0,
            Tr_AF,Tr_BF,Tr_CF,Tr_DF,Tr_EF,0,0,0,
            Tr_AG,Tr_BG,Tr_CG,Tr_DG,Tr_EG,Tr_FG,0,0,
            Tr_AH,Tr_BH,Tr_CH,Tr_DH,Tr_EH,Tr_FH,Tr_GH,0
)
df_thresh<-data.frame(x,y,log_BF)
plotHPT_mat <- ggplot(df_thresh, aes(x = x, y = y, fill = log_BF)) +
                      geom_tile(color = "black") +
                      geom_text(aes(label = log_BF), color = "black", size = 4) +
                      coord_fixed() +
                      scale_fill_gradient2(low = "dodgerblue3", high = "red3", midpoint = 0) +
                      labs(x = "Pre-Post", y = "Pre-Post") +
                      theme_minimal()
plotHPT_mat

#Thresholds
log_BF <- c(
            0,0,0,0,0,0,0,0,     
            To_AB,0,0,0,0,0,0,0,
            To_AC,To_BC,0,0,0,0,0,0,
            To_AD,To_BD,To_CD,0,0,0,0,0,
            To_AE,To_BE,To_CE,To_DE,0,0,0,0,
            To_AF,To_BF,To_CF,To_DF,To_EF,0,0,0,
            To_AG,To_BG,To_CG,To_DG,To_EG,To_FG,0,0,
            To_AH,To_BH,To_CH,To_DH,To_EH,To_FH,To_GH,0
)
df_tol <- data.frame(x,y,log_BF)
plotTOL_mat <- ggplot(df_tol, aes(x = x, y = y, fill = log_BF)) +
                      geom_tile(color = "black") +
                      geom_text(aes(label = log_BF), color = "black", size = 4) +
                      coord_fixed() +
                      scale_fill_gradient2(low = "dodgerblue3", high = "red3", midpoint = 0) +
                      labs(x = "Pre-Post", y = "Pre-Post") +
                      theme_minimal()
plotTOL_mat

# Neutral trials
x <- c( 
  "A. trial 1", "B. trial 2", "C. trial 3", "D. trial 4",
  "A. trial 1", "B. trial 2", "C. trial 3", "D. trial 4", 
  "A. trial 1", "B. trial 2", "C. trial 3", "D. trial 4",
  "A. trial 1", "B. trial 2", "C. trial 3", "D. trial 4"
)
  
y <- c(
  "A. trial 1", "B. trial 2", "C. trial 3", "D. trial 4",
  "B. trial 2", "B. trial 2", "C. trial 3", "D. trial 4",
  "C. trial 3", "C. trial 3", "C. trial 3", "D. trial 4",
  "D. trial 4", "D. trial 4", "D. trial 4", "D. trial 4"
)

log_BF <- c(
            0,0,0,0,
            n_12,0,0,0,
            n_13,n_23,0,0,
            n_14,n_24,n_34,0
  )
df_neu <- data.frame(x,y,log_BF)
plotNEU_mat <- ggplot(df_neu, aes(x = x, y = y, fill = log_BF)) +
                      geom_tile(color = "black") +
                      geom_text(aes(label = log_BF), color = "black", size = 4) +
                      coord_fixed() +
                      scale_fill_gradient2(low = "dodgerblue3", high = "red3", midpoint = 0) +
                      labs(x = "Neutral trials of the task", y = "Neutral trials of the task") +
                      theme_minimal()
plotNEU_mat

#==============================================================================#