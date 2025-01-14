#==============================================================================#
# PDP1: Parameter recovery results
#==============================================================================#

# > FH

# Script used for the plotting of the parameter recovery results (group level)

#==============================================================================#

#----------#
# Utility
#----------#

suppressMessages(suppressWarnings(library(HDInterval)))
suppressMessages(suppressWarnings(library(tictoc)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ggpubr)))
analysis_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(analysis_path))
analysis_dir <- getwd()

#----------#
# Datasets for group level recovery
#----------#

# recoverd hyper parameters
data_delta <- file.path(analysis_dir, 'Delta_group_recovery.xlsx')
datadelta <- read_xlsx(data_delta) 
DFdelta <- as.data.frame(datadelta)
#
data_hmm <- file.path(analysis_dir, 'HMM_group_recovery.xlsx')
datahmm <- read_xlsx(data_hmm) 
DFhmm <- as.data.frame(datahmm)

# generating values for hyper parameters 
gen_rl <- file.path(analysis_dir, 'generating_values_delta.xlsx')
genrl <- read_xlsx(gen_rl) 
genrl <- as.data.frame(genrl)
#
gen_hmm <- file.path(analysis_dir, 'generating_values_hmm.xlsx')
genhmm <- read_xlsx(gen_hmm) 
genhmm <- as.data.frame(genhmm)

# Utility for group level results
collumn_NAMES <- colnames(DFdelta)  # vector of collumn names kept because of all df manipulations
collumn_NAMES2 <- colnames(DFhmm)
dodge = position_dodge(.4)

#==============================================================================#
# Group Level recovery for alpha-pos from diff-delta
#==============================================================================#

BT_lr <- c()

tic()
for (collumn in 1:ncol(DFdelta)) {
  
  that_one_collumn <- DFdelta[,collumn]
  #that_one_collumn <- pnorm(DFdelta[,collumn])  #  in case want back-transformation applied
  BT_lr <- cbind(BT_lr, that_one_collumn)
  
}
toc()

# Give back original collumn names
colnames(BT_lr) <- collumn_NAMES

# format to DF to ease recovery of elements
BT_lr <- as.data.frame(BT_lr)

# pos-learning                           
rec_pos_1  <- BT_lr$alpha_pos
rec_pos_2  <- BT_lr$alpha_pos.1
rec_pos_3  <- BT_lr$alpha_pos.2
rec_pos_4  <- BT_lr$alpha_pos.3
rec_pos_5  <- BT_lr$alpha_pos.4
rec_pos_6  <- BT_lr$alpha_pos.5
rec_pos_7  <- BT_lr$alpha_pos.6
rec_pos_8  <- BT_lr$alpha_pos.7
rec_pos_9  <- BT_lr$alpha_pos.8
rec_pos_10 <- BT_lr$alpha_pos.9
# DF of recovered positive learning rates
rec_pos_DF <- data.frame(rec_pos_1, rec_pos_2, rec_pos_3, rec_pos_4, rec_pos_5, rec_pos_6, rec_pos_7, rec_pos_8, rec_pos_9, rec_pos_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_pos_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_pos_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_pos_DF[,collumn])
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# results_a_pos <- data.frame(recovered_means, recovered_lower, recovered_upper)
# appends generating values for plotting
mean_gen <- genrl$alpha_pos_mu_pr
#mean_gen <- pnorm(genrl$alpha_pos_mu_pr)     # in case want back transformation applied
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_alpha_pos <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)

# RESULT
plot_1 <- ggplot(plot_alpha_pos,
                 aes(x = Simulation, y = MEANS, color = Type)) +
                 geom_point(shape = 19, size  = 3, position = dodge) +
                 geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
                 scale_color_manual(values=c("chocolate3", "black")) +
                 labs(y = 'Learning rate: α pos', x = 'Simulation n°') +
                 theme_pubr()
plot_1

#==============================================================================#
# Group Level recovery for alpha-neg from diff-delta
#==============================================================================#

# neg learning
rec_neg_1  <- BT_lr$alpha_neg
rec_neg_2  <- BT_lr$alpha_neg.1
rec_neg_3  <- BT_lr$alpha_neg.2
rec_neg_4  <- BT_lr$alpha_neg.3
rec_neg_5  <- BT_lr$alpha_neg.4
rec_neg_6  <- BT_lr$alpha_neg.5
rec_neg_7  <- BT_lr$alpha_neg.6
rec_neg_8  <- BT_lr$alpha_neg.7
rec_neg_9  <- BT_lr$alpha_neg.8
rec_neg_10 <- BT_lr$alpha_neg.9

# DF of recovered negative learning rates
rec_neg_DF <- data.frame(rec_neg_1, rec_neg_2, rec_neg_3, rec_neg_4, rec_neg_5, rec_neg_6, rec_neg_7, rec_neg_8, rec_neg_9, rec_neg_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_neg_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_neg_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_neg_DF[,collumn])
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# results_a_pos <- data.frame(recovered_means, recovered_lower, recovered_upper)
# appends generating values for plotting
mean_gen <- genrl$alpha_neg_mu_pr
#mean_gen <- pnorm(genrl$alpha_neg_mu_pr)     # activate for backtransformation 
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_alpha_neg <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)
plot_alpha_neg

# RESULT
plot_2 <- ggplot(plot_alpha_neg,
                 aes(x = Simulation, y = MEANS, color = Type)) +
                 geom_point(shape = 19, size  = 3, position = dodge) +
                 geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
                 scale_color_manual(values=c("chocolate3", "black")) +
                 labs(y = 'Learning rate: α neg', x = 'Simulation n°') +
                 theme_pubr()
plot_2

#==============================================================================#
# Group Level recovery for Inverse temperatures from diff-delta
#==============================================================================#

BT_it <- c() 

tic()
for (collumn in 1:ncol(DFdelta)) {
  
  that_one_collumn <- DFdelta[,collumn]
  #that_one_collumn <- pnorm(DFdelta[,collumn]) * 20   # activate for backtransformation
  BT_it <- cbind(BT_it, that_one_collumn)
  
}
toc()

# Give back original collumn names
colnames(BT_it) <- collumn_NAMES

# format to DF to ease recovery of elements
BT_it <- as.data.frame(BT_it)

# Inverse-temp      
rec_it_1  <- BT_it$beta
rec_it_2  <- BT_it$beta.1
rec_it_3  <- BT_it$beta.2
rec_it_4  <- BT_it$beta.3
rec_it_5  <- BT_it$beta.4
rec_it_6  <- BT_it$beta.5
rec_it_7  <- BT_it$beta.6
rec_it_8  <- BT_it$beta.7
rec_it_9  <- BT_it$beta.8
rec_it_10 <- BT_it$beta.9
# DF of recovered inverse temperatures
rec_it_DF <- data.frame(rec_it_1, rec_it_2, rec_it_3, rec_it_4, rec_it_5, rec_it_6, rec_it_7, rec_it_8, rec_it_9, rec_it_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_it_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_it_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_it_DF[,collumn])
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# appends generating values for plotting
mean_gen <- genrl$beta_mu_pr
# mean_gen <- pnorm(genrl$beta_mu_pr) * 20   # activate for backtransformation
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_beta <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)

# RESULT
plot_3 <- ggplot(plot_beta,
                 aes(x = Simulation, y = MEANS, color = Type)) +
                 geom_point(shape = 19, size  = 3, position = dodge) +
                 geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
                 scale_color_manual(values=c("chocolate3", "black")) +
                 labs(y = 'Inverse temperature β', x = 'Simulation n°') +
                 theme_pubr()
plot_3

#==============================================================================#
# Group Level recovery for emission relief C
#==============================================================================#

C_it <- c()  

# Applies the back-transformation with pnorm * 0.5 + 0.5
tic()
for (collumn in 1:ncol(DFhmm)) {
  
  that_one_collumn <- DFhmm[,collumn]
  #that_one_collumn <- pnorm(DFhmm[,collumn]) * 0.5 + 0.5  # activate for backtransformation 
  C_it <- cbind(C_it, that_one_collumn)
  
}
toc()

# Give back original collumn names
colnames(C_it) <- collumn_NAMES2

# format to DF to ease recovery of elements
C_it <- as.data.frame(C_it)

# Emission C                  
rec_c_1  <- C_it$C
rec_c_2  <- C_it$C.1
rec_c_3  <- C_it$C.2
rec_c_4  <- C_it$C.3
rec_c_5  <- C_it$C.4
rec_c_6  <- C_it$C.5
rec_c_7  <- C_it$C.6
rec_c_8  <- C_it$C.7
rec_c_9  <- C_it$C.8
rec_c_10 <- C_it$C.9
# DF of recovered inverse temperatures
rec_c_DF <- data.frame(rec_c_1, rec_c_2, rec_c_3, rec_c_4, rec_c_5, rec_c_6, rec_c_7, rec_c_8, rec_c_9, rec_c_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_c_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_c_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_c_DF[,collumn])
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# appends generating values for plotting
mean_gen <- genhmm$c_mu_pr
#mean_gen <- pnorm(genhmm$c_mu_pr) * 0.5 + 0.5  # activate for backtransformation
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_C <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)

# RESULT
plot_4 <- ggplot(plot_C,
                 aes(x = Simulation, y = MEANS, color = Type)) +
                 geom_point(shape = 19, size  = 3, position = dodge) +
                 geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
                 scale_color_manual(values=c("red4", "black")) +
                 labs(y = 'Emission c (Relief)', x = 'Simulation n°') +
                 theme_pubr()
plot_4

#==============================================================================#
# Group Level recovery for emission punishment D
#==============================================================================#

D_it <- c()  

tic()
for (collumn in 1:ncol(DFhmm)) {
  
  #that_one_collumn <- pnorm(DFhmm[,collumn]) * 0.5 + 0.5  # activate for backtransformation
  that_one_collumn <- DFhmm[,collumn]
  D_it <- cbind(D_it, that_one_collumn)
  
}
toc()

# Give back original collumn names
colnames(D_it) <- collumn_NAMES2

# format to DF to ease recovery of elements
D_it <- as.data.frame(D_it)

# Inverse-temp                         
rec_d_1  <- D_it$D
rec_d_2  <- D_it$D.1
rec_d_3  <- D_it$D.2
rec_d_4  <- D_it$D.3
rec_d_5  <- D_it$D.4
rec_d_6  <- D_it$D.5
rec_d_7  <- D_it$D.6
rec_d_8  <- D_it$D.7
rec_d_9  <- D_it$D.8
rec_d_10 <- D_it$D.9
# DF of recovered inverse temperatures
rec_d_DF <- data.frame(rec_d_1, rec_d_2, rec_d_3, rec_d_4, rec_d_5, rec_d_6, rec_d_7, rec_d_8, rec_d_9, rec_d_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_d_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_d_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_d_DF[,collumn])
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# appends generating values for plotting
#mean_gen <- pnorm(genhmm$d_mu_pr) * 0.5 + 0.5   # activate for backtransformation
mean_gen <- genhmm$d_mu_pr
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_D <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)

# RESULT
plot_5 <- ggplot(plot_D,
                 aes(x = Simulation, y = MEANS, color = Type)) +
  geom_point(shape = 19, size  = 3, position = dodge) +
  geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
  scale_color_manual(values=c("red4", "black")) +
  labs(y = 'Emission d (Punishment)', x = 'Simulation n°') +
  theme_pubr()
plot_5

#==============================================================================#
# Group Level recovery for transition GAMMA
#==============================================================================#

ga_it <- c()  

tic()
for (collumn in 1:ncol(DFhmm)) {
  
  that_one_collumn <- DFhmm[,collumn]
  #that_one_collumn <- pnorm(DFhmm[,collumn])      # activate for backtransformation
  ga_it <- cbind(ga_it, that_one_collumn)
  
}
toc()

# Give back original collumn names
colnames(ga_it) <- collumn_NAMES2

# format to DF to ease recovery of elements
ga_it <- as.data.frame(ga_it)

# Inverse-temp                      
rec_ga_1  <- ga_it$GAMMA   
rec_ga_2  <- ga_it$GAMMA.1
rec_ga_3  <- ga_it$GAMMA.2
rec_ga_4  <- ga_it$GAMMA.3
rec_ga_5  <- ga_it$GAMMA.4
rec_ga_6  <- ga_it$GAMMA.5
rec_ga_7  <- ga_it$GAMMA.6
rec_ga_8  <- ga_it$GAMMA.7
rec_ga_9  <- ga_it$GAMMA.8
rec_ga_10 <- ga_it$GAMMA.9
# DF of recovered inverse temperatures
rec_ga_DF <- data.frame(rec_ga_1, rec_ga_2, rec_ga_3, rec_ga_4, rec_ga_5, rec_ga_6, rec_ga_7, rec_ga_8, rec_ga_9, rec_ga_10)

# Creates a vector for all 10 mean recovered values, upper and lower HDIs
recovered_means <- c()  # empty vectors for values extracted
recovered_lower <- c()
recovered_upper <- c()

# Loops over the recovered parameters
tic()
for (collumn in 1:ncol(rec_ga_DF)) {
  
  # gets the recovered mean posterior
  that_one_mean <- mean(rec_ga_DF[,collumn])
  recovered_means <- rbind(recovered_means, that_one_mean)
  
  # gets bounds of recovered HDI
  that_one_HDI <- hdi(rec_ga_DF[,collumn], credMass = 0.95)
  that_one_HDI <- as.vector(that_one_HDI)
  that_one_lower <- that_one_HDI[1]
  that_one_upper <- that_one_HDI[2]
  #
  recovered_lower <- rbind(recovered_lower, that_one_lower)
  recovered_upper <- rbind(recovered_upper, that_one_upper)
  
}
toc()

# appends generating values for plotting
mean_gen <- genhmm$gamma_mu_pr
#mean_gen <- pnorm(genhmm$gamma_mu_pr)        # activate for backtransformation 
dim(mean_gen) <- c(10,1)                     # transpose to make appending possible

# Needed to create the DF used for generating plot
MEANS <- rbind(mean_gen,recovered_means)
LOWER <- rbind(mean_gen,recovered_lower)
UPPER <- rbind(mean_gen,recovered_upper)
type <- c("Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating", "Generating",
          "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" , "Recovered" )
TYPE <- as.factor(type)
dim(TYPE) <- c(20,1)
Type <- TYPE
Simulation <- c(1:10)
Simulation <- c(Simulation, Simulation)
Simulation <- as.vector(Simulation)
Simulation <- as.factor(Simulation)
#
dim(Simulation) <- c(20,1)
#
plot_GAMMA <- data.frame(Simulation,MEANS,LOWER,UPPER,Type)

# RESULT
plot_6 <- ggplot(plot_GAMMA,
                 aes(x = Simulation, y = MEANS, color = Type)) +
                 geom_point(shape = 19, size  = 3, position = dodge) +
                 geom_errorbar(aes(ymin  = LOWER, ymax  = UPPER), width = 0.0, size  = 1, position = dodge) +
                 scale_color_manual(values=c("red4", "black")) +
                 labs(y = 'Transition Gamma', x = 'Simulation n°') +
                 theme_pubr()
plot_6

#==============================================================================#
# Figure, group level recovery
#==============================================================================#

Figure <- ggarrange(plot_1, plot_2, plot_3,
                    plot_4, plot_5, plot_6,
                    ncol = 3,
                    nrow = 2,
                    labels = c('A', 'B', 'C',
                               'D', 'E', 'F'),
                    legend = 'top',
                    common.legend = FALSE)
Figure

#==============================================================================#
#------------------------------------------------------------------------------#
#==============================================================================#