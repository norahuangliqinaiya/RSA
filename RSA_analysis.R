rm(list=ls())

# set path to where you store the download file
setwd("C:/RSA")
library(misty)
library(foreign)

# =================================================
# national sim matrix all
# Read data organized as the demo document
my_data <- as.data.frame(read.table('sim_data.txt'))

# # Define an empthy matrix to store the similarity values between items (all) for each country

# The numbers of rows are defined by the numbers of cells of the
# lower/upper triangle of the similarity matrices defined by items
# (numbers of items (14) x (numbers of items - 1) (13))/2
sim_trust_country <- matrix(0, 91, 15)

# Compute the similarity in scores between items by Pearson correlation and store 
# the values of coeficient in the prepared matrixe (91 x 15)
correlations <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4)]))

for (i in (1: length(correlations))){
  
  sim_trust_country[,i] <- correlations[[i]][lower.tri(correlations[[i]], diag = FALSE)]
}


# Generate the national profile of trust (all)
# Perform fisher r to z transformation
library(DescTools)
sim_trust_country_r2z_all <- FisherZ(sim_trust_country)
sim_trust_nations_all <- cor(sim_trust_country_r2z_all)

sim_trust_nations_all <- as.data.frame(sim_trust_nations_all)
colnames(sim_trust_nations_all) <- seq(1,15,1)
rownames(sim_trust_nations_all) <- seq(1,15,1)

# =================================================
# National sim matrix inter
my_data <- as.data.frame(read.table('sim_data.txt'))

# # Define an empthy matrix to store the similarity values between items (inter) for each country

# The numbers of rows are defined by the numbers of cells of the
# lower/upper triangle of the similarity matrices defined by items
# (numbers of items (7) x (numbers of items - 1) (6))/2
sim_trust_country <- matrix(0, 21, 15)


correlations <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4,12:18)]))

for (i in (1: length(correlations))){
  
  sim_trust_country[,i] <- correlations[[i]][lower.tri(correlations[[i]], diag = FALSE)]
  
}

# Generate the national profile of trust (inter)
# Perform fisher r to z transformation
library(DescTools)
sim_trust_country_r2z_inter <- FisherZ(sim_trust_country)

sim_trust_nations_inter <- cor(sim_trust_country_r2z_inter)

sim_trust_nations_inter <- as.data.frame(sim_trust_nations_inter)
colnames(sim_trust_nations_inter) <- seq(1,15,1)
rownames(sim_trust_nations_inter) <- seq(1,15,1)
trust_nations_vector_inter <- sim_trust_nations_inter[lower.tri(sim_trust_nations_inter, diag = FALSE)]


# =================================================
# National sim matrix inst
my_data <- as.data.frame(read.table('sim_data.txt'))

# # Define an empthy matrix to store the similarity values between items (inst) for each country

# The numbers of rows are defined by the numbers of cells of the
# lower/upper triangle of the similarity matrices defined by items
# (numbers of items (7) x (numbers of items - 1) (6))/2
sim_trust_country <- matrix(0, 21, 15)

correlations <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4,5:11)]))

for (i in (1: length(correlations))){
  
  sim_trust_country[,i] <- correlations[[i]][lower.tri(correlations[[i]], diag = FALSE)]
  
}

# Generate the national profile of trust (inst)
# Perform fisher r to z transformation
library(DescTools)
sim_trust_country_r2z_inst <- FisherZ(sim_trust_country)

sim_trust_nations_inst <- cor(sim_trust_country_r2z_inst)

sim_trust_nations_inst <- as.data.frame(sim_trust_nations_inst)
colnames(sim_trust_nations_inst) <- seq(1,15,1)
rownames(sim_trust_nations_inst) <- seq(1,15,1)
trust_nations_vector_inst <- sim_trust_nations_inst[lower.tri(sim_trust_nations_inst, diag = FALSE)]


# =================================================
# National sim matrix bet
my_data <- as.data.frame(read.table('sim_data.txt'))

# # Define an empthy matrix to store the similarity values between items (between domains) for each country

# The numbers of rows are defined by the numbers of cells of the similarity matrices defined by items
# (numbers of items of inter(7) x (numbers of items of inst) (7))
sim_trust_country <- matrix(0, 49, 15)

correlations <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4,12:18)], x[,-c(1:4,5:11)]))

for (i in (1: length(correlations))){
  
  sim_trust_country[,i] <- correlations[[i]]
  
}


# Generate the national profile of trust (bet)
# Perform fisher r to z transformation
library(DescTools)
sim_trust_country_r2z_bet <- FisherZ(sim_trust_country)

sim_trust_nations_bet <- cor(sim_trust_country_r2z_bet)

sim_trust_nations_bet <- as.data.frame(sim_trust_nations_bet)
colnames(sim_trust_nations_bet) <- seq(1,15,1)
rownames(sim_trust_nations_bet) <- seq(1,15,1)
trust_nations_vector_bet <- sim_trust_nations_bet[lower.tri(sim_trust_nations_bet, diag = FALSE)]


# =================================================
# National sim matrix of tightness-looseness
my_data <- read.sav("tight.sav", as.data.frame = T)
sim_tight <- (max(abs(dist(my_data[,-1]))) - abs(dist(my_data[,-1])))/max(abs(dist(my_data[,-1])))

# Significance test of the correlation with Mantel test
library(cultevo)
mantel.test(sim_tight, sim_trust_nations_all, plot = T)
mantel.test(sim_tight, sim_trust_nations_inter, plot = T)
mantel.test(sim_tight, sim_trust_nations_inst, plot = T)
mantel.test(sim_tight, sim_trust_nations_bet, plot = T)


# =================================================
# Calculation of RSIN
# Individual level matrix
my_data <- as.data.frame(read.table('sim_data.txt'))

library(dplyr)
library(BBmisc)
library(boot)
library(ggplot2)
library(nlme)
library(reshape)
library(foreign)
library(dplyr)
library(car)
library(lme4)
library(reghelper)
library(gtools)
library(data.table)
library(sjPlot)
library(ggpubr)

# Normalized the score of items within each country

my_data <- my_data %>%
  group_by(Country) %>%
  mutate(I1_z = normalize(I1),
         I2_z = normalize(I2),
         I3_z = normalize(I3),
         I4_z = normalize(I4),
         I5_z = normalize(I5),
         I6_z = normalize(I6),
         I7_z = normalize(I7),
         I8_z = normalize(I8),
         I9_z = normalize(I9),
         I10_z = normalize(I10),
         I11_z = normalize(I11),
         I12_z = normalize(I12),
         I13_z = normalize(I13),
         I14_z = normalize(I14))

trust_dist_indiv <- vector("list", dim(my_data)[1])

# Compute the distance matrix for each subject
# and transform it into similarity matrix with values between 0 and 1 (Sijtrans=1-Dijraw/Max(Dijraw)

for (i in (1:dim(my_data)[1])){
  
  trust_dist_indiv[[i]] <- (max(abs(dist(as.numeric(my_data[i,19:32])))) - abs(dist(as.numeric(my_data[i,19:32]))))/max(abs(dist(as.numeric(my_data[i,19:32]))))
  
}


# Calculate the RSIN (all) within each country using Spearman correlation
country_id <- c(1:15)
num_subj <- 20
num_count <- 15
sim_trust_indiv_nation_all <- matrix(0,num_subj * num_count, 1)
i = 1

for (j in (1:dim(my_data)[1])){
  
  if (my_data[j,2] == country_id[i]){
    sim_trust_indiv_nation_all[j] = cor(trust_dist_indiv[[j]], sim_trust_country_r2z_all[,i], method = "spearman")
  } else {
    i = i+1
    sim_trust_indiv_nation_all[j] = cor(trust_dist_indiv[[j]], sim_trust_country_r2z_all[,i], method = "spearman")
  }
}

my_data$RSIN_all <- sim_trust_indiv_nation_all

# HLM: well-being ~ RSIN * tightness-looseness
library(nlme)
library(sjPlot)
my_data <- as.data.frame(my_data)

model <- lme(LS ~ RSIN_all * tight,
             data = my_data,
             random = ~1 + RSIN_all| Country,
             method = "ML",
             na.action = na.exclude,
             control = lmeControl(opt = "optim"))
summary(model)
ICC(model)
simple_slopes(model)
