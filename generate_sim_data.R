
# =============================================================================
#### Info #### 
# =============================================================================
# 
# Nora (Liqin) Huang
# nora_huangliqin@163.com
#
# Adapted from Guangzhou, 2022
# =============================================================================

rm(list=ls())

# Define sample size

# number of participants nested within country
num_subj <- 20
# number of countries
num_count <- 15

# simulate data for life satisfaction (LS), tightness-looseness(tight), ID
# country, and scores of trust (sim_trust)
LS <- matrix(0, num_subj * num_count, 1)
tight <- matrix(0, num_subj * num_count, 1)
ID <- matrix(0, num_subj * num_count, 1)
Country <- matrix(0, num_subj * num_count, 1)
sim_trust <- matrix(0, num_subj * num_count, 14)


id <- 0
for (c in 1:num_count) {
  
  for (s in 1:num_subj){
    id <- id + 1
    
    LS[(c - 1) * num_subj + s] <- runif(1,1,7)
    tight[(c - 1) * num_subj + s] <- runif(1,1,12)
    sim_trust[(c - 1) * num_subj + s,1:14] <- sample(c(1:7), 14, replace = T)
    ID[(c - 1) * num_subj + s] <- id
    Country[(c - 1) * num_subj + s] <- c
    
  }
}

sim_data <- cbind(ID, Country, LS, tight, sim_trust)
colnames(sim_data) <- c("ID", "Country", "LS", "tight",paste0("I",1:14))
sim_data <- as.data.frame(sim_data)

setwd('C:/RSA')
filename <- 'sim_data.txt'
write.table(sim_data, filename)
