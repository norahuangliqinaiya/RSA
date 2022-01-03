rm(list=ls())

# set path
setwd("C:/RSA")
library(misty)
library("dplyr")

# =================================================
# National sim matrix all
# Read data organized as the demo file
my_data <- as.data.frame(read.table('sim_data.txt'))

# Figures 2A
sim_trust <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4)]))
reorder_names <- paste0("I", 1:14)

gplots <- matrix(0,1,15)

library(ggplot2)
library(scales)

# Plot the similarity matrix of trust profile for each country

for (i in (1: length(sim_trust))){
  
  sim_trust_new <- reshape2::melt(sim_trust[[i]], c("x", "y"), value.name = "sim_trust")
  sim_trust_new$y <- factor(sim_trust_new$y, levels = rev(reorder_names))
  
  
  gplots[i] <- list(ggplot(data =sim_trust_new, aes(x = x, y = y, fill = sim_trust)) +
                      geom_tile(color = "white") +
                      scale_x_discrete(expand = c(0,0),
                                       limits = reorder_names) +
                      scale_y_discrete(expand = c(0.06,0),
                                       limits = rev(reorder_names)) +
                      scale_fill_gradientn(colours = c("#097CFF", "#81BDFF", "white", "#FF8989","#FF0000"),
                                           guide = "colorbar") + 
                      
                      coord_equal() +
                      theme(axis.title = element_blank(),
                            axis.text.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.border = element_blank(),
                            panel.background = element_blank(),
                            legend.title = element_blank()))
}

library(ggpubr)
ggarrange(gplots[[1]]+rremove("legend"),
          gplots[[2]]+rremove("legend"),
          gplots[[3]]+rremove("legend"),
          gplots[[4]]+rremove("legend"),
          gplots[[5]]+rremove("legend"),
          gplots[[6]]+rremove("legend"),
          gplots[[7]]+rremove("legend"),
          gplots[[8]]+rremove("legend"),
          gplots[[9]]+rremove("legend"),
          gplots[[10]]+rremove("legend"),
          gplots[[11]]+rremove("legend"),
          gplots[[12]]+rremove("legend"),
          gplots[[13]]+rremove("legend"),
          gplots[[14]]+rremove("legend"),
          gplots[[15]]+rremove("legend"),
          ncol = 5, nrow = 3)


# Figures 2A
# Tightness-looseness
setwd("C:/RSA")
my_data <- read.sav("tight.sav", as.data.frame = T)
sim_tight <- (max(abs(dist(my_data[,-1]))) - abs(dist(my_data[,-1])))/max(abs(dist(my_data[,-1])))
sim_tight <- as.matrix(sim_tight)

diag(sim_tight) <- 1

sim_tight[which(sim_tight <= 0.4)] <- 0.4

names <- c("Brazil", "China", "Estonia",
           "Germany", "India", "Italy",
           "Japan", "Korea", "New Zealand",
           "Poland", "Spain", "Turkey",
           "UK", "Ukraine", "USA")

colnames(sim_tight) <- names
rownames(sim_tight) <- names

reorder_names <- c("India", "Turkey", "Japan", "Korea",
                   "China", "Italy", "Poland", "Germany",
                   "Spain", "UK", "USA", "New Zealand",
                   "Estonia", "Ukraine", "Brazil")

sim_tight_new <- reshape2::melt(sim_tight, c("x", "y"), value.name = "sim_tight")
sim_tight_new$y <- factor(sim_tight_new$y, levels = rev(reorder_names))


ggplot(data =sim_tight_new, aes(x = x, y = y, fill = sim_tight)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0,0),
                   limits = reorder_names) +
  scale_y_discrete(expand = c(0.06,0),
                   limits = rev(reorder_names)) +
  scale_fill_gradientn(colours = c("#097CFF", "#81BDFF", "white", "#FF8989","#FF0000"),
                       guide = "colorbar",
                       values = rescale(c(0.4,0.5,0.7,0.9,1)),
                       limits = c(0.4, 1),
                       breaks = seq(0.4,1,0.2)) + 
  coord_equal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())

# =================================================
# Figures 2B
# National trust profile (all)
sim_trust_country <- matrix(0, 91, 15)

correlations <- lapply(split(my_data, my_data$Country), function(x) cor(x[,-c(1:4)]))

for (i in (1: length(correlations))){
  
  sim_trust_country[,i] <- correlations[[i]][lower.tri(correlations[[i]], diag = FALSE)]

}


# National profile of trust
# Fisher r to z transformation
library(DescTools)
sim_trust_country_r2z_all <- FisherZ(sim_trust_country)
sim_trust_nations_all <- cor(sim_trust_country_r2z_all)

names <- c("Brazil", "China", "Estonia",
           "Germany", "India", "Italy",
           "Japan", "Korea", "New Zealand",
           "Poland", "Spain", "Turkey",
           "UK", "Ukraine", "USA")

colnames(sim_trust_nations_all) <- names
rownames(sim_trust_nations_all) <- names

reorder_names <- c("India", "Turkey", "Japan", "Korea",
                   "China", "Italy", "Poland", "Germany",
                   "Spain", "UK", "USA", "New Zealand",
                   "Estonia", "Ukraine", "Brazil")

trust_all_new <- reshape2::melt(sim_trust_nations_all, c("x", "y"), value.name = "sim_trust_all")
trust_all_new$y <- factor(trust_all_new$y, levels = rev(reorder_names))


ggplot(data =trust_all_new, aes(x = x, y = y, fill = sim_trust_all)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0,0),
                   limits = reorder_names) +
  scale_y_discrete(expand = c(0.06,0),
                   limits = rev(reorder_names)) +
  scale_fill_gradientn(colours = c("#097CFF", "#81BDFF", "white", "#FF8989","#FF0000"),
                       guide = "colorbar") + 
  coord_equal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())


# Theoretical model 1
theor_v <- c(rep(c(rep(1,7),rep(0,7)),7),
             rep(0,7),c(1,rep(0,6)),
             rep(0,7),c(0,1,rep(0,5)),
             rep(0,7),c(0,0,1,rep(0,4)),
             rep(0,7),c(0,0,0,1,rep(0,3)),
             rep(0,7),c(0,0,0,0,1,rep(0,2)),
             rep(0,7),c(0,0,0,0,0,1,0),
             rep(0,7),c(0,0,0,0,0,0,1))

theor_M <- matrix(theor_v, nrow = 14, ncol = 14)


names <- paste("I",1:14,sep = "")
colnames(theor_M) <- names
rownames(theor_M) <- names

trust_theor_sim_new <- reshape2::melt(theor_M, c("x", "y"), value.name = "trust_theor_sim")
trust_theor_sim_new$y <- factor(trust_theor_sim_new$y, levels = rev(names))


ggplot(data =trust_theor_sim_new, aes(x = x, y = y, fill = trust_theor_sim)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0,0),
                   limits = names) +
  scale_y_discrete(expand = c(0.06,0),
                   limits = rev(names)) +
  scale_fill_gradientn(colours = c("#097CFF", "white", "#FF0000"),
                       guide = "colorbar",
                       limits = c(0, 1),
                       breaks = seq(0,1,0.2)) + 
  coord_equal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())


# Theoretical model 2
theor_v_2 <- c(rep(c(rep(1,7),rep(0,7)),7),
               rep(c(rep(0,7),rep(1,7)),7))

theor_M_2 <- matrix(theor_v_2, nrow = 14, ncol = 14, byrow = T)
names <- paste("I",1:14,sep = "")
colnames(theor_M_2) <- names
rownames(theor_M_2) <- names

trust_theor_sim_new <- reshape2::melt(theor_M_2, c("x", "y"), value.name = "trust_theor_sim")
trust_theor_sim_new$y <- factor(trust_theor_sim_new$y, levels = rev(names))


ggplot(data =trust_theor_sim_new, aes(x = x, y = y, fill = trust_theor_sim)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0,0),
                   limits = names) +
  scale_y_discrete(expand = c(0.06,0),
                   limits = rev(names)) +
  scale_fill_gradientn(colours = c("#097CFF", "white", "#FF0000"),
                       guide = "colorbar",
                       limits = c(0, 1),
                       breaks = seq(0,1,0.2)) + 
  coord_equal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())


# Theoretical model 3

theor_v_3 <- c(c(1,rep(0,13)),
               c(0,1,rep(0,12)),
               c(0,0,1,rep(0,11)),
               c(0,0,0,1,rep(0,10)),
               c(0,0,0,0,1,rep(0,9)),
               c(0,0,0,0,0,1,rep(0,8)),
               c(0,0,0,0,0,0,1,rep(0,7)),
               rep(c(rep(0,7),rep(1,7)),7))

theor_M_3 <- matrix(theor_v_3, nrow = 14, ncol = 14)
names <- paste("I",1:14,sep = "")
colnames(theor_M_3) <- names
rownames(theor_M_3) <- names

trust_theor_sim_new <- reshape2::melt(theor_M_3, c("x", "y"), value.name = "trust_theor_sim")
trust_theor_sim_new$y <- factor(trust_theor_sim_new$y, levels = rev(names))


ggplot(data =trust_theor_sim_new, aes(x = x, y = y, fill = trust_theor_sim)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0,0),
                   limits = names) +
  scale_y_discrete(expand = c(0.06,0),
                   limits = rev(names)) +
  scale_fill_gradientn(colours = c("#097CFF", "white", "#FF0000"),
                       guide = "colorbar",
                       limits = c(0, 1),
                       breaks = seq(0,1,0.2)) + 
  coord_equal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())

