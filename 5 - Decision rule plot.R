# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 5 - Determine optimal decision rule and plot
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 2 Aug 2021
# Date completed: 2 Aug 2021
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(cowplot)       # arrange plots

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

rules <- read.csv("Decision rules.csv") 

str(rules)

rules$days <- factor(rules$days)

# define palette
plot.palette <- c("#FF3300", "#009900", "#000099")

#_____________________________________________________________________________________________________________
# 3. Plot of all ----
#_____________________________________________________________________________________________________________

rules.all <- rules %>% filter(type == "all")

plot.all <- ggplot() +
            geom_vline(xintercept = 0.7, size = 2, color = "lightgray") +
            geom_hline(yintercept = max(rules.all$correct)) +
            theme_bw() +
            geom_line(data = rules.all, aes(x = threshold, y = correct, linetype = days, color = days), size = 1.5) +
            xlab("Probability threshold") +
            ylab("Proportion of individuals correctly classified") +
            theme(panel.grid = element_blank(),
                  legend.position = c(0.15, 0.3),
                  axis.title = element_blank(),
                  legend.background = element_rect(colour = 'black', fill = 'white', linetype = 'solid')) +
            scale_x_continuous(breaks = seq(0.4, 0.95, 0.05)) +
            scale_color_manual(values = plot.palette)
       
print(plot.all)

#_____________________________________________________________________________________________________________
# 4. Plot of p ----
#_____________________________________________________________________________________________________________

rules.p <- rules %>% filter(type == "p")

plot.p <- ggplot() +
            geom_vline(xintercept = 0.7, size = 2, color = "lightgray") +
            geom_hline(yintercept = max(rules.p$correct)) +
            theme_bw() +
            geom_line(data = rules.p, aes(x = threshold, y = correct, linetype = days, color = days), size = 1.5) +
            xlab("Probability threshold") +
            ylab("Proportion of individuals correctly classified") +
            theme(panel.grid = element_blank(),
                  legend.position = "none",
                  axis.title = element_blank()) +
            scale_x_continuous(breaks = seq(0.4, 0.95, 0.1)) +
            scale_y_continuous(breaks = seq(0, 0.8, 0.2)) +
            scale_color_manual(values = plot.palette)
       
print(plot.p)

#_____________________________________________________________________________________________________________
# 5. Plot of np ----
#_____________________________________________________________________________________________________________

rules.np <- rules %>% filter(type == "np")

plot.np <- ggplot() +
           geom_vline(xintercept = 0.7, size = 2, color = "lightgray") +
           geom_hline(yintercept = max(rules.np$correct)) +
           theme_bw() +
           geom_line(data = rules.np, aes(x = threshold, y = correct, linetype = days, color = days), size = 1.5) +
           xlab("Probability threshold") +
           ylab("") +
           theme(panel.grid = element_blank(),
                 legend.position = "none",
                 axis.title = element_blank()) +
           scale_x_continuous(breaks = seq(0.4, 0.95, 0.1)) +
           scale_y_continuous(breaks = seq(0.5, 1.0, 0.1)) +
           scale_color_manual(values = plot.palette)
       
print(plot.np)

#_____________________________________________________________________________________________________________
# 6. Arrange plots ----
#_____________________________________________________________________________________________________________

bottom.panel <- plot_grid(plot.p, plot.np)

plot_grid(plot.all, bottom.panel, rel_widths = c(2, 1, 1), nrow = 2)

