# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 2 - Exploratory data analysis & visualization
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Apr 2021
# Date completed: 13 Apr 2021
# Date modified: 17 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(cowplot)
library(pdp)           # partial dependence plots

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

elk.data <- read.csv("elk_days_part.csv")

#_____________________________________________________________________________________________________________
# 3. Distribution of variables around parturition ----
#_____________________________________________________________________________________________________________

elk.data.summary <- elk.data %>% group_by(days.part) %>%
                                 summarize(sl.3day = mean(sl.3day, na.rm = TRUE),
                                           mcp = mean(mcp, na.rm = TRUE),
                                           sl.post7 = mean(sl.post7, na.rm = TRUE))

elk.data.summary.30 <- elk.data.summary %>% filter(days.part >= -30 & days.part <= 30)

# pivot_longer for facetting
elk.data.summary.30.long <- elk.data.summary.30 %>% pivot_longer(cols = c(2:4))

# graph it
ggplot(data = elk.data.summary.30.long, aes(x = days.part, y = value)) +
       geom_vline(xintercept = 0) +
       theme_bw() +
       facet_wrap(~name, scales = "free_y") +
       geom_point()

# correlation between variables
cor(x = elk.data.summary.30, method = "pearson")

# fit GAMS
ggplot(data = elk.data.summary.30.long, aes(x = days.part, y = value)) +
       geom_vline(xintercept = 0) +
       theme_bw() +
       facet_wrap(~name, scales = "free_y") +
       geom_point(alpha = 0.2) +
       stat_smooth(method = "gam") +
       scale_x_continuous(breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20))

#_____________________________________________________________________________________________________________
# 4. Look at individual variation ----
#_____________________________________________________________________________________________________________

# filter between +/-30 days
elk.data.30 <- elk.data %>% filter(days.part >= -30 & days.part <= 30)

# pivot_longer
elk.data.30.long <- elk.data.30 %>% pivot_longer(cols = c(4:6))

# rename variables
elk.data.30.long$name <- factor(elk.data.30.long$name,
                                levels = c("sl.3day", "mcp", "sl.post7"),
                                labels = c("Mean step length (3 day)",
                                           "Moving MCP (11 day)",
                                           "Mean step length (7 days post)"))

# facetted
ggplot(data = elk.data.30.long, aes(x = days.part, y = value)) +
       geom_vline(xintercept = 0, linetype = "dashed") +
       theme_bw() +
       facet_wrap(~name, scales = "free_y", nrow = 3) +
       geom_line(aes(group = CollarID), color = "lightgray") +
       stat_smooth(aes(color = name, fill = name), method = "gam") +
       scale_color_brewer(palette = "Set1") +
       scale_fill_brewer(palette = "Set1") +
       theme(legend.position = "none",
             panel.grid = element_blank()) +
       ylab("") +
       xlab("Days from parturition") +
       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# arranged plots
# sl.3day
elk.data.30.long.sl3day <- elk.data.30.long %>% filter(name == "Mean step length (3 day)")

plot.sl3day <- ggplot(data = elk.data.30.long.sl3day, aes(x = days.part, y = value)) +
                      geom_vline(xintercept = 0, linetype = "dashed") +
                      theme_bw() +
                      geom_line(aes(group = CollarID), color = "lightgray") +
                      stat_smooth(color = "darkgreen", fill = "darkgreen", method = "gam") +
                      theme(legend.position = "none",
                            panel.grid = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank()) +
                      ylab("Mean step length (3 day)") +
                      xlab("Days from parturition") +
                      scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# sl.post7
elk.data.30.long.slpost7 <- elk.data.30.long %>% filter(name == "Mean step length (7 days post)")

plot.slpost7 <- ggplot(data = elk.data.30.long.slpost7, aes(x = days.part, y = value)) +
                       geom_vline(xintercept = 0, linetype = "dashed") +
                       theme_bw() +
                       geom_line(aes(group = CollarID), color = "lightgray") +
                       stat_smooth(color = "red", fill = "red", method = "gam") +
                       theme(legend.position = "none",
                             panel.grid = element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_blank()) +
                       ylab("Mean step length (7 days post)") +
                       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# mcp
elk.data.30.long.mcp <- elk.data.30.long %>% filter(name == "Moving MCP (11 day)")

plot.mcp <- ggplot(data = elk.data.30.long.mcp, aes(x = days.part, y = value)) +
                       geom_vline(xintercept = 0, linetype = "dashed") +
                       theme_bw() +
                       geom_line(aes(group = CollarID), color = "lightgray") +
                       stat_smooth(color = "darkblue", fill = "darkblue", method = "gam") +
                       theme(legend.position = "none",
                             panel.grid = element_blank(),
                             axis.title.x = element_blank()) +
                       ylab("Moving MCP (11 day)") +
                       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))



dist.plot <- plot_grid(plot.sl3day, plot.slpost7, plot.mcp, nrow = 3)

#_____________________________________________________________________________________________________________
# 5. Absolute days from part ----
#_____________________________________________________________________________________________________________

ggplot(data = elk.data.30.long, aes(x = abs(days.part), y = value)) +
       theme_bw() +
       facet_wrap(~name, scales = "free_y", nrow = 3) +
       geom_point(aes(group = CollarID), color = "lightgray", alpha = 0.4) +
       stat_smooth(aes(color = name, fill = name), method = "lm") +
       scale_color_brewer(palette = "Set1") +
       scale_fill_brewer(palette = "Set1") +
       theme(legend.position = "none",
             panel.grid = element_blank()) +
       ylab("") +
       xlab("Days from parturition") +
       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

#_____________________________________________________________________________________________________________
# 6. Partial dependence plots ----
#_____________________________________________________________________________________________________________

# read in RF model
load(paste0(getwd(), "/elk_model.Rdata"))


# sl.3day
par.sl.3day <- partial(elk.model, pred.var = c("sl.3day"), which.class = 2)

pdp.sl.3day <- ggplot(data = par.sl.3day, aes(x = sl.3day, y = yhat)) +
                      theme_bw() +
                      geom_line(color = "darkgreen", size = 1) +
                      theme(legend.position = "none",
                            panel.grid = element_blank()) +
                      ylab("") +
                      xlab("Mean step length (3 day)")

# sl.post7
par.sl.post7 <- partial(elk.model, pred.var = c("sl.post7"), which.class = 2)

pdp.sl.post7 <- ggplot(data = par.sl.post7, aes(x = sl.post7, y = yhat)) +
                       theme_bw() +
                       geom_line(color = "red", size = 1) +
                       theme(legend.position = "none",
                             panel.grid = element_blank()) +
                       xlab("Mean step length (7 days post)") +
                       ylab(expression(Y["hat"])) +
                       scale_x_continuous(breaks = seq(0, 1500, 250))

# mcp
par.mcp <- partial(elk.model, pred.var = c("mcp"), which.class = 2)

pdp.mcp <- ggplot(data = par.mcp, aes(x = mcp, y = yhat)) +
                  theme_bw() +
                  geom_line(color = "darkblue", size = 1) +
                  theme(legend.position = "none",
                        panel.grid = element_blank()) +
                  xlab("Moving MCP (11 day)") +
                  ylab("") + 
                  scale_x_continuous(breaks = seq(0, 15, 3))
       
partial.plot <- plot_grid(pdp.sl.3day, pdp.sl.post7, pdp.mcp, nrow = 3)

partial.plot

plot_grid(dist.plot, partial.plot)
