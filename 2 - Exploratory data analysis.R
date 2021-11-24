# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 2 - Exploratory data analysis & visualization
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Apr 2021
# Date completed: 13 Apr 2021
# Date modified: 10 Sep 2021
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
                                           sl.post7 = mean(sl.post7, na.rm = TRUE),
                                           canopy.5day = mean(canopy.5day, na.rm = TRUE),
                                           TRI.5day = mean(TRI.5day, na.rm = TRUE),
                                           dEdge.5day = mean(dEdge.5day, na.rm = TRUE))

elk.data.summary.30 <- elk.data.summary %>% filter(days.part >= -30 & days.part <= 30)

# pivot_longer for facetting
elk.data.summary.30.long <- elk.data.summary.30 %>% pivot_longer(cols = c(2:7))

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
elk.data.30.long <- elk.data.30 %>% pivot_longer(cols = c(4:9))

# rename variables
elk.data.30.long$name <- factor(elk.data.30.long$name,
                                levels = c("sl.3day", "mcp", "sl.post7", "canopy.5day", "TRI.5day", "dEdge.5day"),
                                labels = c("Mean step length (3 day)",
                                           "Moving MCP (11 day)",
                                           "Mean step length (7 days post)",
                                           "% Canopy Cover",
                                           "Terrain ruggedness index",
                                           "Distance from edge"))

# facetted
ggplot(data = elk.data.30.long, aes(x = days.part, y = value)) +
       geom_vline(xintercept = 0, linetype = "dashed") +
       theme_bw() +
       facet_wrap(~name, scales = "free_y", nrow = 6) +
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
                      theme_bw() +
                      geom_line(aes(group = CollarID), color = "lightgray", size = 0.5) +
                      stat_smooth(color = "darkgreen", fill = "darkgreen", method = "gam") +
                      geom_vline(xintercept = 0, linetype = "dashed") +
                      theme(legend.position = "none",
                            panel.grid = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank()) +
                      ylab("sl.3day") +
                      xlab("Days from parturition") +
                      scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# sl.post7
elk.data.30.long.slpost7 <- elk.data.30.long %>% filter(name == "Mean step length (7 days post)")

plot.slpost7 <- ggplot(data = elk.data.30.long.slpost7, aes(x = days.part, y = value)) +
                       theme_bw() +
                       geom_line(aes(group = CollarID), color = "lightgray", size = 0.5) +
                       stat_smooth(color = "red", fill = "red", method = "gam") +
                       geom_vline(xintercept = 0, linetype = "dashed") +
                       theme(legend.position = "none",
                             panel.grid = element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_blank()) +
                       ylab("sl.post7") +
                       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# mcp
elk.data.30.long.mcp <- elk.data.30.long %>% filter(name == "Moving MCP (11 day)")

plot.mcp <- ggplot(data = elk.data.30.long.mcp, aes(x = days.part, y = value)) +
                       theme_bw() +
                       geom_line(aes(group = CollarID), color = "lightgray", size = 0.5) +
                       stat_smooth(color = "darkblue", fill = "darkblue", method = "gam") +
                       geom_vline(xintercept = 0, linetype = "dashed") +
                       theme(legend.position = "none",
                             panel.grid = element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_blank()) +
                       ylab("mcp") +
                       scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30))

# canopy
elk.data.30.long.canopy <- elk.data.30.long %>% filter(name == "% Canopy Cover")

plot.canopy <- ggplot(data = elk.data.30.long.canopy, aes(x = days.part, y = value)) +
               theme_bw() +
               geom_line(aes(group = CollarID), color = "lightgray", size = 0.5) +
               stat_smooth(color = "khaki4", fill = "khaki4", method = "gam") +
               geom_vline(xintercept = 0, linetype = "dashed") +
               theme(legend.position = "none",
                     panel.grid = element_blank(),
                     axis.title.x = element_blank()) +
               ylab("canopy.5day") +
               xlab("Days from parturition") +
               scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
               scale_y_continuous(breaks = seq(0, 100, 25)) +
               coord_cartesian(ylim = c(0, 100))

dist.plot <- plot_grid(plot.sl3day, plot.slpost7, plot.mcp, plot.canopy, nrow = 4)

dist.plot

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
                      xlab("sl.3day") +
                      scale_y_continuous(position = "right")

# sl.post7
par.sl.post7 <- partial(elk.model, pred.var = c("sl.post7"), which.class = 2)

pdp.sl.post7 <- ggplot(data = par.sl.post7, aes(x = sl.post7, y = yhat)) +
                       theme_bw() +
                       geom_line(color = "red", size = 1) +
                       theme(legend.position = "none",
                             panel.grid = element_blank()) +
                       ylab("") +
                       xlab("sl.post7") +
                       scale_x_continuous(breaks = seq(0, 1500, 250)) +
                       scale_y_continuous(position = "right")

# mcp
par.mcp <- partial(elk.model, pred.var = c("mcp"), which.class = 2)

pdp.mcp <- ggplot(data = par.mcp, aes(x = mcp, y = yhat)) +
                  theme_bw() +
                  geom_line(color = "darkblue", size = 1) +
                  theme(legend.position = "none",
                        panel.grid = element_blank()) +
                  xlab("mcp") +
                  ylab("") + 
                  scale_x_continuous(breaks = seq(0, 15, 3)) +
                  scale_y_continuous(position = "right")

# canopy.5day
par.canopy <- partial(elk.model, pred.var = c("canopy.5day"), which.class = 2)

pdp.canopy <- ggplot(data = par.canopy, aes(x = canopy.5day, y = yhat)) +
        theme_bw() +
        geom_line(color = "khaki4", size = 1) +
        theme(legend.position = "none",
              panel.grid = element_blank()) +
        xlab("canopy.5day") +
        ylab("") + 
        scale_x_continuous(breaks = seq(0, 100, 25)) +
        scale_y_continuous(position = "right")
       
partial.plot <- plot_grid(pdp.sl.3day, pdp.sl.post7, pdp.mcp, pdp.canopy, nrow = 4)

partial.plot

plot_grid(dist.plot, partial.plot)

#_____________________________________________________________________________________________________________
# 7. Extra plot to illustrate pre-parturition movements ----
#_____________________________________________________________________________________________________________

# GAM
plot.prepart <- ggplot(data = elk.data.30.long.sl3day, aes(x = days.part, y = value)) +
                theme_bw() +
                geom_line(aes(group = CollarID), color = "lightgray") +
                stat_smooth(color = "darkgreen", fill = "darkgreen", method = "gam") +
                geom_vline(xintercept = 0, linetype = "dashed") +
                theme(legend.position = "none",
                      panel.grid = element_blank()) +
                ylab("sl.3day") +
                xlab("Days from parturition") +
                coord_cartesian(xlim = c(-20, 5))

plot.prepart

# boxplots
plot.prepart <- ggplot(data = elk.data.30.long.sl3day, aes(x = days.part, y = value)) +
                theme_bw() +
                geom_point(color = "darkgreen", alpha = 0.25) +
                geom_vline(xintercept = 0, 
                           linetype = "dashed") +
                theme(legend.position = "none",
                      panel.grid = element_blank()) +
                ylab("sl.3day") +
                xlab("Days from parturition") +
                coord_cartesian(xlim = c(-20, 5)) +
                geom_segment(aes(x = -21.5, 
                                 y = mean(value[days.part == -21]), 
                                 xend = -20.5, 
                                 yend = mean(value[days.part == -21])),
                                 size = 1.25) +
                geom_segment(aes(x = -20.5, 
                                 y = mean(value[days.part == -20]), 
                                 xend = -19.5, 
                                 yend = mean(value[days.part == -20])),
                                 size = 1.25) +
                geom_segment(aes(x = -19.5, 
                                 y = mean(value[days.part == -19]), 
                                 xend = -18.5, 
                                 yend = mean(value[days.part == -19])),
                                 size = 1.25) +
                geom_segment(aes(x = -18.5, 
                                 y = mean(value[days.part == -18]), 
                                 xend = -17.5, 
                                 yend = mean(value[days.part == -18])),
                                 size = 1.25) +
                geom_segment(aes(x = -17.5, 
                                 y = mean(value[days.part == -17]), 
                                 xend = -16.5, 
                                 yend = mean(value[days.part == -17])),
                                 size = 1.25) +
                geom_segment(aes(x = -16.5, 
                                 y = mean(value[days.part == -16]), 
                                 xend = -15.5, 
                                 yend = mean(value[days.part == -16])),
                                 size = 1.25) +
                geom_segment(aes(x = -15.5, 
                                 y = mean(value[days.part == -15]), 
                                 xend = -14.5, 
                                 yend = mean(value[days.part == -15])),
                                 size = 1.25) +
                geom_segment(aes(x = -14.5, 
                                 y = mean(value[days.part == -14]), 
                                 xend = -13.5, 
                                 yend = mean(value[days.part == -14])),
                                 size = 1.25) +
                geom_segment(aes(x = -13.5, 
                                 y = mean(value[days.part == -13]), 
                                 xend = -12.5, 
                                 yend = mean(value[days.part == -13])),
                                 size = 1.25) +
                geom_segment(aes(x = -12.5, 
                                 y = mean(value[days.part == -12]), 
                                 xend = -11.5, 
                                 yend = mean(value[days.part == -12])),
                                 size = 1.25) +
                geom_segment(aes(x = -11.5, 
                                 y = mean(value[days.part == -11]), 
                                 xend = -10.5, 
                                 yend = mean(value[days.part == -11])),
                                 size = 1.25) +
                geom_segment(aes(x = -10.5, 
                                 y = mean(value[days.part == -10]), 
                                 xend = -9.5, 
                                 yend = mean(value[days.part == -10])),
                                 size = 1.25) +
                geom_segment(aes(x = -9.5, 
                                 y = mean(value[days.part == -9]), 
                                 xend = -8.5, 
                                 yend = mean(value[days.part == -9])),
                                 size = 1.25) +
                geom_segment(aes(x = -8.5, 
                                 y = mean(value[days.part == -8]), 
                                 xend = -7.5, 
                                 yend = mean(value[days.part == -8])),
                                 size = 1.25) +
                geom_segment(aes(x = -7.5, 
                                 y = mean(value[days.part == -7]), 
                                 xend = -6.5, 
                                 yend = mean(value[days.part == -7])),
                                 size = 1.25) +
                geom_segment(aes(x = -6.5, 
                                 y = mean(value[days.part == -6]), 
                                 xend = -5.5, 
                                 yend = mean(value[days.part == -6])),
                                 size = 1.25) +
                geom_segment(aes(x = -5.5, 
                                 y = mean(value[days.part == -5]), 
                                 xend = -4.5, 
                                 yend = mean(value[days.part == -5])),
                                 size = 1.25) +
                geom_segment(aes(x = -4.5, 
                                 y = mean(value[days.part == -4]), 
                                 xend = -3.5, 
                                 yend = mean(value[days.part == -4])),
                                 size = 1.25) +
                geom_segment(aes(x = -3.5, 
                                 y = mean(value[days.part == -3]), 
                                 xend = -2.5, 
                                 yend = mean(value[days.part == -3])),
                                 size = 1.25) +
                geom_segment(aes(x = -2.5, 
                                 y = mean(value[days.part == -2]), 
                                 xend = -1.5, 
                                 yend = mean(value[days.part == -2])),
                                 size = 1.25) +
                geom_segment(aes(x = -1.5, 
                                 y = mean(value[days.part == -1]), 
                                 xend = -0.5, 
                                 yend = mean(value[days.part == -1])),
                                 size = 1.25) +
                geom_segment(aes(x = -0.5, 
                                 y = mean(value[days.part == 0], na.rm = TRUE), 
                                 xend = 0.5, 
                                 yend = mean(value[days.part == 0], na.rm = TRUE)),
                                 size = 1.25) +
                geom_segment(aes(x = 0.5, 
                                 y = mean(value[days.part == 1]), 
                                 xend = 1.5, 
                                 yend = mean(value[days.part == 1])),
                                 size = 1.25) +
                geom_segment(aes(x = 1.5, 
                                 y = mean(value[days.part == 2]), 
                                 xend = 2.5, 
                                 yend = mean(value[days.part == 2])),
                                 size = 1.25) +
                geom_segment(aes(x = 2.5, 
                                 y = mean(value[days.part == 3]), 
                                 xend = 3.5, 
                                 yend = mean(value[days.part == 3])),
                                 size = 1.25) +
                geom_segment(aes(x = 3.5, 
                                 y = mean(value[days.part == 4]), 
                                 xend = 4.5, 
                                 yend = mean(value[days.part == 4])),
                                 size = 1.25) +
                geom_segment(aes(x = 4.5, 
                                 y = mean(value[days.part == 5]), 
                                 xend = 5.5, 
                                 yend = mean(value[days.part == 5])),
                                 size = 1.25) +
                geom_segment(aes(x = 5.5, 
                                 y = mean(value[days.part == 6]), 
                                 xend = 6.5, 
                                 yend = mean(value[days.part == 6])),
                                 size = 1.25)

plot.prepart

mean(elk.data.30.long.sl3day$value[elk.data.30.long.sl3day$days.part == -10])

sl.summary.table <- elk.data.30.long.sl3day %>% group_by(days.part) %>%
                                                summarize(mean = mean(value, na.rm = TRUE),
                                                          se = sd(value, na.rm = TRUE) / sqrt(n()),
                                                          max = max(value, na.rm = TRUE),
                                                          min = min(value, na.rm = TRUE))

write.table(sl.summary.table, "clipboard", sep = "\t")
