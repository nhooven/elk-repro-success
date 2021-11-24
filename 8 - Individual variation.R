# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 8 - Individual variation
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 17 Aug 2021
# Date completed: 17 Aug 2021
# Date modified: 23 Nov 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mefa4)        # %notin%
library(ggridges)     # ridgeline plots
library(cowplot)      # arrange multiple plots
library(rptR)         # repeatability
library(randomForest)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# parturient elk
elk.part <- read.csv("elk_days_part.csv")

# remove 37718 and 45510
elk.part <- elk.part %>% filter(CollarID %notin% c(37718, 45510))

# non-pregnant/lost calf/outside of window
elk.np <- read.csv("elk_np.csv")

# unknowns 
elk.unknowns <- read.csv("elk_unknowns.csv")

# 2020 collars in 2021
elk.thisyear <- read.csv("elk_thisyear_2021.csv")

#_____________________________________________________________________________________________________________
# 3. Select only columns we need and add in dataset and year column ----
#_____________________________________________________________________________________________________________

# Training set
elk.births.2020 <- as.vector(c(37705, 37706, 37708, 37709, 37710, 37711, 37712, 
                               37714, 37715, 37716, 37717, 37718, 37719, 37720, 
                               37722, 37723, 37724, 37725, 37726, 37727))


elk.part.1 <- elk.part %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp, canopy.5day) %>%
                                  mutate(Year = ifelse(CollarID %in% elk.births.2020,
                                                       2020,
                                                       2021),
                                         Group = "Training")

# Testing set - Group A (non-parturient/calf loss)
elk.np.2020 <- as.vector(c(37704, 102489, 102491, 102497, 103172, 103174, 103179, 103181, 103182))


elk.np.1 <- elk.np %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp, canopy.5day) %>%
                              mutate(Year = ifelse(CollarID %in% elk.np.2020,
                                                   2020,
                                                   2021),
                                     Group = "A")

# Testing set - Group B (unknowns)
elk.unk.2020 <- as.vector(c(37703, 37707, 101940, 101968, 101969, 101978, 102492, 102493, 102495, 102536,
                            103176, 103177, 103178, 103183, 103249))


elk.unknowns.1 <- elk.unknowns %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp, canopy.5day) %>%
                                          mutate(Year = ifelse(CollarID %in% elk.unk.2020,
                                                               2020,
                                                               2021),
                                                 Group = "B")

# Testing set - Group C (2020 collars in 2021)
elk.thisyear.1 <- elk.thisyear %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp, canopy.5day) %>%
                                          mutate(Year = 2021, 
                                                 Group = "C")

#_____________________________________________________________________________________________________________
# 4. Add a column of predicted status ----
#_____________________________________________________________________________________________________________

# Training set
elk.part.yes <- as.vector(c(37705, 37706, 37708, 37710, 37711, 37712, 37714, 37715, 37716, 37717, 37719, 37720,
                            37722, 37723, 37724, 37725, 37726, 45492, 45493, 45494, 45495, 45496, 45498, 45500,
                            45501, 45505, 45507, 45509, 45511, 46393, 46394, 46397, 46401, 46402))

elk.part.1 <- elk.part.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.part.yes,
                                                         1,
                                                         0))

# Testing set - Group A (non-parturient/calf loss)
elk.np.yes <- as.vector(c(103182, 37704, 45502))

elk.np.1 <- elk.np.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.np.yes,
                                                     1,
                                                     0))

# Testing set - Group B (unknowns)
elk.unknowns.yes <- as.vector(c(37718, 101978, 102492, 102536, 103173, 103177, 103183, 103186, 103249))

elk.unknowns.1 <- elk.unknowns.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.unknowns.yes,
                                            1,
                                            0))

# Testing set - Group C (2020 collars in 2021)
elk.thisyear.yes <- as.vector(c(37703, 37704, 37705, 37708, 37709, 37710, 37711, 37714, 37716, 37718,
                                37722, 37726))

elk.thisyear.1 <- elk.thisyear.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.thisyear.yes,
                                            1,
                                            0))

#_____________________________________________________________________________________________________________
# 5. Bind together and remove collars with few days of prediction ----
#_____________________________________________________________________________________________________________

all.elk <- rbind(elk.part.1, elk.np.1, elk.unknowns.1, elk.thisyear.1)

all.elk <- all.elk %>% filter(CollarID %notin% c(103185, 103244, 103250, 45506, 101940, 101968, 103251))

#_____________________________________________________________________________________________________________
# 6. Reorder factor levels ----
#_____________________________________________________________________________________________________________

all.elk$Group <- factor(all.elk$Group, 
                        levels = c("Training", "A", "B", "C"))

#_____________________________________________________________________________________________________________
# 7. Ridgeline plot ----
#_____________________________________________________________________________________________________________

# sl.day
ggplot(data = all.elk, aes(x = sl.3day, y = as.factor(CollarID))) +
       theme_bw() +
       facet_wrap(~Group, nrow = 4, scales = "free_y") +
       geom_density_ridges(aes(fill = as.factor(Pred.status))) +
       theme(legend.position = "none") +
       coord_cartesian(xlim = c(0, 2000))

#_____________________________________________________________________________________________________________
# 7. Point plot - by variable ----
#_____________________________________________________________________________________________________________

all.elk.long <- all.elk %>% pivot_longer(cols = c(2:4))

ggplot(data = all.elk.long, aes(x = value, y = as.factor(CollarID))) +
       theme_bw() +
       facet_grid(rows = vars(Group), cols = vars(name), scales = "free") +
       geom_point(aes(color = as.factor(Pred.status)),
                  alpha = 0.2) +
       geom_boxplot(outlier.shape = NA) +
       theme(axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             legend.position = "none") +
       ylab("Individual animal") +
       scale_color_manual(values = c("red", "blue"))

#_____________________________________________________________________________________________________________
# 7a. Ridgeline plot - training set only ----
#_____________________________________________________________________________________________________________

# sl.3day
elk.part.1$CollarID.fct.sl3day <- fct_reorder(as.factor(elk.part.1$CollarID), elk.part.1$sl.3day, mean)

ggplot(data = elk.part.1, 
       aes(x = sl.3day, y = CollarID.fct.sl3day)) +
       theme_bw() +
       geom_density_ridges(aes(fill = as.factor(Pred.status))) +
       theme(legend.position = "none") +
       coord_cartesian(xlim = c(0, 2000))

# sl.post7
elk.part.1$CollarID.fct.slpost7 <- fct_reorder(as.factor(elk.part.1$CollarID), elk.part.1$sl.post7, mean)

ggplot(data = elk.part.1, 
       aes(x = sl.post7, y = CollarID.fct.slpost7)) +
       theme_bw() +
       geom_density_ridges(aes(fill = as.factor(Pred.status))) +
       theme(legend.position = "none") +
       coord_cartesian(xlim = c(0, 1500))

# mcp
elk.part.1$CollarID.fct.mcp <- fct_reorder(as.factor(elk.part.1$CollarID), elk.part.1$mcp, mean)

ggplot(data = elk.part.1, 
       aes(x = mcp, y = CollarID.fct.mcp)) +
       theme_bw() +
       geom_density_ridges(aes(fill = as.factor(Pred.status))) +
       theme(legend.position = "none") +
       coord_cartesian(xlim = c(0, 10))

#_____________________________________________________________________________________________________________
# 8. Summarize based on mean, median, and sd ----
#_____________________________________________________________________________________________________________
# 8a. Training set ----
#_____________________________________________________________________________________________________________

elk.part.1.summary <- elk.part.1 %>% group_by(CollarID) %>% 
                                     summarize(mean.sl.3day = mean(sl.3day, na.rm = TRUE),
                                               mean.sl.post7 = mean(sl.post7, na.rm = TRUE),
                                               mean.mcp = mean(mcp, na.rm = TRUE),
                                               median.sl.3day = median(sl.3day, na.rm = TRUE),
                                               median.sl.post7 = median(sl.post7, na.rm = TRUE),
                                               median.mcp = median(mcp, na.rm = TRUE),
                                               sd.sl.3day = sd(sl.3day, na.rm = TRUE),
                                               sd.sl.post7 = sd(sl.post7, na.rm = TRUE),
                                               sd.mcp = sd(mcp, na.rm = TRUE),
                                               Pred.status = median(Pred.status, na.rm = TRUE)) %>%
                                     mutate(Group = "Training")

# mean
ggplot(data = elk.part.1.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# median
ggplot(data = elk.part.1.summary, aes(x = median.sl.3day, y = median.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# sd
ggplot(data = elk.part.1.summary, aes(x = sd.sl.3day, y = sd.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# Pearson's correlation between mean.sl.3day and mean.mcp
cor.test(elk.part.1.summary$mean.sl.3day, elk.part.1.summary$mean.mcp)

#_____________________________________________________________________________________________________________
# 8b. Testing set - Group A ----
#_____________________________________________________________________________________________________________

elk.np.1.summary <- elk.np.1 %>% group_by(CollarID) %>% 
                                     summarize(mean.sl.3day = mean(sl.3day, na.rm = TRUE),
                                               mean.sl.post7 = mean(sl.post7, na.rm = TRUE),
                                               mean.mcp = mean(mcp, na.rm = TRUE),
                                               median.sl.3day = median(sl.3day, na.rm = TRUE),
                                               median.sl.post7 = median(sl.post7, na.rm = TRUE),
                                               median.mcp = median(mcp, na.rm = TRUE),
                                               sd.sl.3day = sd(sl.3day, na.rm = TRUE),
                                               sd.sl.post7 = sd(sl.post7, na.rm = TRUE),
                                               sd.mcp = sd(mcp, na.rm = TRUE),
                                               Pred.status = median(Pred.status, na.rm = TRUE)) %>%
                                     mutate(Group = "A")

# mean
ggplot(data = elk.np.1.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# median
ggplot(data = elk.np.1.summary, aes(x = median.sl.3day, y = median.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# sd
ggplot(data = elk.np.1.summary, aes(x = sd.sl.3day, y = sd.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# Pearson's correlation between mean.sl.3day and mean.mcp
cor.test(elk.np.1.summary$mean.sl.3day, elk.np.1.summary$mean.mcp)

#_____________________________________________________________________________________________________________
# 8c. Testing set - Group B ----
#_____________________________________________________________________________________________________________

elk.unknowns.1.summary <- elk.unknowns.1 %>% group_by(CollarID) %>% 
                                     summarize(mean.sl.3day = mean(sl.3day, na.rm = TRUE),
                                               mean.sl.post7 = mean(sl.post7, na.rm = TRUE),
                                               mean.mcp = mean(mcp, na.rm = TRUE),
                                               median.sl.3day = median(sl.3day, na.rm = TRUE),
                                               median.sl.post7 = median(sl.post7, na.rm = TRUE),
                                               median.mcp = median(mcp, na.rm = TRUE),
                                               sd.sl.3day = sd(sl.3day, na.rm = TRUE),
                                               sd.sl.post7 = sd(sl.post7, na.rm = TRUE),
                                               sd.mcp = sd(mcp, na.rm = TRUE),
                                               Pred.status = median(Pred.status, na.rm = TRUE)) %>%
                                     mutate(Group = "B")

# mean
ggplot(data = elk.unknowns.1.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# median
ggplot(data = elk.unknowns.1.summary, aes(x = median.sl.3day, y = median.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# sd
ggplot(data = elk.unknowns.1.summary, aes(x = sd.sl.3day, y = sd.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# Pearson's correlation between mean.sl.3day and mean.mcp
cor.test(elk.unknowns.1.summary$mean.sl.3day, elk.unknowns.1.summary$mean.mcp)

#_____________________________________________________________________________________________________________
# 8d. Testing set - Group C ----
#_____________________________________________________________________________________________________________

elk.thisyear.1.summary <- elk.thisyear.1 %>% group_by(CollarID) %>% 
                                     summarize(mean.sl.3day = mean(sl.3day, na.rm = TRUE),
                                               mean.sl.post7 = mean(sl.post7, na.rm = TRUE),
                                               mean.mcp = mean(mcp, na.rm = TRUE),
                                               median.sl.3day = median(sl.3day, na.rm = TRUE),
                                               median.sl.post7 = median(sl.post7, na.rm = TRUE),
                                               median.mcp = median(mcp, na.rm = TRUE),
                                               sd.sl.3day = sd(sl.3day, na.rm = TRUE),
                                               sd.sl.post7 = sd(sl.post7, na.rm = TRUE),
                                               sd.mcp = sd(mcp, na.rm = TRUE),
                                               Pred.status = median(Pred.status, na.rm = TRUE)) %>%
                                     mutate(Group = "C")

# mean
ggplot(data = elk.thisyear.1.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# median
ggplot(data = elk.thisyear.1.summary, aes(x = median.sl.3day, y = median.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# sd
ggplot(data = elk.thisyear.1.summary, aes(x = sd.sl.3day, y = sd.mcp)) +
       theme_bw() +
       geom_point(aes(color = as.factor(Pred.status)), size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE) +
       scale_color_manual(values = c("orange", "blue"))

# Pearson's correlation between mean.sl.3day and mean.mcp
cor.test(elk.thisyear.1.summary$mean.sl.3day, elk.thisyear.1.summary$mean.mcp)

#_____________________________________________________________________________________________________________
# 9. Make decent-looking plots ----
#_____________________________________________________________________________________________________________

# training set only
ggplot(data = elk.part.1.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_smooth(method = "lm", color = "black", se = FALSE, size = 2) +
       geom_point(aes(fill = as.factor(Pred.status)), shape = 21, size = 3, stroke = 1.25) +
       scale_fill_manual(values = c("#FF6600", "white")) +
       theme(panel.grid = element_blank(),
             legend.position = "none") +
       xlab("Mean step length (3 day) - individual mean") +
       ylab("Moving MCP (11 day) - individual mean") +
       scale_y_continuous(breaks = seq(0, 3, 0.5))

summary(lm(mean.mcp ~ mean.sl.3day, data = elk.part.1.summary))

ggplot(data = elk.part.1, 
       aes(x = mcp, y = CollarID.fct.mcp)) +
       theme_bw() +
       geom_density_ridges2(aes(fill = as.factor(Pred.status))) +
       theme(legend.position = "none") +
       coord_cartesian(xlim = c(0, 10)) +
       xlab("Moving MCP (11 day)") +
       scale_fill_manual(values = c("#FF6600", "white")) +
       theme(axis.title.y = element_blank(),
             panel.grid = element_blank())

# all
all.elk.summary <- rbind(elk.part.1.summary, elk.np.1.summary, elk.unknowns.1.summary, elk.thisyear.1.summary)

all.elk.summary$Group <- factor(all.elk.summary$Group, levels = c("Training", "A", "B", "C"))

ggplot(data = all.elk.summary, aes(x = mean.sl.3day, y = mean.mcp)) +
       theme_bw() +
       geom_point(aes(fill = Group), shape = 21, size = 3) +
       geom_smooth(method = "lm", color = "black", se = FALSE, size = 2) +
       theme(panel.grid = element_blank(),
             legend.position = c(0.15, 0.8)) +
       xlab("Mean step length (3 day) - individual mean") +
       ylab("Moving MCP (11 day) - individual mean") +
       scale_y_continuous(breaks = seq(0, 7, 1)) +
       scale_fill_viridis_d(option = "D")

#_____________________________________________________________________________________________________________
# 10. Compare elk for which we have multiple years of data ----
#_____________________________________________________________________________________________________________

repeat.elk <- all.elk %>% filter(Group %in% c("Training", "C")) %>%
                          filter(CollarID %in% c(37705, 37706, 37708, 37709, 37710, 37711, 
                                                 37712, 37714, 37716, 37719, 37720, 
                                                 37722, 37723, 37726, 37725))

repeat.elk.summary <- repeat.elk %>% group_by(CollarID, Group) %>%
                                     summarize(mean.sl.3day = mean(sl.3day, na.rm = TRUE),
                                               mean.sl.post7 = mean(sl.post7, na.rm = TRUE),
                                               mean.mcp = mean(mcp, na.rm = TRUE),
                                               mean.canopy = mean(canopy.5day, na.rm = TRUE),
                                               Pred.status = median(Pred.status, na.rm = TRUE))

ggplot(data = repeat.elk.summary, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1, alpha = 0.2) +
       geom_point(aes(fill = as.factor(Pred.status)), size = 3, shape = 21) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(panel.grid = element_blank(),
             axis.title.x = element_blank(),
             legend.position = "none") +
       coord_cartesian(xlim = c(1.5, 1.5))

#_____________________________________________________________________________________________________________
# 11. 3x4 plot ----
#_____________________________________________________________________________________________________________

correct.part <- repeat.elk.summary %>% filter(CollarID %in% c(37705, 37708, 37710, 37711, 37714, 37716, 37719, 37722, 37726))
correct.np <- repeat.elk.summary %>% filter(CollarID %in% c(37706, 37712, 37720, 37723, 37725))
incorrect.part <- repeat.elk.summary %>% filter(CollarID == 37709)

# correct part, sl.3day
plot.1 <- ggplot(data = correct.part, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#0000FF") +
       geom_point(fill = "#0000FF", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             legend.position = "none",
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             plot.title = element_text(size = 8)) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 3 day") +
       scale_y_continuous(breaks = seq(250, 650, 100)) +
       ggtitle("Correct in 2020, parturient in 2021")

# correct np, sl.3day
plot.2 <- ggplot(data = correct.np, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#009900") +
       geom_point(fill = "#009900", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(size = 8)) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 3 day") +
       scale_y_continuous(breaks = seq(250, 650, 100)) +
       ggtitle("Correct in 2020, non-parturient in 2021")

# incorrect part, sl.3day
plot.3 <- ggplot(data = incorrect.part, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#FF6600") +
       geom_point(fill = "#FF6600", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(size = 8)) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 3 day") +
       scale_y_continuous(breaks = seq(250, 650, 100)) +
       ggtitle("Incorrect in 2020, parturient in 2021")

# incorrect np, sl.3day
plot.4 <- ggplot(data = incorrect.np, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "red") +
       geom_point(fill = "red", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(size = 8)) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 3 day") +
       scale_y_continuous(breaks = seq(250, 650, 100)) +
       ggtitle("Incorrect in 2020, non-parturient in 2021")

# correct part, sl.post7
plot.5 <- ggplot(data = correct.part, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#0000FF") +
       geom_point(fill = "#0000FF", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             legend.position = "none",
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 7 days post") +
       scale_y_continuous(breaks = seq(250, 650, 100))

# correct np, sl.post7
plot.6 <- ggplot(data = correct.np, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#009900") +
       geom_point(fill = "#009900", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 7 days post") +
       scale_y_continuous(breaks = seq(250, 650, 100))

# incorrect part, sl.post7
plot.7 <- ggplot(data = incorrect.part, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#FF6600") +
       geom_point(fill = "#FF6600", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 7 days post") +
       scale_y_continuous(breaks = seq(250, 650, 100))

# incorrect np, sl.post7
plot.8 <- ggplot(data = incorrect.np, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "red") +
       geom_point(fill = "red", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(250, 650)) +
       ylab("Mean step length - 7 days post") +
       scale_y_continuous(breaks = seq(250, 650, 100))

# correct part, mcp
plot.9 <- ggplot(data = correct.part, aes(x = Group, y = mean.mcp, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#0000FF") +
       geom_point(fill = "#0000FF", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             legend.position = "none") +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(0.25, 3.25)) +
       ylab("MCP - 11 day") +
       scale_y_continuous(breaks = seq(0.25, 3.25, 0.5))

# correct np, mcp
plot.10 <- ggplot(data = correct.np, aes(x = Group, y = mean.mcp, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#009900") +
       geom_point(fill = "#009900", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(0.25, 3.25)) +
       ylab("MCP - 11 day") +
       scale_y_continuous(breaks = seq(0.25, 3.25, 0.5))

# incorrect part, mcp
plot.11 <- ggplot(data = incorrect.part, aes(x = Group, y = mean.mcp, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "#FF6600") +
       geom_point(fill = "#FF6600", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(0.25, 3.25)) +
       ylab("MCP - 11 day") +
       scale_y_continuous(breaks = seq(0.25, 3.25, 0.5))

# incorrect np, mcp
plot.12 <- ggplot(data = incorrect.np, aes(x = Group, y = mean.mcp, group = CollarID)) +
       theme_bw() +
       geom_line(size = 1.5, 
                 color = "red") +
       geom_point(fill = "red", 
                  size = 3, 
                  shape = 21, 
                  color = "black", 
                  stroke = 1.5) +
       scale_x_discrete(labels = c("2020", "2021")) +
       theme(axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "none",
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank()) +
       coord_cartesian(xlim = c(1.4, 1.6),
                       ylim = c(0.25, 3.25)) +
       ylab("MCP - 3 day (individual mean") +
       scale_y_continuous(breaks = seq(0.25, 3.25, 0.5))

plot_grid(nrow = 3, ncol = 4, rel_widths = c(1.2, 1, 1, 1),
          plot.1, plot.2, plot.3, plot.4, plot.5, plot.6, plot.7, plot.8,
          plot.9, plot.10, plot.11, plot.12)

#_____________________________________________________________________________________________________________
# 12. 3X2 plot ----
#_____________________________________________________________________________________________________________

consist <- repeat.elk.summary %>% filter(CollarID %in% c(37705, 37708, 37710, 37711, 37714, 37716, 37719, 37722, 37726))
inconsist <- repeat.elk.summary %>% filter(CollarID %in% c(37706, 37709, 37712, 37720, 37723, 37725))

consist <- consist %>% mutate(category = "correct")
inconsist <- inconsist %>% mutate(category = ifelse(CollarID == 37709, "incorrect", "correct"))

# consistent, sl.3day
plot.1b <- ggplot(data = consist, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
                  theme_bw() +
                  geom_line(aes(linetype = category),
                            size = 1.5, 
                            color = "#0000FF") +
                  geom_point(aes(shape = category),
                             fill = "#0000FF", 
                             size = 3, 
                             color = "black", 
                             stroke = 1.5) +
                  scale_x_discrete(labels = c("2020", "2021")) +
                  theme(axis.title.x = element_blank(),
                        legend.position = "none",
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        plot.title = element_text(size = 8)) +
                  coord_cartesian(xlim = c(1.4, 1.6),
                                  ylim = c(250, 650)) +
                  ylab("sl.3day") +
                  scale_y_continuous(breaks = seq(250, 650, 100)) +
                  ggtitle("Consistent predicted status") +
                  scale_shape_manual(values = c(21, 24))

# inconsistent, sl.3day
plot.2b <- ggplot(data = inconsist, aes(x = Group, y = mean.sl.3day, group = CollarID)) +
                 theme_bw() +
                 geom_line(aes(linetype = category),
                           size = 1.5, 
                           color = "#FF3300") +
                 geom_point(aes(shape = category),
                            fill = "#FF3300", 
                            size = 3, 
                            color = "black", 
                            stroke = 1.5) +
                 scale_x_discrete(labels = c("2020", "2021")) +
                 theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position = "none",
                       axis.text.y = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       plot.title = element_text(size = 8)) +
                 coord_cartesian(xlim = c(1.4, 1.6),
                                 ylim = c(250, 650)) +
                 ylab("sl.3day") +
                 scale_y_continuous(breaks = seq(250, 650, 100)) +
                 ggtitle("Inconsistent predicted status") +
                 scale_shape_manual(values = c(21, 24))

# consistent, sl.post7
plot.3b <- ggplot(data = consist, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
                  theme_bw() +
                  geom_line(aes(linetype = category),
                            size = 1.5, 
                            color = "#0000FF") +
                  geom_point(aes(shape = category),
                             fill = "#0000FF", 
                             size = 3, 
                             shape = 21, 
                             color = "black", 
                             stroke = 1.5) +
                  scale_x_discrete(labels = c("2020", "2021")) +
                  theme(axis.title.x = element_blank(),
                        legend.position = "none",
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()) +
                  coord_cartesian(xlim = c(1.4, 1.6),
                                  ylim = c(250, 650)) +
                  ylab("sl.post7") +
                  scale_y_continuous(breaks = seq(250, 650, 100)) +
                 scale_shape_manual(values = c(21, 24))

# inconsistent, sl.post7
plot.4b <- ggplot(data = inconsist, aes(x = Group, y = mean.sl.post7, group = CollarID)) +
                  theme_bw() +
                  geom_line(aes(linetype = category),
                            size = 1.5, 
                            color = "#FF3300") +
                  geom_point(aes(shape = category),
                             fill = "#FF3300", 
                             size = 3, 
                             color = "black", 
                             stroke = 1.5) +
                  scale_x_discrete(labels = c("2020", "2021")) +
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.position = "none",
                        axis.text.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.ticks.y = element_blank()) +
                  coord_cartesian(xlim = c(1.4, 1.6),
                                  ylim = c(250, 650)) +
                  ylab("sl.post7") +
                  scale_y_continuous(breaks = seq(250, 650, 100)) +
                 scale_shape_manual(values = c(21, 24))

# consistent, mcp
plot.5b <- ggplot(data = consist, aes(x = Group, y = mean.mcp, group = CollarID)) +
                  theme_bw() +
                  geom_line(aes(linetype = category),
                            size = 1.5, 
                            color = "#0000FF") +
                  geom_point(aes(shape = category),
                             fill = "#0000FF", 
                             size = 3, 
                             shape = 21, 
                             color = "black", 
                             stroke = 1.5) +
                  scale_x_discrete(labels = c("2020", "2021")) +
                  theme(axis.title.x = element_blank(),
                        legend.position = "none") +
                  coord_cartesian(xlim = c(1.4, 1.6),
                                  ylim = c(0.25, 3.25)) +
                  ylab("mcp") +
                  scale_y_continuous(breaks = seq(0.25, 3.25, 0.5)) +
                 scale_shape_manual(values = c(21, 24))

# inconsistent, mcp
plot.6b <- ggplot(data = inconsist, aes(x = Group, y = mean.mcp, group = CollarID)) +
                  theme_bw() +
                  geom_line(aes(linetype = category),
                            size = 1.5, 
                            color = "#FF3300") +
                  geom_point(aes(shape = category),
                             fill = "#FF3300", 
                             size = 3, 
                             color = "black", 
                             stroke = 1.5) +
                  scale_x_discrete(labels = c("2020", "2021")) +
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.position = "none",
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()) +
                  coord_cartesian(xlim = c(1.4, 1.6),
                                  ylim = c(0.25, 3.25)) +
                  ylab("mcp") +
                  scale_y_continuous(breaks = seq(0.25, 3.25, 0.5)) +
                  scale_shape_manual(values = c(21, 24))

plot_grid(nrow = 3, ncol = 2, rel_widths = c(1.2, 1, 1, 1),
          plot.1b, plot.2b, plot.3b, plot.4b, plot.5b, plot.6b)
           
#_____________________________________________________________________________________________________________
# 13. Repeatability ----
#_____________________________________________________________________________________________________________

# overall
rpt(mean.sl.3day ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.summary, nboot = 1000, npermut = 0)
rpt(mean.sl.post7 ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.summary, nboot = 1000, npermut = 0)
rpt(mean.mcp ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.summary, nboot = 1000, npermut = 0)

# those with agreement
repeat.elk.agreement <- repeat.elk.summary %>% filter(CollarID %in% c(37705, 37708, 37710, 37711, 37714, 37719, 37716, 37722, 37726))

rpt(mean.sl.3day ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.agreement, nboot = 1000, npermut = 0)
rpt(mean.sl.post7 ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.agreement, nboot = 1000, npermut = 0)
rpt(mean.mcp ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.agreement, nboot = 1000, npermut = 0)
rpt(mean.canopy ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.agreement, nboot = 1000, npermut = 0)

# those with disagreement
repeat.elk.disagreement <- repeat.elk.summary %>% filter(CollarID %in% c(37706, 37712, 37720, 37723, 37725, 37709))

rpt(mean.sl.3day ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.disagreement, nboot = 1000, npermut = 0)
rpt(mean.sl.post7 ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.disagreement, nboot = 1000, npermut = 0)
rpt(mean.mcp ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.disagreement, nboot = 1000, npermut = 0)
rpt(mean.canopy ~ (1 | CollarID), grname = "CollarID", data = repeat.elk.disagreement, nboot = 1000, npermut = 0)

#_____________________________________________________________________________________________________________
# 14. Plot comparing parturient and non-parturient individuals ----
#_____________________________________________________________________________________________________________

load("elk_model.Rdata")

# predicted
predictions <- as.data.frame(predict(elk.model, type = "prob"))

pred.prob <- predictions[ ,2]

elk.data.complete <- elk.part[complete.cases(elk.part), ]

elk.data.complete <- cbind(elk.data.complete, pred.prob)

elk.data.indiv <- elk.data.complete %>% filter(CollarID == 37711)

elk.part.plot <- ggplot(data = elk.data.indiv, aes(x = DOY, y = pred.prob)) +
        theme_bw() +
        geom_hline(yintercept = 0.75) +
        geom_vline(xintercept = elk.data.indiv$DOY[which.max(elk.data.indiv$pred.prob)],
                   linetype = "dashed") +
        geom_point(size = 2,
                   shape = 21,
                   fill = "deepskyblue1",
                   stroke = 1) +
        theme(panel.grid = element_blank()) +
        ylab("Predicted probability") +
        xlab("Day of the year") +
        scale_x_continuous(breaks = seq(130, 190, 10)) +
        scale_y_continuous(breaks = seq(0, 0.9, 0.1)) +
        coord_cartesian(ylim = c(0, 0.9))
        
# np
pred.np <- as.data.frame(predict(elk.model, newdata = elk.np, type = "prob"))

# bind prob of "1" to main df
pred.np.prob <- pred.np[ ,2]

elk.np <- cbind(elk.np, pred.np.prob)

elk.np.indiv <- elk.np %>% filter(CollarID == 45469)

elk.np.plot <- ggplot(data = elk.np.indiv, aes(x = DOY, y = pred.np.prob)) +
        theme_bw() +
        geom_hline(yintercept = 0.75) +
        geom_point(size = 2,
                   shape = 21,
                   fill = "orange",
                   stroke = 1) +
        theme(panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank()) +
        ylab("Predicted probability") +
        xlab("Day of the year") +
        scale_x_continuous(breaks = seq(130, 200, 10)) +
        scale_y_continuous(breaks = seq(0, 0.9, 0.1)) +
        coord_cartesian(ylim = c(0, 0.9))

plot_grid(nrow = 1, ncol = 2, rel_widths = c(1.1, 1),
          elk.part.plot, elk.np.plot)
