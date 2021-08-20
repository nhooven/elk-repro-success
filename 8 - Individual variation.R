# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 8 - Individual variation
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 17 Aug 2021
# Date completed: 
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mefa4)        # %notin%
library(ggridges)     # ridgeline plots

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


elk.part.1 <- elk.part %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp) %>%
                                  mutate(Year = ifelse(CollarID %in% elk.births.2020,
                                                       2020,
                                                       2021),
                                         Group = "Training")

# Testing set - Group A (non-parturient/calf loss)
elk.np.2020 <- as.vector(c(37704, 102489, 102491, 102497, 103172, 103174, 103179, 103181, 103182))


elk.np.1 <- elk.np %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp) %>%
                              mutate(Year = ifelse(CollarID %in% elk.np.2020,
                                                   2020,
                                                   2021),
                                     Group = "A")

# Testing set - Group B (unknowns)
elk.unk.2020 <- as.vector(c(37703, 37707, 101940, 101968, 101969, 101978, 102492, 102493, 102495, 102536,
                            103176, 103177, 103178, 103183, 103249))


elk.unknowns.1 <- elk.unknowns %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp) %>%
                                          mutate(Year = ifelse(CollarID %in% elk.unk.2020,
                                                               2020,
                                                               2021),
                                                 Group = "B")

# Testing set - Group C (2020 collars in 2021)
elk.thisyear.1 <- elk.thisyear %>% dplyr::select(CollarID, sl.3day, sl.post7, mcp) %>%
                                          mutate(Year = 2021, 
                                                 Group = "C")

#_____________________________________________________________________________________________________________
# 4. Add a column of predicted status ----
#_____________________________________________________________________________________________________________

# Training set
elk.part.yes <- as.vector(c(37705, 37708, 37710, 37711, 37712, 37714, 37715, 37716, 37717, 37719, 37720,
                            37722, 37723, 37724, 37725, 37726, 45493, 45494, 45495, 45496, 45498, 45500,
                            45501, 45505, 45507, 45509, 45511, 46393, 46394, 46401, 46402))

elk.part.1 <- elk.part.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.part.yes,
                                                         1,
                                                         0))

# Testing set - Group A (non-parturient/calf loss)
elk.np.yes <- as.vector(c(103182, 37704, 45502))

elk.np.1 <- elk.np.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.np.yes,
                                                     1,
                                                     0))

# Testing set - Group B (unknowns)
elk.unknowns.yes <- as.vector(c(37718, 101978, 102492, 102536, 103177, 103183, 103186, 103249))

elk.unknowns.1 <- elk.unknowns.1 %>% mutate(Pred.status = ifelse(CollarID %in% elk.unknowns.yes,
                                            1,
                                            0))

# Testing set - Group C (2020 collars in 2021)
elk.thisyear.yes <- as.vector(c(37703, 37705, 37708, 37709, 37710, 37711, 37714, 37716, 37718,
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
       geom_point(aes(fill = as.factor(Pred.status)), shape = 21, size = 3, stroke = 1.25) +
       geom_smooth(method = "lm", color = "black", se = FALSE, size = 2) +
       scale_fill_manual(values = c("#FF6600", "white")) +
       theme(panel.grid = element_blank(),
             legend.position = "none") +
       xlab("Mean step length (3 day) - individual mean") +
       ylab("Moving MCP (11 day) - individual mean") +
       scale_y_continuous(breaks = seq(0, 3, 0.5))

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
