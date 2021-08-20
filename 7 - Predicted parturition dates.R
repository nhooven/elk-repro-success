# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 7 - Predicted parturition dates
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Aug 2021
# Date completed: 13 Aug 2021 
# Date modified: 17 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(sp)             # work with spatial objects
library(amt)            # work with steps
library(adehabitatHR)   # fit MCPs
library(mefa4)          # notin function
library(randomForest)   # random forest models

#_____________________________________________________________________________________________________________
# 2. Read in RF model and data ----
#_____________________________________________________________________________________________________________

# read in RF model
load(paste0(getwd(), "/elk_model.Rdata"))

# processed data
# parturient elk
elk.part <- read.csv("elk_days_part.csv")

# non-pregnant/lost calf/outside of window
elk.np <- read.csv("elk_np.csv")

# unknowns 
elk.unknowns <- read.csv("elk_unknowns.csv")

# 2020 collars in 2021
elk.thisyear <- read.csv("elk_thisyear_2021.csv")

#_____________________________________________________________________________________________________________
# 3. Parturient elk ----
#_____________________________________________________________________________________________________________

predictions <- as.data.frame(predict(elk.model, type = "prob"))

pred.prob <- predictions[ ,2]

elk.part.complete <- elk.part[complete.cases(elk.part), ]

elk.part.complete <- cbind(elk.part.complete, pred.prob)

part.id <- 46400

indiv.part <- elk.part.complete %>% filter(CollarID == part.id)

ggplot(data = indiv.part, aes(DOY, pred.prob)) +
       geom_hline(yintercept = 0.70) +
       geom_vline(xintercept = indiv.part$DOY[which.max(indiv.part$pred.prob)], color = "darkgreen", size = 1.25) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks = seq(0, 1, 0.05)) +
       scale_x_continuous(breaks = seq(130, 200, 2))

#_____________________________________________________________________________________________________________
# 4. Test set Group B (unknowns) ----
#_____________________________________________________________________________________________________________

# predict on unk data
pred.unk <- as.data.frame(predict(elk.model, newdata = elk.unknowns, type = "prob"))

# bind prob of "1" to main df
pred.unk.prob <- pred.unk[ ,2]

elk.unknowns <- cbind(elk.unknowns, pred.unk.prob)

unk.id <- 103249

indiv.unk <- elk.unknowns %>% filter(CollarID == unk.id)

ggplot(data = indiv.unk, aes(DOY, pred.unk.prob)) +
       geom_hline(yintercept = 0.70) +
       geom_vline(xintercept = indiv.unk$DOY[which.max(indiv.unk$pred.unk.prob)], color = "darkgreen", size = 1.25) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks = seq(0, 1, 0.05)) +
       scale_x_continuous(breaks = seq(130, 200, 2))

# predict on 37718
data.18 <- elk.part %>% filter(CollarID == 37718)

pred.18 <- as.data.frame(predict(elk.model, newdata = data.18, type = "prob"))

# bind prob of "1" to main df
pred.18.prob <- pred.18[ ,2]

data.18 <- cbind(data.18, pred.18.prob)

ggplot(data = data.18 , aes(DOY, pred.18.prob)) +
       geom_hline(yintercept = 0.70) +
       geom_vline(xintercept = data.18 $DOY[which.max(data.18 $pred.18.prob)], color = "darkgreen", size = 1.25) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks = seq(0, 1, 0.05)) +
       scale_x_continuous(breaks = seq(130, 200, 2))

#_____________________________________________________________________________________________________________
# 5. Test set Group C (2020 collars in 2021) ----
#_____________________________________________________________________________________________________________

# predict on unk data
pred.thisyear <- as.data.frame(predict(elk.model, newdata = elk.thisyear, type = "prob"))

# bind prob of "1" to main df
pred.thisyear.prob <- pred.thisyear[ ,2]

elk.thisyear <- cbind(elk.thisyear, pred.thisyear.prob)

thisyear.id <- 37726

indiv.thisyear <- elk.thisyear %>% filter(CollarID == thisyear.id)

ggplot(data = indiv.thisyear, aes(DOY, pred.thisyear.prob)) +
       geom_hline(yintercept = 0.70) +
       geom_vline(xintercept = indiv.thisyear$DOY[which.max(indiv.thisyear$pred.thisyear.prob)], color = "darkgreen", size = 1.25) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text.x = element_text(angle = 90, vjust = 0.5)) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks = seq(0, 1, 0.05)) +
       scale_x_continuous(breaks = seq(130, 200, 2))

#_____________________________________________________________________________________________________________
# 6. Create density plots ----
#_____________________________________________________________________________________________________________

# read in data
part.dates <- read.csv("Part_dates.csv")

# reorder factor levels
part.dates$group <- factor(part.dates$group, levels = c("predicted", "confirmed"))

ggplot() +
       theme_bw() +
       geom_rect(aes(xmin = 151,
                 xmax = 152,
                 ymin = 0,
                 ymax = Inf),
                 alpha = 0.1) + 
       geom_rect(aes(xmin = 181,
                 xmax = 182,
                 ymin = 0,
                 ymax = Inf),
                 alpha = 0.1) +
       geom_rect(aes(xmin = 212,
                 xmax = 213,
                 ymin = 0,
                 ymax = Inf),
                 alpha = 0.1) +
       geom_density(data = part.dates, 
                    aes(x = DOY, color = group, fill = group),
                    size = 1.1,
                    alpha = 0.15) +
       theme(panel.grid = element_blank(),
             legend.title = element_blank(),
             legend.position = c(0.8, 0.7)) +
       ylab("") +
       xlab("Day of the year") +
       scale_x_continuous(breaks = seq(140, 250, 10)) +
       scale_y_continuous(breaks = seq(0, 0.03, 0.01)) +
       coord_cartesian(ylim = c(0, 0.042)) +
       scale_color_manual(values = c("#FF3300", "#6600CC")) +
       scale_fill_manual(values = c("#FF3300", "#6600CC")) + 
       geom_segment(aes(x = median(part.dates$DOY[part.dates$group == "predicted"]),
                        xend = median(part.dates$DOY[part.dates$group == "predicted"]),
                        y = 0,
                        yend = Inf),
                    color = "#FF3300",
                    size = 1.1,
                    linetype = "dashed") +
       geom_segment(aes(x = median(part.dates$DOY[part.dates$group == "confirmed"]),
                        xend = median(part.dates$DOY[part.dates$group == "confirmed"]),
                        y = 0,
                        yend = Inf),
                    color = "#6600CC",
                    size = 1.1,
                    linetype = "dashed") +
       geom_rug(data = part.dates, 
                aes(x = DOY, color = group), 
                size = 0.75)
       
#_____________________________________________________________________________________________________________
# 7. Summarize ----
#_____________________________________________________________________________________________________________

median(part.dates$DOY[part.dates$group == "confirmed"])

median(part.dates$DOY[part.dates$group == "predicted"])
