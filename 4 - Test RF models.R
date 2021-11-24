# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 4 - Test random forest models
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 1 Jun 2021
# Date completed: 9 Jul 2021
# Date modified: 22 Nov 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(randomForest)   # random forest models
library(mefa4)          # %notin%
 
set.seed(5432)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# read in RF model
load(paste0(getwd(), "/elk_model.Rdata"))

# non-pregnant/lost calf/outside of window
elk.np <- read.csv("elk_np.csv")

# unknowns 
elk.unknowns <- read.csv("elk_unknowns.csv")

# 2020 collars in 2021
elk.thisyear <- read.csv("elk_thisyear_2021.csv")

#_____________________________________________________________________________________________________________
# 3. Determine decision rule (probability threshold) ----
#_____________________________________________________________________________________________________________
# 3a. Predict on non-parturient data ----
#_____________________________________________________________________________________________________________

# predict on np data
pred.np <- as.data.frame(predict(elk.model, newdata = elk.np, type = "prob"))

# bind prob of "1" to main df
pred.np.prob <- pred.np[ ,2]

elk.np <- cbind(elk.np, pred.np.prob)

# graph probability time series
ggplot(data = elk.np, aes(DOY, pred.np.prob)) +
       geom_hline(yintercept = 0.7) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point()

# mean and median
mean(elk.np$pred.np.prob, na.rm = TRUE)
median(elk.np$pred.np.prob, na.rm = TRUE)
max(elk.np$pred.np.prob, na.rm = TRUE)

quantile(elk.np$pred.np.prob, na.rm = TRUE, prob = 0.90)

#_____________________________________________________________________________________________________________
# 3c. Summarize number of probabilities ----
#_____________________________________________________________________________________________________________

# group and summarize
elk.np.summaries <- elk.np %>% group_by(CollarID) %>%
                               filter(!is.na(pred.np.prob)) %>%
                               summarize(max = max(pred.np.prob, na.rm = TRUE),
                                                   count = n())

View(elk.np.summaries)

#_____________________________________________________________________________________________________________
# 3d. Plot individual probabilities ----
#_____________________________________________________________________________________________________________

np.id <- 45502

indiv.np <- elk.np %>% filter(CollarID == np.id)

ggplot(data = indiv.np, aes(DOY, pred.np.prob)) +
       geom_hline(yintercept = 0.95, linetype = "dashed") +
       geom_hline(yintercept = 0.90) +
       geom_hline(yintercept = 0.85, linetype = "dashed") +
       geom_hline(yintercept = 0.80) +
       geom_hline(yintercept = 0.75, linetype = "dashed") +
       geom_hline(yintercept = 0.70) +
       geom_hline(yintercept = 0.65, linetype = "dashed") +
       geom_hline(yintercept = 0.6) +
       geom_hline(yintercept = 0.55, linetype = "dashed") +
       geom_hline(yintercept = 0.5) +
       geom_hline(yintercept = 0.45, linetype = "dashed") +
       geom_hline(yintercept = 0.4) +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks = seq(0, 1, 0.05))

#_____________________________________________________________________________________________________________
# 4a. Predict on unknowns (confirmed pregnant) ----
#_____________________________________________________________________________________________________________

# add in 18's data
part.elk <- read.csv("elk_days_part.csv")

other.data <- part.elk %>% filter(CollarID == 37718) %>%
                    dplyr::select(1:9)

elk.unknowns <- rbind(elk.unknowns, other.data)

# predict on unk data
pred.unk <- as.data.frame(predict(elk.model, newdata = elk.unknowns, type = "prob"))

# bind prob of "1" to main df
pred.unk.prob <- pred.unk[ ,2]

elk.unknowns <- cbind(elk.unknowns, pred.unk.prob)

# graph probability time series
ggplot(data = elk.unknowns, aes(DOY, pred.unk.prob)) +
       geom_hline(yintercept = 0.7) +
       geom_hline(yintercept = 0.85) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point(alpha = 0.5, 
                  color = "darkgreen") +
       xlab("") +
       ylab("Predicted probability")

#_____________________________________________________________________________________________________________
# 4c. Summarize number of probabilities ----
#_____________________________________________________________________________________________________________

# group and summarize
elk.unk.summaries <- elk.unknowns %>% group_by(CollarID) %>%
                                      filter(!is.na(pred.unk.prob)) %>%
                                      summarize(max = max(pred.unk.prob, na.rm = TRUE),
                                                count = n())

View(elk.unk.summaries)

#_____________________________________________________________________________________________________________
# 4d. Plot individual probabilities ----
#_____________________________________________________________________________________________________________

indiv.unk <- elk.unknowns %>% filter(CollarID == 103249)

ggplot(data = indiv.unk, aes(DOY, pred.unk.prob)) +
       geom_hline(yintercept = 0.75, linetype = "dashed") +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       facet_wrap(~CollarID) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("")

# add in test data to use 37718's collar
elk.part <- read.csv("elk_days_part.csv")

indiv.part <- elk.part %>% filter(CollarID == 37718)

# predict on unk data
pred.part <- as.data.frame(predict(elk.model, newdata = indiv.part, type = "prob"))

# bind prob of "1" to main df
pred.part.prob <- pred.part[ ,2]

elk.part <- cbind(elk.part, pred.part.prob)

ggplot(data = indiv.part, aes(DOY, pred.part.prob)) +
       geom_hline(yintercept = 0.75, linetype = "dashed") +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       facet_wrap(~CollarID) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("")

#_____________________________________________________________________________________________________________
# 5a. Predict on 2020 collars in 2021 ----
#_____________________________________________________________________________________________________________

# predict on thisyear data
pred.thisyear <- as.data.frame(predict(elk.model, newdata = elk.thisyear, type = "prob"))

# bind prob of "1" to main df
pred.thisyear.prob <- pred.thisyear[ ,2]

elk.thisyear <- cbind(elk.thisyear, pred.thisyear.prob)

# graph probability time series
ggplot(data = elk.thisyear, aes(DOY, pred.thisyear.prob)) +
       geom_hline(yintercept = 0.7) +
       geom_hline(yintercept = 0.85) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point()

#_____________________________________________________________________________________________________________
# 5c. Summarize number of probabilities ----
#_____________________________________________________________________________________________________________

# group and summarize
elk.thisyear.summaries <- elk.thisyear %>% group_by(CollarID) %>%
                                           filter(!is.na(pred.thisyear.prob)) %>%
                                           summarize(max = max(pred.thisyear.prob, na.rm = TRUE),
                                                     count = n())

View(elk.thisyear.summaries)

#_____________________________________________________________________________________________________________
# 5d. Plot individual probabilities ----
#_____________________________________________________________________________________________________________

indiv.thisyear <- elk.thisyear %>% filter(CollarID == 37726)

ggplot(data = indiv.thisyear, aes(DOY, pred.thisyear.prob)) +
       geom_hline(yintercept = 0.75, linetype = "dashed") +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line() +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("")

#_____________________________________________________________________________________________________________
# 6. Graph all test data ----
#_____________________________________________________________________________________________________________
# 6a. Non-parturient ----
#_____________________________________________________________________________________________________________

elk.np.keep <- elk.np %>% filter(CollarID %notin% c(103185, 103244, 103250, 45506))

# graph probability time series
ggplot(data = elk.np.keep, aes(DOY, pred.np.prob)) +
       geom_hline(yintercept = 0.7) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point(color = "#FF3300", alpha = 0.4) +
       ylab("") +
       xlab("") +
       ggtitle("Non-parturient")

#_____________________________________________________________________________________________________________
# 6b. Unknowns (confirmed pregnant) ----
#_____________________________________________________________________________________________________________

elk.unk.keep <- elk.unknowns %>% filter(CollarID %notin% c(101940, 101968, 101978, 102536, 103251))

# graph probability time series
ggplot(data = elk.unk.keep, aes(DOY, pred.unk.prob)) +
       geom_hline(yintercept = 0.7) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point(color = "darkblue", alpha = 0.4) +
       ylab("") +
       xlab("") +
       ggtitle("Unknowns (confirmed preg)")

#_____________________________________________________________________________________________________________
# 6c. Unknowns (2020 collars in 2021) ----
#_____________________________________________________________________________________________________________

# graph probability time series
ggplot(data = elk.thisyear, aes(DOY, pred.thisyear.prob)) +
       geom_hline(yintercept = 0.7) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point(color = "orange", alpha = 0.4) +
       ylab("") +
       xlab("") +
       ggtitle("Unknowns (2020 collars in 2021)")
