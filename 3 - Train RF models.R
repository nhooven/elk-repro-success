# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 3 - Build random forest models
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Apr 2021
# Date completed: 1 Jun 2021
# Date modified: 2 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(randomForest)    # fit RF model
library(mefa4)           # %notin%

set.seed(5432)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

elk.data <- read.csv("elk_days_part.csv")

# remove 37718 and 45510
elk.data <- elk.data %>% filter(CollarID %notin% c(37718, 45510))

# change part.case to a factor
elk.data$part.case <- as.factor(elk.data$part.case)

# density plots of data
ggplot(data = elk.data, aes(x = sl.3day, color = part.case, fill = part.case)) +
       theme_bw() +
       geom_density(alpha = 0.5)

ggplot(data = elk.data, aes(x = sl.post7, color = part.case, fill = part.case)) +
       theme_bw() +
       geom_density(alpha = 0.5)

ggplot(data = elk.data, aes(x = mcp, color = part.case, fill = part.case)) +
       theme_bw() +
       geom_density(alpha = 0.5)

#_____________________________________________________________________________________________________________
# 3. Fit RF model with balanced sample size and 1000 trees ----
#_____________________________________________________________________________________________________________

elk.model <- randomForest(as.factor(part.case) ~ sl.3day +
                                                 mcp +
                                                 sl.post7,
                                     na.action = na.omit,
                                     sampsize = c(36, 36),
                                     ntree = 1000,
                                     data = elk.data)

# confusion matrix
elk.model

# variable importance
importance(elk.model, type = 2)

#_____________________________________________________________________________________________________________
# 4. Determine predicted probabilities for each day ----
#_____________________________________________________________________________________________________________

predictions <- as.data.frame(predict(elk.model, type = "prob"))

pred.prob <- predictions[ ,2]

elk.data.complete <- elk.data[complete.cases(elk.data), ]

elk.data.complete <- cbind(elk.data.complete, pred.prob)

#_____________________________________________________________________________________________________________
# 5a. Graph probability time series ----
#_____________________________________________________________________________________________________________

ggplot(data = elk.data.complete, aes(DOY, pred.prob)) +
       geom_hline(yintercept = 0.7) +
       theme_bw() +
       facet_wrap(~CollarID) +
       geom_point(color = "darkgreen", alpha = 0.5) +
       ylab("") +
       xlab("") +
       ggtitle("Confirmed parturient")

#_____________________________________________________________________________________________________________
# 5b. Plot individual probabilities ----
#_____________________________________________________________________________________________________________

part.id <- 46397

indiv.part <- elk.data.complete %>% filter(CollarID == part.id)

ggplot(data = indiv.part, aes(DOY, pred.prob)) +
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
# 6. Determine predicted parturition date ----
#_____________________________________________________________________________________________________________

all.prob.summary <- data.frame()

for (x in unique(elk.data$CollarID)) {
  
  elkID <- x
  
  indiv.prob <- elk.data.complete %>% filter(CollarID == elkID)
  
  indiv.prob.2 <- data.frame(CollarID = elkID,
                             max.prob = max(indiv.prob$pred.prob, na.rm = TRUE),
                             pred.day = indiv.prob$DOY[which.max(indiv.prob$pred.prob)])
  
  all.prob.summary <- rbind(all.prob.summary, indiv.prob.2)
             
  
}

# calculate deviance from actual day of parturition
elk.part.dates <- data.frame("CollarID" = c(37705, 37706, 37708, 37709, 37710, 37711, 37712, 
                                            37714, 37715, 37716, 37717, 37718, 37719, 37720, 
                                            37722, 37723, 37724, 37725, 37726, 37727,
                                            45511, 45501, 45505, 46393, 45494, 46394, 45495, 45496, 46402, 46400,
                                            45507, 45492, 46401, 45500, 45509, 45493, 45498, 45510),
                             "Part.date" = c(158, 153, 138, 144, 149, 161, 175, 152, 160, 157,
                                             160, 152, 151, 143, 146, 166, 160, 163, 147, 144,
                                             141, 144, 144, 146, 147, 149, 150, 150, 152, 158, 163, 164, 166, 171, 174, 181, 190, 196))

# merge the two datasets
all.prob.summary.1 <- merge(all.prob.summary, elk.part.dates) 

# calculate differences
all.prob.summary.1 <- all.prob.summary.1 %>% mutate(diff = as.numeric(pred.day) - Part.date)

# write to csv
write.csv(all.prob.summary.1, "all_prob_summary_parturient.csv")

#_____________________________________________________________________________________________________________
# 7. Examine peaks ----
#_____________________________________________________________________________________________________________

# peak parameters
# minimum peak height (a peak must be this high to be considered a part event)
min.ph <- 0.7

# steps up and down (how many days should the maximum be bounded by?)
up.down <- 1

elk.part.peaks <- data.frame()

for(x in unique(elk.data$CollarID)) {
  
  elkID <- x
  
  peak.data <- elk.data %>% filter(CollarID == elkID)
  
  peak.data.1 <- peak.data$pred.prob
  
  # find peaks
  found.peaks <- as.data.frame(findpeaks(peak.data.1, nups = up.down, ndowns = up.down, minpeakheight = min.ph))
  
  #if-else statement in case no peaks are identified
  if (nrow(found.peaks) > 0) {
    
    # bind together with elkID
    found.peaks <- cbind(elkID, found.peaks)
  
  } else {
    
    found.peaks <- data.frame(elkID = elkID, V1 = NA, V2 = NA, V3 = NA, V4 = NA)
    
  }
  
  # rename
  names(found.peaks) <- c("CollarID", "max.prob", "day", "begin", "end")
  
  # bind to master df
  elk.part.peaks <- rbind(elk.part.peaks, found.peaks)
  
}
# subset 2020 data
elk.part.peaks.2020 <- elk.part.peaks[substr(elk.part.peaks$CollarID, 1, 1) == 3,]

elk.part.peaks.2020[c("day", "begin", "end")] <- elk.part.peaks.2020[c("day", "begin", "end")] + 130

# subset 2021 data
elk.part.peaks.2021 <- elk.part.peaks[substr(elk.part.peaks$CollarID, 1, 1) == 4,]

elk.part.peaks.2021[c("day", "begin", "end")] <- elk.part.peaks.2021[c("day", "begin", "end")] + 129

# bind back together
elk.part.peaks.1 <- rbind(elk.part.peaks.2020, elk.part.peaks.2021)

# merge actual part date dataset
elk.part.peaks.2 <- merge(elk.part.peaks.1 , elk.part.dates) 

# calculate differences between actual part date and the maximum and determine if the actual part date is within the peak
elk.part.peaks.2 <- elk.part.peaks.2 %>% mutate(diff = as.numeric(day) - Part.date,
                                                inside = ifelse(Part.date >= begin & Part.date <= end,
                                                                TRUE,
                                                                FALSE))

# write to csv
write.csv(elk.part.peaks.2, "elk_part_peaks.csv")

#_____________________________________________________________________________________________________________
# 8. Determine condition for positive determination with z-scores ----
#_____________________________________________________________________________________________________________

# group and summarize
elk.data.summaries <- elk.data %>% group_by(CollarID) %>%
                                   filter(!is.na(pred.prob)) %>%
                                   summarize(max = max(pred.prob, na.rm = TRUE),
                                             count = n())

View(elk.data.summaries)


#_____________________________________________________________________________________________________________
# 9. Save image for elk.models ----
#_____________________________________________________________________________________________________________

save(elk.model, file = "elk_model.Rdata")
