# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 6 - Fix success and days predicted
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Aug 2021
# Date completed: 13 Aug 2021
# Date modified: 
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

#_____________________________________________________________________________________________________________
# 2a. Parturient elk ----
#_____________________________________________________________________________________________________________

# raw data
vectronic.data <- read.csv("D:/Elk project/Data analysis/Raw data processing/Relocations_vectronic_1.csv")
lotek.data <- read.csv("D:/Elk project/Data analysis/Raw data processing/Relocations_lotek_1.csv")

# get dates into correct formats
vectronic.data$t <- as.POSIXct(vectronic.data$t, tz = "America/New_York")
lotek.data$t <- as.POSIXct(lotek.data$t, tz = "America/New_York")

# add collar brand
vectronic.data$brand <- "Vectronic"
lotek.data$brand <- "Lotek"

# rbind both data frames together
elk.data <- rbind(vectronic.data, lotek.data)

# processed data
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
# 3. Determine fix success rate ----
#_____________________________________________________________________________________________________________
# 3a. Parturient elk ----
#_____________________________________________________________________________________________________________

# 2020
# define elk that gave birth
elk.births.2020 <- as.vector(c(37705, 37706, 37708, 37709, 37710, 37711, 37712, 
                               37714, 37715, 37716, 37717, 37719, 37720, 
                               37722, 37723, 37724, 37725, 37726, 37727))

# define timeframe
start.time.1 <- as.POSIXct("2020-05-15 00:00:00")
end.time.1 <- as.POSIXct("2020-07-15 23:59:59")

# determine number of hours within time period (start - 1 and end + 7)
total.time.1 <- as.numeric((end.time.1 + 7*24*60*60) - (start.time.1 - 1*24*60*60)) * 24

# df to hold data
elk.1.2020 <- data.frame()

for (i in elk.births.2020) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.1 - 1*24*60*60) & t < (end.time.1 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.1 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 122, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 122, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.1.2020 <- rbind(elk.1.2020, indiv.fix)
  
}

# summarize
mean(elk.1.2020$fix.suc)
range(elk.1.2020$fix.suc)

# 2021
# define elk that gave birth
elk.births.2021 <- as.vector(c(45492, 45493, 45494, 45495, 45496, 45498, 45500, 45501, 45505,
                               45507, 45509, 45510, 45511, 46393, 46394, 46397,  46400, 46401, 46402))

# define timeframe
start.time.2 <- as.POSIXct("2021-05-15 00:00:00")
end.time.2 <- as.POSIXct("2021-07-15 23:59:59")

# determine number of hours within time period (start - 1 and end + 7)
total.time.2 <- as.numeric((end.time.2 + 7*24*60*60) - (start.time.2 - 1*24*60*60)) * 24

# df to hold data
elk.1.2021 <- data.frame()

for (i in elk.births.2021) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.2 - 1*24*60*60) & t < (end.time.2 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.2 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 125, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 125, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.1.2021 <- rbind(elk.1.2021, indiv.fix)
  
}

# summarize
mean(elk.1.2021$fix.suc)
range(elk.1.2021$fix.suc)

#_____________________________________________________________________________________________________________
# 3b. Non-parturient elk ----
#_____________________________________________________________________________________________________________

# 2020
# define elk who were not pregnant, whose calf died, or who gave birth outside of the calving season
elk.np.2020 <- as.vector(c(37704, 102489, 102491, 102497, 103172, 103174, 103179, 103181, 103182))

# df to hold data
elk.2.2020 <- data.frame()

for (i in elk.np.2020) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.1 - 1*24*60*60) & t < (end.time.1 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.1 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 122, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 122, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.2.2020 <- rbind(elk.2.2020, indiv.fix)
  
}

# summarize
mean(elk.2.2020$fix.suc)
range(elk.2.2020$fix.suc)

# 2021
# define elk who were not pregnant, whose calf died, or who gave birth outside of the calving season
elk.np.2021 <- as.vector(c(46391, 46399, 45508, 46396, 45497, 46392, 45506,
                           45502, 45469, 103185, 103250, 45499, 103244, 103239, 103248))

# df to hold data
elk.2.2021 <- data.frame()

for (i in elk.np.2021) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.2 - 1*24*60*60) & t < (end.time.2 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.2 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 125, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 125, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.2.2021 <- rbind(elk.2.2021, indiv.fix)
  
}

# summarize
mean(elk.2.2021$fix.suc)
range(elk.2.2021$fix.suc)

#_____________________________________________________________________________________________________________
# 3c. Unknowns ----
#_____________________________________________________________________________________________________________

# 2020
elk.unknowns.2020 <- as.vector(c(37703, 37707, 101940, 101968, 101969, 101978, 102492, 102493, 102495, 102536,
                                 103176, 103177, 103178, 103183, 103249))

# df to hold data
elk.3.2020 <- data.frame()

for (i in elk.unknowns.2020) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.1 - 1*24*60*60) & t < (end.time.1 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.1 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 122, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 122, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.3.2020 <- rbind(elk.3.2020, indiv.fix)
  
}

# summarize
mean(elk.3.2020$fix.suc)
range(elk.3.2020$fix.suc)

# 2021
# define unknown elk
elk.unknowns.2021 <- as.vector(c(45504, 45470, 103184, 103186, 103173, 103251))

# df to hold data
elk.3.2021 <- data.frame()

for (i in elk.unknowns.2021) {
  
  elkID <- i
  
  indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.2 - 1*24*60*60) & t < (end.time.2 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.2 / 13)
  
  # fix success
  fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 125, n.fixes / ex.fixes)
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 125, ex.fixes),
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.3.2021 <- rbind(elk.3.2021, indiv.fix)
  
}

# summarize
mean(elk.3.2021$fix.suc)
range(elk.3.2021$fix.suc)

#_____________________________________________________________________________________________________________
# 3d. 2020 collars in 2021 ----
#_____________________________________________________________________________________________________________

# define Vectronic elk still alive through 2021 calving season
elk.thisyear.2021 <- as.vector(c(37703, 37704, 37705, 37706, 37707, 37708, 37709, 37710, 37711,
                                 37712, 37714, 37716, 37718, 37719, 37720, 37722, 37723, 37725, 37726))

# df to hold data
elk.4.2021 <- data.frame()

for (i in elk.thisyear.2021) {
  
  elkID <- i
  
    indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.2 - 1*24*60*60) & t < (end.time.2 + 7*24*60*60))
  
  # number of fixes
  n.fixes <- nrow(indiv.data)
  
  # expected number of fixes
  ex.fixes <- round(total.time.2 / 13)
  
  # fix success
  fix.suc <- n.fixes / ex.fixes
  
  # bind to df
  indiv.fix <- data.frame("CollarID" = elkID, 
                          "n.fixes" = n.fixes,
                          "ex.fixes" = ex.fixes,
                          "fix.suc" = fix.suc,
                          "brand" = indiv.data$brand[1])
  
  elk.4.2021 <- rbind(elk.4.2021, indiv.fix)
  
}

# summarize
mean(elk.4.2021$fix.suc)
range(elk.4.2021$fix.suc)

#_____________________________________________________________________________________________________________
# 4. Determine days predicted ----
#_____________________________________________________________________________________________________________
# 4a. Parturient elk ----
#_____________________________________________________________________________________________________________

elk.1.predictions <- as.data.frame(predict(elk.model, type = "prob"))

elk.1.pred.prob <- elk.1.predictions[ ,2]

elk.part.complete <- elk.part[complete.cases(elk.part), ]

elk.part.complete <- cbind(elk.part.complete, elk.1.pred.prob)

elk.part.days <- elk.part.complete %>% group_by(CollarID) %>%
                                       summarize(n())

# summaries
mean(elk.part.days$`n()`)
sd(elk.part.days$`n()`) / sqrt(nrow(elk.part.days))
range(elk.part.days$`n()`)

#_____________________________________________________________________________________________________________
# 4b. Non-parturient elk ----
#_____________________________________________________________________________________________________________

# predict on np data
pred.np <- as.data.frame(predict(elk.model, newdata = elk.np, type = "prob"))

# bind prob of "1" to main df
pred.np.prob <- pred.np[ ,2]

elk.np <- cbind(elk.np, pred.np.prob)

elk.np.complete <- elk.np[complete.cases(elk.np), ]

elk.np.days <- elk.np.complete %>% group_by(CollarID) %>%
                                   summarize(n())

# summaries
mean(elk.np.days$`n()`)
sd(elk.np.days$`n()`) / sqrt(nrow(elk.np.days))
range(elk.np.days$`n()`)

#_____________________________________________________________________________________________________________
# 4c. Unknowns ----
#_____________________________________________________________________________________________________________

# predict on np data
pred.unk <- as.data.frame(predict(elk.model, newdata = elk.unknowns, type = "prob"))

# bind prob of "1" to main df
pred.unk.prob <- pred.unk[ ,2]

elk.unk <- cbind(elk.unknowns, pred.unk.prob)

elk.unk.complete <- elk.unknowns[complete.cases(elk.unknowns), ]

elk.unk.days <- elk.unk.complete %>% group_by(CollarID) %>%
                                     summarize(n())

# summaries
mean(elk.unk.days$`n()`)
sd(elk.unk.days$`n()`) / sqrt(nrow(elk.np.days))
range(elk.unk.days$`n()`)

#_____________________________________________________________________________________________________________
# 4d. 2020 collars in 2021 ----
#_____________________________________________________________________________________________________________

# predict on unk data
pred.thisyear <- as.data.frame(predict(elk.model, newdata = elk.thisyear, type = "prob"))

# bind prob of "1" to main df
pred.thisyear.prob <- pred.thisyear[ ,2]

elk.thisyear <- cbind(elk.thisyear, pred.thisyear.prob)

elk.thisyear.complete <- elk.thisyear[complete.cases(elk.thisyear), ]

elk.thisyear.days <- elk.thisyear.complete %>% group_by(CollarID) %>%
                                               summarize(n())

# summaries
mean(elk.thisyear.days$`n()`)
sd(elk.thisyear.days$`n()`) / sqrt(nrow(elk.np.days))
range(elk.thisyear.days$`n()`)

#_____________________________________________________________________________________________________________
# 5. Overall fix success ----
#_____________________________________________________________________________________________________________

# add year and group columns
elk.1.2020$Year <- 2020
elk.1.2020$Group <- "part"

elk.2.2020$Year <- 2020
elk.2.2020$Group <- "np"

elk.1.2021$Year <- 2021
elk.1.2021$Group <- "part"

elk.2.2021$Year <- 2021
elk.2.2021$Group <- "np"

elk.3.2020$Year <- 2020
elk.3.2020$Group <- "unk"

elk.3.2021$Year <- 2021
elk.3.2021$Group <- "unk"

elk.4.2021$Year <- 2021
elk.4.2021$Group <- "thisyear"

all.elk.fixes <- rbind(elk.1.2020, elk.2.2020, elk.1.2021, elk.2.2021, elk.3.2020, elk.3.2021, elk.4.2021)

# summarize based upon collar brand
all.elk.fixes %>% group_by(brand) %>%
                  summarize(number = n(),
                            mean.fix.suc = mean(fix.suc),
                            se.fix.suc = sd(fix.suc) / sqrt(n()),
                            low.range.fix.suc = range(fix.suc)[1],
                            high.range.fix.suc = range(fix.suc)[2])

#_____________________________________________________________________________________________________________
# 6. Overall prediction days ----
#_____________________________________________________________________________________________________________

# add group
elk.part.days$Group <- "part"
elk.np.days$Group <- "np"
elk.unk.days$Group <- "unk"
elk.thisyear.days$Group <- "thisyear"

# bind together
elk.days <- rbind(elk.part.days, elk.np.days, elk.unk.days, elk.thisyear.days)

# merge with all.elk.fixes
all.elk <- merge(all.elk.fixes, elk.days, by.x = c("CollarID", "Group"))

# summarize based upon collar brand
all.elk %>% group_by(brand) %>%
            summarize(number = n(),
                      mean.n = mean(`n()`),
                      se.n = sd(`n()`) / sqrt(n()),
                      low.range.n = range(`n()`)[1],
                      high.range.n = range(`n()`)[2])

#_____________________________________________________________________________________________________________
# 7. Multiple grouping factors for table ----
#_____________________________________________________________________________________________________________

all.elk.summary <- all.elk %>% group_by(brand, Group, Year) %>%
                               summarize(number = n(),
                                         mean.fix.suc = mean(fix.suc),
                                         se.fix.suc = sd(fix.suc) / sqrt(n()),
                                         low.range.fix.suc = range(fix.suc)[1],
                                         high.range.fix.suc = range(fix.suc)[2],
                                         mean.n = mean(`n()`),
                                         se.n = sd(`n()`) / sqrt(n()),
                                         low.range.n = range(`n()`)[1],
                                         high.range.n = range(`n()`)[2])

all.elk.summary

write.table(all.elk.summary, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8. Total fixes ----
#_____________________________________________________________________________________________________________

sum(all.elk$n.fixes)

mean(all.elk$fix.suc)
range(all.elk$fix.suc)

mean(all.elk$n.fixes)
sd(all.elk$n.fixes)/ sqrt(nrow(all.elk))

mean(all.elk$`n()`)
sd(all.elk$`n()`) / sqrt(nrow((all.elk)))
range(all.elk$`n()`)

# correlation
plot(all.elk$fix.suc, all.elk$`n()`)

cor.test(all.elk$fix.suc, all.elk$`n()`, method = "spearman")
