# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 6 - Fix success and days predicted
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 13 Aug 2021
# Date completed: 8 Sep 2021
# Date modified: 10 Sep 2021
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
elk.part.all <- read.csv("elk_days_part.csv")

# remove 37718 and 45510
elk.part <- elk.part.all %>% filter(CollarID %notin% c(37718, 45510))

# non-pregnant/lost calf/outside of window
elk.np <- read.csv("elk_np.csv")

# unknowns 
elk.unknowns <- read.csv("elk_unknowns.csv")
elk.unknowns <- rbind(elk.unknowns, elk.part.all %>% filter(CollarID == 37718) %>% dplyr::select(1:6))

# 2020 collars in 2021
elk.thisyear <- read.csv("elk_thisyear_2021.csv")

# other collar (successful but fix success declined)
elk.other <- elk.part.all %>% filter(CollarID == 45510)

#_____________________________________________________________________________________________________________
# 3. Determine fix success rate ----
#_____________________________________________________________________________________________________________
# 3a. Parturient elk ----
#_____________________________________________________________________________________________________________

# 2020
# define elk that gave birth
elk.births.2020 <- unique(elk.part$CollarID)[1:19]

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
elk.births.2021 <- unique(elk.part$CollarID)[20:37]

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
elk.np.2020 <- unique(elk.np$CollarID)[1:13]

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
elk.np.2021 <- unique(elk.np$CollarID)[14:30]

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
elk.unknowns.2020 <- unique(elk.unknowns$CollarID)[c(1:22, 32)]

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
elk.unknowns.2021 <- unique(elk.unknowns$CollarID)[23:31]

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
elk.thisyear.2021 <- unique(elk.thisyear$CollarID)

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
# 3e. Other elk ----
#_____________________________________________________________________________________________________________

# define timeframe
start.time.2 <- as.POSIXct("2021-05-15 00:00:00")
end.time.2 <- as.POSIXct("2021-07-15 23:59:59")

# determine number of hours within time period (start - 1 and end + 7)
total.time.2 <- as.numeric((end.time.2 + 7*24*60*60) - (start.time.2 - 1*24*60*60)) * 24

elkID <- 45510

indiv.data <- elk.data %>% filter(Animal == elkID & t > (start.time.2 - 1*24*60*60) & t < (end.time.2 + 7*24*60*60))

# number of fixes
n.fixes <- nrow(indiv.data)

# expected number of fixes
ex.fixes <- round(total.time.2 / 13)

# fix success
fix.suc <- ifelse(indiv.data$brand[1] == "Vectronic", n.fixes / 125, n.fixes / ex.fixes)

# bind to df
elk.5.2021 <- data.frame("CollarID" = elkID, 
                         "n.fixes" = n.fixes,
                         "ex.fixes" = ifelse(indiv.data$brand[1] == "Vectronic", 125, ex.fixes),
                         "fix.suc" = fix.suc,
                         "brand" = indiv.data$brand[1])

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
                                       summarize(n = n())

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
elk.np.incomplete <- elk.np[!complete.cases(elk.np), ]

elk.np.days <- elk.np.complete %>% count(CollarID)

# add those with no prediction days
elk.np.days.0days <- elk.np.incomplete %>% count(CollarID) %>%
                                           filter(n == 62) %>%
                                           dplyr::select(CollarID) %>%
                                           mutate(n = 0)

elk.np.days <- rbind(elk.np.days, elk.np.days.0days)
                                   

# summaries
mean(elk.np.days$n)
sd(elk.np.days$n) / sqrt(nrow(elk.np.days))
range(elk.np.days$n)

#_____________________________________________________________________________________________________________
# 4c. Unknowns ----
#_____________________________________________________________________________________________________________

# predict on np data
pred.unk <- as.data.frame(predict(elk.model, newdata = elk.unknowns, type = "prob"))

# bind prob of "1" to main df
pred.unk.prob <- pred.unk[ ,2]

elk.unk <- cbind(elk.unknowns, pred.unk.prob)

elk.unk.complete <- elk.unknowns[complete.cases(elk.unknowns), ]
elk.unk.incomplete <- elk.unknowns[!complete.cases(elk.unknowns), ]

elk.unk.days <- elk.unk.complete %>% count(CollarID)

# add those with no prediction days
elk.unk.days.0days <- elk.unk.incomplete %>% count(CollarID) %>%
                                           filter(n == 62) %>%
                                           dplyr::select(CollarID) %>%
                                           mutate(n = 0)

elk.unk.days <- rbind(elk.unk.days, elk.unk.days.0days)

# summaries
mean(elk.unk.days$n)
sd(elk.unk.days$n) / sqrt(nrow(elk.np.days))
range(elk.unk.days$n)

#_____________________________________________________________________________________________________________
# 4d. 2020 collars in 2021 ----
#_____________________________________________________________________________________________________________

# predict on unk data
pred.thisyear <- as.data.frame(predict(elk.model, newdata = elk.thisyear, type = "prob"))

# bind prob of "1" to main df
pred.thisyear.prob <- pred.thisyear[ ,2]

elk.thisyear <- cbind(elk.thisyear, pred.thisyear.prob)

elk.thisyear.complete <- elk.thisyear[complete.cases(elk.thisyear), ]
elk.thisyear.incomplete <- elk.thisyear[!complete.cases(elk.thisyear), ]

elk.thisyear.days <- elk.thisyear.complete %>% count(CollarID)

# add those with no prediction days
elk.thisyear.days.0days <- elk.thisyear.incomplete %>% count(CollarID) %>%
                                           filter(n == 62) %>%
                                           dplyr::select(CollarID) %>%
                                           mutate(n = 0)

elk.thisyear.days <- rbind(elk.thisyear.days, elk.thisyear.days.0days)

# summaries
mean(elk.thisyear.days$n)
sd(elk.thisyear.days$n) / sqrt(nrow(elk.np.days))
range(elk.thisyear.days$n)

#_____________________________________________________________________________________________________________
# 4e. Other collar ----
#_____________________________________________________________________________________________________________

# predict on unk data
pred.other <- as.data.frame(predict(elk.model, newdata = elk.other, type = "prob"))

# bind prob of "1" to main df
pred.other.prob <- pred.other[ ,2]

elk.other <- cbind(elk.other, pred.other.prob)

elk.other.complete <- elk.other[complete.cases(elk.other), ]

elk.other.days <- elk.other.complete %>% group_by(CollarID) %>%
                                         summarize(n = n())

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

elk.5.2021$Year <- 2021
elk.5.2021$Group <- "other"

all.elk.fixes <- rbind(elk.1.2020, elk.2.2020, elk.1.2021, elk.2.2021, elk.3.2020, elk.3.2021, elk.4.2021, elk.5.2021)

# summarize based upon collar brand
all.elk.fixes %>% group_by(brand) %>%
                  summarize(number = n(),
                            mean.fix.suc = mean(fix.suc),
                            se.fix.suc = sd(fix.suc) / sqrt(n()),
                            low.range.fix.suc = range(fix.suc)[1],
                            high.range.fix.suc = range(fix.suc)[2])

# summarize based upon collar brand
all.elk.fixes %>% group_by(brand, Group, Year) %>%
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
elk.other.days$Group <- "other"

# bind together
elk.days <- rbind(elk.part.days, elk.np.days, elk.unk.days, elk.thisyear.days, elk.other.days)

# merge with all.elk.fixes
all.elk <- merge(all.elk.fixes, elk.days, by.x = c("CollarID", "Group"))

# summarize based upon collar brand
all.elk %>% group_by(brand) %>%
            summarize(number = n(),
                      mean.n = mean(n),
                      se.n = sd(n) / sqrt(n()),
                      low.range.n = range(n)[1],
                      high.range.n = range(n)[2])

#_____________________________________________________________________________________________________________
# 7. Multiple grouping factors for table ----
#_____________________________________________________________________________________________________________

all.elk.summary.1 <- all.elk %>% group_by(brand, Group, Year) %>%
                               summarize(number = n(),
                                         mean.fix.suc = mean(fix.suc),
                                         se.fix.suc = sd(fix.suc) / sqrt(n()),
                                         low.range.fix.suc = range(fix.suc)[1],
                                         high.range.fix.suc = range(fix.suc)[2],
                                         mean.n = mean(n),
                                         se.n = sd(n) / sqrt(n()),
                                         low.range.n = range(n)[1],
                                         high.range.n = range(n)[2])

all.elk.summary.1

write.table(all.elk.summary, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8. Total fixes ----
#_____________________________________________________________________________________________________________

sum(all.elk$n.fixes)

mean(all.elk$fix.suc)
range(all.elk$fix.suc)

# overall fix success
sum(all.elk$n.fixes) / sum(all.elk$ex.fixes)

mean(all.elk$n.fixes)
sd(all.elk$n.fixes)/ sqrt(nrow(all.elk))
range(all.elk$n.fixes)

mean(all.elk$n)
sd(all.elk$n) / sqrt(nrow((all.elk)))
range(all.elk$n)

# correlation
plot(all.elk$fix.suc, all.elk$n)

cor.test(all.elk$fix.suc, all.elk$n, method = "spearman")
