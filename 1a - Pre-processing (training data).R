# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 1a - Pre-processing (parturient elk for training the model)
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 12 Apr 2021
# Date completed: 12 Apr 2021
# Date modified: 1 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(sp)             # work with spatial objects
library(amt)            # work with steps
library(adehabitatHR)   # fit MCPs
library(adehabitatLT)   # first passage time
library(mefa4)          # notin function

#_____________________________________________________________________________________________________________
# 2. Read in and rbind relocation data ----
#_____________________________________________________________________________________________________________

vectronic.data <- read.csv("D:/Elk project/Data analysis/Raw data processing/Relocations_vectronic_1.csv")

# get dates into correct formats
vectronic.data$t <- as.POSIXct(vectronic.data$t, tz = "America/New_York")

# rbind both data frames together
elk.data <- vectronic.data

# define projection
projection <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

# define start dates
startdate.2020 <- as.POSIXct("2020-01-01 00:00:00", tz = "America/New_York")
startdate.2021 <- as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York")

#_____________________________________________________________________________________________________________
# 3. Mean step length (3 days) ----
#_____________________________________________________________________________________________________________
# 3a. 2020 - Elk that gave birth and had a calf survive to a week old ----

# Here we're interested in May 15-Jul 15, so we're using DOY: 136-197

#_____________________________________________________________________________________________________________

# define elk that gave birth
elk.births.2020 <- as.vector(c("37705", "37706", "37708", "37709", "37710", "37711", "37712", 
                               "37714", "37715", "37716", "37717", "37718", "37719", "37720", 
                               "37722", "37723", "37724", "37725", "37726", "37727"))

# define timeframe
start.time.1 <- as.POSIXct("2020-05-15 00:00:00")
end.time.1 <- as.POSIXct("2020-07-15 23:59:59")

# for loop to run through each elk that gave birth
elk.birth.steps.2020 <- data.frame()

for (x in elk.births.2020) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  indiv.track <- indiv.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(indiv.track)[which(colnames(indiv.track) == "burst")] <- "burst_"
  
  # make steps 
  indiv.steps <- indiv.track %>% steps_by_burst()
  
  # filter steps (1 day previous to the first focal day and one day after the last)
  indiv.steps.1 <- indiv.steps %>% filter(t1_ < (end.time.1 + 24*60*60) & t1_ >= (start.time.1 - 24*60*60))
  
  # aggregate and sum steps per day (daily distance traveled in m)
  indiv.steps.1.summary <- indiv.steps.1 %>% mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                             group_by(day) %>%
                                             summarize(sl.avg = mean(sl_)) %>%
                                             mutate(DOY = as.integer(difftime(day, startdate.2020, units = "days")) + 1)
  
  # remove 'day' variable
  indiv.steps.1.summary <- indiv.steps.1.summary %>% dplyr::select(DOY, sl.avg)
  
  # if one of the DOYs in the sequence isn't there, add a blank column
  for (y in 136:197) {
    
    if (y %notin% indiv.steps.1.summary$DOY) {
      indiv.steps.1.summary <- rbind(indiv.steps.1.summary, data.frame(DOY = y, sl.avg = NA))
      }
    
  }
  
  # add day and DOY column
  indiv.steps.1 <- indiv.steps.1 %>% mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                            mutate(DOY = as.integer(difftime(day, startdate.2020, units = "days")) + 1)
  
  # compute mean step length within a 3-day moving window
  indiv.steps.1.summary <- data.frame()
  
  # for loop which calculates 3-day averages of average daily sl
  for (z in 136:197) {
    
    # subset data
    focal.steps <- indiv.steps.1 %>% filter(DOY %in% c(z - 1, z, z + 1))
    
    # calculate mean sl for focal period
    focal.mean <- mean(focal.steps$sl_, na.rm = TRUE)
    
    # bind into a df with the DOY
    focal.summary <- data.frame(Animal = CollarID,
                                sl.3day = focal.mean,
                                DOY = z)
    
    # bind to master df
    indiv.steps.1.summary <- rbind(indiv.steps.1.summary, focal.summary)
    
  }
  
  # keep only rows we need
  indiv.steps.1.summary <- indiv.steps.1.summary %>% dplyr::filter(DOY >= 136 & DOY <= 197)
  
  elk.birth.steps.2020 <- rbind(elk.birth.steps.2020, indiv.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 3b. 2021 - Elk that gave birth and had a calf survive to a week old ----

# Here we're interested in May 15-Jul 15, so we're using DOY: 135-196

#_____________________________________________________________________________________________________________

# define elk that gave birth
elk.births.2021 <- as.vector(c(45492, 45493, 45494, 45495, 45496, 45498, 45500, 45501, 45505,
                               45507, 45509, 45510, 45511, 46393, 46394, 46397,  46400, 46401, 46402))

# define timeframe
start.time.2 <- as.POSIXct("2021-05-15 00:00:00")
end.time.2 <- as.POSIXct("2021-07-15 23:59:59")

# for loop to run through each elk that gave birth
elk.birth.steps.2021 <- data.frame()

for (x in elk.births.2021) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  indiv.track <- indiv.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(indiv.track)[which(colnames(indiv.track) == "burst")] <- "burst_"
  
  # make steps 
  indiv.steps <- indiv.track %>% steps_by_burst()
  
  # filter steps (1 day previous to the first focal day and one day after the last)
  indiv.steps.1 <- indiv.steps %>% filter(t1_ < (end.time.2 + 24*60*60) & t1_ >= (start.time.2 - 24*60*60))
  
  # aggregate and sum steps per day (daily distance traveled in m)
  indiv.steps.1.summary <- indiv.steps.1 %>% mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                             group_by(day) %>%
                                             summarize(sl.avg = mean(sl_)) %>%
                                             mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # remove 'day' variable
  indiv.steps.1.summary <- indiv.steps.1.summary %>% dplyr::select(DOY, sl.avg)
  
  # if one of the DOYs in the sequence isn't there, add a blank column
  for (y in 135:196) {
    
    if (y %notin% indiv.steps.1.summary$DOY) {
      indiv.steps.1.summary <- rbind(indiv.steps.1.summary, data.frame(DOY = y, sl.avg = NA))
      
      }
    
  }
  
  # add day and DOY column
  indiv.steps.1 <- indiv.steps.1 %>% mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                            mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # compute mean step length within a 3-day moving window
  indiv.steps.1.summary <- data.frame()
  
  # for loop which calculates 3-day averages of average daily sl
  for (z in 135:196) {
    
    # subset data
    focal.steps <- indiv.steps.1 %>% filter(DOY %in% c(z - 1, z, z + 1))
    
    # calculate mean sl for focal period
    focal.mean <- mean(focal.steps$sl_, na.rm = TRUE)
    
    # bind into a df with the DOY
    focal.summary <- data.frame(Animal = CollarID,
                                sl.3day = focal.mean,
                                DOY = z)
    
    # bind to master df
    indiv.steps.1.summary <- rbind(indiv.steps.1.summary, focal.summary)
    
  }
  
  # keep only rows we need
  indiv.steps.1.summary <- indiv.steps.1.summary %>% dplyr::filter(DOY >= 135 & DOY <= 196)
  
  elk.birth.steps.2021 <- rbind(elk.birth.steps.2021, indiv.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 4. rMCPs ----
#_____________________________________________________________________________________________________________
# 4a. Elk that gave birth in 2020 ----
#_____________________________________________________________________________________________________________

# for loop to run through each elk that gave birth
elk.birth.mcp.2020 <- data.frame()

for (x in elk.births.2020) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # create "day" column
  indiv.data <- indiv.data %>% mutate(day = as.Date(t, tz = "America/New_York")) %>%
                               mutate(DOY = as.integer(difftime(day, startdate.2020, units = "days")) + 1)
  
  # define how many days to use for the MCP
  mcp.days <- 11
  mcp.ends <- (mcp.days - 1) / 2
  
  # filter steps to calving season range (+/- 5 days to get our 11-day windows)
  indiv.data.1 <- indiv.data %>% filter(t < (end.time.1 + mcp.ends*60*60*24) & t >= (start.time.1 - mcp.ends*60*60*24))
  
  # calculate 100% MCP areas and add to data frame
  Win.MCP <- data.frame(MCP = NA,
                        DOY = 136:197)
  
  for (p in 136:197) {
    
    # define relocations for moving window p
    indiv.data.2 <- indiv.data.1 %>% dplyr::filter(DOY >= (p - mcp.ends) & DOY <= (p + mcp.ends))
    
    # fit MCP to relocations
    focal.sp <- SpatialPoints(coords = indiv.data.2[ ,c("x", "y")], 
                              proj4string = projection)
    
    focal.mcp <- mcp.area(focal.sp, percent = 100, unin = "m", unout = "km2", plotit = FALSE)
    
    Win.MCP$MCP[Win.MCP$DOY == p] <- focal.mcp[1, 1]
    
  }
  
  # add CollarID and bind to master data frame
  Win.MCP <- Win.MCP %>% mutate(CollarID = CollarID)
  
  # bind to master df
  elk.birth.mcp.2020 <- rbind(elk.birth.mcp.2020, Win.MCP)
  
}

#_____________________________________________________________________________________________________________
# 4b. Elk that gave birth in 2021 ----
#_____________________________________________________________________________________________________________

# for loop to run through each elk that gave birth
elk.birth.mcp.2021 <- data.frame()

for (x in elk.births.2021) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # create "day" column
  indiv.data <- indiv.data %>% mutate(day = as.Date(t, tz = "America/New_York")) %>%
                               mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # define how many days to use for the MCP
  mcp.days <- 11
  mcp.ends <- (mcp.days - 1) / 2
  
  # filter steps to calving season range (+/- 5 days to get our 11-day windows)
  indiv.data.1 <- indiv.data %>% filter(t < (end.time.2 + mcp.ends*60*60*24) & t >= (start.time.2 - mcp.ends*60*60*24))
  
  # calculate 100% MCP areas and add to data frame
  Win.MCP <- data.frame(MCP = NA,
                        DOY = 135:196)
  
  for (p in 135:196) {
    
    # define relocations for moving window p
    indiv.data.2 <- indiv.data.1 %>% dplyr::filter(DOY >= (p - mcp.ends) & DOY <= (p + mcp.ends))
    
    # fit MCP to relocations
    focal.sp <- SpatialPoints(coords = indiv.data.2[ ,c("x", "y")], 
                              proj4string = projection)
    
    focal.mcp <- mcp.area(focal.sp, percent = 100, unin = "m", unout = "km2", plotit = FALSE)
    
    Win.MCP$MCP[Win.MCP$DOY == p] <- focal.mcp[1, 1]
    
  }
  
  # add CollarID and bind to master data frame
  Win.MCP <- Win.MCP %>% mutate(CollarID = CollarID)
  
  # bind to master df
  elk.birth.mcp.2021 <- rbind(elk.birth.mcp.2021, Win.MCP)
  
}

#_____________________________________________________________________________________________________________
# 5. Mean step length - 7 days after focal day ----
#_____________________________________________________________________________________________________________
# 5a. 2020 - Elk that gave birth and had a calf survive to a week old ----
#_____________________________________________________________________________________________________________

# for loop to run through each elk that gave birth
elk.calf.steps.2020 <- data.frame()

for (x in elk.births.2020) {
  
  CollarID <- x
  
  calf.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  calf.track <- calf.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(calf.track)[which(colnames(calf.track) == "burst")] <- "burst_"
  
  # make steps 
  calf.steps <- calf.track %>% steps_by_burst()
  
  # filter steps (7 days after the last) and create DOY variable
  calf.steps.1 <- calf.steps %>% filter(t1_ < (end.time.1 + 7*24*60*60) & t1_ >= (start.time.1)) %>%
                                 mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                 mutate(DOY = as.integer(difftime(day, startdate.2020, units = "days")) + 1)
  
  # calculate average step length over the 7 day period
  calf.steps.1.summary <- data.frame(DOY = 136:197,
                                     sl.post7 = NA)
  
  for (q in 136:197) {
    
    focal.calf <- calf.steps.1 %>% dplyr::filter(DOY > q & DOY <= (q + 7))
    
    focal.calf.avg <- mean(focal.calf$sl_, na.rm = TRUE)
    
    calf.steps.1.summary$sl.post7[calf.steps.1.summary$DOY == q] <- focal.calf.avg
    
  }
  
  # add CollarID, scale, and bind to master data frame
  calf.steps.1.summary <- calf.steps.1.summary %>% mutate(CollarID = CollarID)
  
  elk.calf.steps.2020 <- rbind(elk.calf.steps.2020, calf.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 5b. 2021 - Elk that gave birth and had a calf survive to a week old ----
#_____________________________________________________________________________________________________________

# for loop to run through each elk that gave birth
elk.calf.steps.2021 <- data.frame()

for (x in elk.births.2021) {
  
  CollarID <- x
  
  calf.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  calf.track <- calf.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(calf.track)[which(colnames(calf.track) == "burst")] <- "burst_"
  
  # make steps 
  calf.steps <- calf.track %>% steps_by_burst()
  
  # filter steps (7 days after the last) and create DOY variable
  calf.steps.1 <- calf.steps %>% filter(t1_ < (end.time.2 + 7*24*60*60) & t1_ >= (start.time.2)) %>%
                                 mutate(day = as.Date(t1_, tz = "America/New_York")) %>%
                                 mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # calculate average step length over the 7 day period
  calf.steps.1.summary <- data.frame(DOY = 135:196,
                                     sl.post7 = NA)
  
  for (q in 135:196) {
    
    focal.calf <- calf.steps.1 %>% dplyr::filter(DOY > q & DOY <= (q + 7))
    
    focal.calf.avg <- mean(focal.calf$sl_, na.rm = TRUE)
    
    calf.steps.1.summary$sl.post7[calf.steps.1.summary$DOY == q] <- focal.calf.avg
    
  }
  
  # add CollarID, scale, and bind to master data frame
  calf.steps.1.summary <- calf.steps.1.summary %>% mutate(CollarID = CollarID)
  
  elk.calf.steps.2021 <- rbind(elk.calf.steps.2021, calf.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 5. Merge all dataframes ----
#_____________________________________________________________________________________________________________

# 2020
elk.birth.2020 <- data.frame(DOY = 136:197,
                             CollarID = elk.birth.steps.2020$Animal,
                             sl.3day = elk.birth.steps.2020$sl.3day,
                             sl.post7 = elk.calf.steps.2020$sl.post7,
                             mcp = elk.birth.mcp.2020$MCP)

# 2021
elk.birth.2021 <- data.frame(DOY = 135:196,
                             CollarID = elk.birth.steps.2021$Animal,
                             sl.3day = elk.birth.steps.2021$sl.3day,
                             sl.post7 = elk.calf.steps.2021$sl.post7,
                             mcp = elk.birth.mcp.2021$MCP)

#_____________________________________________________________________________________________________________
# 7. Add in actual parturition dates ----
#_____________________________________________________________________________________________________________
# 7a. Elk that gave birth in 2020 ----
#_____________________________________________________________________________________________________________

# define parturition dates
elk.part.dates.2020 <- data.frame("CollarID" = elk.births.2020,
                                  "Part.date" = as.vector(c(158, 153, 138, 144, 149, 161, 175, 152, 160, 157, 160, 152,
                                                            151, 143, 146, 166, 160, 163, 147, 144)))

# create blank dataframe
elk.days.part.2020 <- data.frame()

# for loop
for (z in elk.births.2020) {
  
  elkID <- z
  
  # extract only this collar's data
  indiv.birth.2020 <- elk.birth.2020 %>% filter(CollarID == elkID)
  
  # part.date
  part.date <- elk.part.dates.2020$Part.date[elk.part.dates.2020$CollarID == elkID]
  
  # create "days.part" variable
  indiv.birth.2020 <- indiv.birth.2020 %>% mutate(days.part = DOY - part.date)
  
  # create binary variable
  indiv.birth.2020 <- indiv.birth.2020 %>% mutate(part.case = ifelse(days.part == 0, 1, 0))
  
  # bind to master data frame
  elk.days.part.2020 <- rbind(elk.days.part.2020, indiv.birth.2020)
  
}

#_____________________________________________________________________________________________________________
# 7b. Elk that gave birth in 2021 ----
#_____________________________________________________________________________________________________________

# define parturition dates
elk.part.dates.2021 <- data.frame("CollarID" = elk.births.2021,
                                  "Part.date" = as.vector(c(164, 181, 147, 150, 150, 190, 171, 144, 144,
                                                            163, 174, 196, 141, 146, 149, 164, 158, 166, 152)))

# create blank dataframe
elk.days.part.2021 <- data.frame()

# for loop
for (z in elk.births.2021) {
  
  elkID <- z
  
  # extract only this collar's data
  indiv.birth.2021 <- elk.birth.2021 %>% filter(CollarID == elkID)
  
  # part.date
  part.date <- elk.part.dates.2021$Part.date[elk.part.dates.2021$CollarID == elkID]
  
  # create "days.part" variable
  indiv.birth.2021 <- indiv.birth.2021 %>% mutate(days.part = DOY - part.date)
  
  # create binary variable
  indiv.birth.2021 <- indiv.birth.2021 %>% mutate(part.case = ifelse(days.part == 0, 1, 0))
  
  # bind to master data frame
  elk.days.part.2021 <- rbind(elk.days.part.2021, indiv.birth.2021)
  
}

# change CollarID to factor
elk.days.part.2021$CollarID <- as.factor(elk.days.part.2021$CollarID)

#_____________________________________________________________________________________________________________
# 8. rbind together and write to .csv ----
#_____________________________________________________________________________________________________________

elk.days.part <- rbind(elk.days.part.2020, elk.days.part.2021)

# write to csv
write.csv(elk.days.part, "elk_days_part.csv")
