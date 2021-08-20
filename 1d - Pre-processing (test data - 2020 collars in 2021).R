# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 1d - Pre-processing (test data - 2020 collars in 2021)
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 2 Jun 2021
# Date completed: 8 Jul 2021
# Date modified: 1 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(sp)             # work with spatial objects
library(amt)            # work with steps
library(adehabitatHR)   # fit MCPs
library(mefa4)          # notin function

#_____________________________________________________________________________________________________________
# 2. Read in and rbind relocation data ----
#_____________________________________________________________________________________________________________

elk.data <- read.csv("D:/Elk project/Data analysis/Raw data processing/Relocations_vectronic_1.csv")

# get dates into correct formats
elk.data$t <- as.POSIXct(elk.data$t, tz = "America/New_York")

# define projection
projection <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

# define start date
startdate.2021 <- as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York")

#_____________________________________________________________________________________________________________
# 3. Mean step length (3 days) ----

# Here we're interested in May 15-Jul 15, so we're using DOY: 135-196

#_____________________________________________________________________________________________________________

# define Vectronic elk still alive through 2021 calving season
elk.thisyear.2021 <- as.vector(c(37703, 37704, 37705, 37706, 37707, 37708, 37709, 37710, 37711,
                                 37712, 37714, 37716, 37718, 37719, 37720, 37722, 37723, 37725, 37726))

# define timeframe
start.time <- as.POSIXct("2021-05-15 00:00:00")
end.time <- as.POSIXct("2021-07-15 23:59:59")

# for loop 
elk.thisyear.steps.2021 <- data.frame()

for (x in elk.thisyear.2021) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  indiv.track <- indiv.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(indiv.track)[which(colnames(indiv.track) == "burst")] <- "burst_"
  
  # make steps 
  indiv.steps <- indiv.track %>% steps_by_burst()
  
  # filter steps (1 day previous to the first focal day and one day after the last)
  indiv.steps.1 <- indiv.steps %>% filter(t1_ < (end.time + 24*60*60) & t1_ >= (start.time - 24*60*60))
  
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
  
  elk.thisyear.steps.2021 <- rbind(elk.thisyear.steps.2021, indiv.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 4. rMCPs ----
#_____________________________________________________________________________________________________________

elk.thisyear.mcp.2021 <- data.frame()

for (x in elk.thisyear.2021) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # create "day" column
  indiv.data <- indiv.data %>% mutate(day = as.Date(t, tz = "America/New_York")) %>%
                               mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # define how many days to use for the MCP
  mcp.days <- 11
  mcp.ends <- (mcp.days - 1) / 2
  
  # filter steps to calving season range (+/- 5 days to get our 11-day windows)
  indiv.data.1 <- indiv.data %>% filter(t < (end.time + mcp.ends*60*60*24) & t >= (start.time - mcp.ends*60*60*24))
  
  # calculate 100% MCP areas and add to data frame
  Win.MCP <- data.frame(MCP = NA,
                        DOY = 135:196)
  
  for (p in 135:196) {
    
    # define relocations for moving window p
    indiv.data.2 <- indiv.data.1 %>% dplyr::filter(DOY >= (p - mcp.ends) & DOY <= (p + mcp.ends))
    
    # if else statement to bypass creating SP, mcp, etc.
    if (nrow(indiv.data.2) > 0) {
      
        # fit MCP to relocations
        focal.sp <- SpatialPoints(coords = indiv.data.2[ ,c("x", "y")], 
                                  proj4string = projection)
        
        focal.mcp <- ifelse(nrow(focal.sp@coords) > 4,
                            mcp.area(focal.sp, percent = 100, unin = "m", unout = "km2", plotit = FALSE),
                            NA)
        
        Win.MCP$MCP[Win.MCP$DOY == p] <- ifelse(nrow(focal.sp@coords) > 4,
                                                     focal.mcp[[1]],
                                                     NA)
      
      
    } else {
      
      Win.MCP$MCP[Win.MCP$DOY == p] <- NA
      
    }
    
  }
  
  # add CollarID and bind to master data frame
  Win.MCP <- Win.MCP %>% mutate(CollarID = CollarID)
  
  # bind to master df
  elk.thisyear.mcp.2021 <- rbind(elk.thisyear.mcp.2021, Win.MCP)
  
}

#_____________________________________________________________________________________________________________
# 5. Mean step length - 7 days after focal day ----
#_____________________________________________________________________________________________________________

elk.thisyear.calf.steps.2021 <- data.frame()

for (x in elk.thisyear.2021) {
  
  CollarID <- x
  
  calf.data <- elk.data %>% filter(Animal == CollarID)
  
  # make a track
  calf.track <- calf.data %>% make_track(.x = x, .y = y, .t = t, all_cols = TRUE)
  
  # rename burst column
  colnames(calf.track)[which(colnames(calf.track) == "burst")] <- "burst_"
  
  # make steps 
  calf.steps <- calf.track %>% steps_by_burst()
  
  # filter steps (7 days after the last) and create DOY variable
  calf.steps.1 <- calf.steps %>% filter(t1_ < (end.time + 7*24*60*60) & t1_ >= (start.time)) %>%
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
  
  elk.thisyear.calf.steps.2021 <- rbind(elk.thisyear.calf.steps.2021, calf.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 5. Merge all dataframes ----
#_____________________________________________________________________________________________________________

# 2021
elk.thisyear.2021 <- data.frame(DOY = 135:196,
                                CollarID = elk.thisyear.steps.2021$Animal,
                                sl.3day = elk.thisyear.steps.2021$sl.3day,
                                sl.post7 = elk.thisyear.calf.steps.2021$sl.post7,
                                mcp = elk.thisyear.mcp.2021$MCP)

#_____________________________________________________________________________________________________________
# 8. Write to .csv ----
#_____________________________________________________________________________________________________________

write.csv(elk.thisyear.2021, "elk_thisyear_2021.csv")
