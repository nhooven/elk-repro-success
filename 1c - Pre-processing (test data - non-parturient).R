# Title: Parturition success 2 - RF models to determine probability of parturition
# Subtitle: 1c - Pre-processing (test data - non-parturient, early calf deaths, outside of window)
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 2 Jun 2021
# Date completed: 2 Jun 2021
# Date modified: 10 Aug 2021
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(sp)             # work with spatial objects
library(amt)            # work with steps
library(adehabitatHR)   # fit MCPs
library(mefa4)          # notin function
library(raster)         # read in rasters

#_____________________________________________________________________________________________________________
# 2. Read in and rbind relocation data ----
#_____________________________________________________________________________________________________________

vectronic.data <- read.csv("G:/Elk project/Data analysis/Raw data processing/Relocations_vectronic_1.csv")
lotek.data <- read.csv("G:/Elk project/Data analysis/Raw data processing/Relocations_lotek_1.csv")

# get dates into correct formats
vectronic.data$t <- as.POSIXct(vectronic.data$t, tz = "America/New_York")

lotek.data$t <- as.POSIXct(lotek.data$t, tz = "America/New_York")

# rbind both data frames together
elk.data <- rbind(vectronic.data, lotek.data)

# define projection
projection <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

# define start dates
startdate.2020 <- as.POSIXct("2020-01-01 00:00:00", tz = "America/New_York")
startdate.2021 <- as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York")

# include rasters for habitat data
raster.dir <- "G:/Elk project/Elk Zone rasters (7-20-21)"

# Read in each raster
canopy <- raster(paste0(raster.dir, "/", "canopy.tif"))

TRI <- raster(paste0(raster.dir, "/", "TRI_10.tif"))

dEdge <- raster(paste0(raster.dir, "/", "dEdge.tif"))

# define raster projection
raster.proj <- TRI@crs

#_____________________________________________________________________________________________________________
# 3. Mean step length (3 days) ----
#_____________________________________________________________________________________________________________
# 3a. 2020 - Non-parturient ----

# Here we're interested in May 10-Jun 30, so we're using DOY: 136-197

#_____________________________________________________________________________________________________________

# define elk who were not pregnant, whose calf died, or who gave birth outside of the calving season
elk.np.2020 <- as.vector(c(37704, 101957, 101965, 101972, 102489, 102491, 102497, 103172, 103174, 103175, 103179, 103181, 103182))

# define timeframe
start.time.1 <- as.POSIXct("2020-05-15 00:00:00")
end.time.1 <- as.POSIXct("2020-07-15 23:59:59")

elk.np.steps.2020 <- data.frame()

for (x in elk.np.2020) {
  
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
  
  elk.np.steps.2020 <- rbind(elk.np.steps.2020, indiv.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 3b. 2021 - Non-parturient ----

# Here we're interested in May 15-Jul 15, so we're using DOY: 135-196

#_____________________________________________________________________________________________________________

# define elk who were not pregnant, whose calf died, or who gave birth outside of the calving season
elk.np.2021 <- as.vector(c(46391, 46399, 45508, 46396, 45497, 46392, 45506,
                           45502, 45469, 101956, 101959, 103185, 103250, 45499, 103244, 103239, 103248))

# define timeframe
start.time.2 <- as.POSIXct("2021-05-15 00:00:00")
end.time.2 <- as.POSIXct("2021-07-15 23:59:59")

elk.np.steps.2021 <- data.frame()

for (x in elk.np.2021) {
  
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
  
  elk.np.steps.2021 <- rbind(elk.np.steps.2021, indiv.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 4. rMCPs ----
#_____________________________________________________________________________________________________________
# 4a. 2020 - Non-parturient ----
#_____________________________________________________________________________________________________________

elk.np.mcp.2020 <- data.frame()

for (x in elk.np.2020) {
  
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
  elk.np.mcp.2020 <- rbind(elk.np.mcp.2020, Win.MCP)
  
}

#_____________________________________________________________________________________________________________
# 4b. 2021 - Non-parturient ----
#_____________________________________________________________________________________________________________

elk.np.mcp.2021 <- data.frame()

for (x in elk.np.2021) {
  
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
  elk.np.mcp.2021 <- rbind(elk.np.mcp.2021, Win.MCP)
  
}

#_____________________________________________________________________________________________________________
# 5. Mean step length - 7 days after focal day ----
#_____________________________________________________________________________________________________________
# 5a. 2020 - Non-parturient ----
#_____________________________________________________________________________________________________________

elk.np.calf.steps.2020 <- data.frame()

for (x in elk.np.2020) {
  
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
  
  elk.np.calf.steps.2020 <- rbind(elk.np.calf.steps.2020, calf.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 5b. 2021 - Non-parturient ----
#_____________________________________________________________________________________________________________

elk.np.calf.steps.2021 <- data.frame()

for (x in elk.np.2021) {
  
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
  
  elk.np.calf.steps.2021 <- rbind(elk.np.calf.steps.2021, calf.steps.1.summary)
  
}

#_____________________________________________________________________________________________________________
# 6. Five-day average canopy cover, TRI, and dEdge ----
#_____________________________________________________________________________________________________________
# 6a. 2020 - Elk that did not give birth ----
#_____________________________________________________________________________________________________________

elk.np.2020 <- as.vector(c(37704, 101957, 101965, 101972, 102489, 102491, 102497, 103172, 103174, 103175, 103179, 103181, 103182))

# for loop to run through each elk that gave birth
elk.np.landscape.2020 <- data.frame()

for (x in elk.np.2020) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # filter days to all focal days
  indiv.data.1 <- indiv.data %>% filter(t < (end.time.1 + 2*24*60*60) & t >= (start.time.1 - 2*24*60*60))
  
  # create a SPDF
  indiv.spdf <- SpatialPointsDataFrame(coords = indiv.data.1[ ,c("x", "y")],
                                       proj4string = projection,
                                       data = indiv.data.1)
  
  # transform to correct crs
  indiv.spdf.proj <- spTransform(indiv.spdf, raster.proj)
  
  # sample from each raster
  indiv.spdf.proj@data$canopy <- raster::extract(canopy, indiv.spdf.proj, method = "simple")
  
  indiv.spdf.proj@data$TRI <- raster::extract(TRI, indiv.spdf.proj, method = "simple")
  
  indiv.spdf.proj@data$dEdge <- raster::extract(dEdge, indiv.spdf.proj, method = "simple")
  
  # convert to data.frame
  indiv.landscape <- indiv.spdf.proj@data
  
  # add day DOY variable
  indiv.landscape.summary <- indiv.landscape %>% mutate(day = as.Date(t, tz = "America/New_York")) %>%
    mutate(DOY = as.integer(difftime(day, startdate.2020, units = "days")) + 1)
  
  # remove 'day' variable and add CollarID
  indiv.landscape.summary <- indiv.landscape.summary  %>% dplyr::select(DOY, canopy, TRI, dEdge) %>%
    mutate(CollarID = CollarID)
  
  # if one of the DOYs in the sequence isn't there, add a blank column
  for (y in 134:199) {
    
    if (y %notin% indiv.landscape.summary$DOY) {
      indiv.landscape.summary <- rbind(indiv.landscape.summary, data.frame(DOY = y, 
                                                                           canopy = NA,
                                                                           TRI = NA, 
                                                                           dEdge = NA,
                                                                           CollarID = CollarID))
    }
    
  }
  
  # for loop which calculates 5-day averages of each variable
  indiv.landscape.summary.1 <- data.frame()
  
  for (z in 135:197) {
    
    # subset data
    focal.points <- indiv.landscape.summary %>% filter(DOY %in% c(z - 2, z, z + 2))
    
    # bind into a df with the DOY
    focal.summary <- data.frame(Animal = CollarID,
                                canopy.5day = mean(focal.points$canopy, na.rm = TRUE),
                                TRI.5day = mean(focal.points$TRI, na.rm = TRUE),
                                dEdge.5day = mean(focal.points$dEdge, na.rm = TRUE),
                                DOY = z)
    
    # bind to master df
    indiv.landscape.summary.1 <- rbind(indiv.landscape.summary.1, focal.summary)
    
  }
  
  # keep only those DOYs we need
  indiv.landscape.summary.1 <- indiv.landscape.summary.1 %>% filter(DOY > 135 & DOY < 198)
  
  elk.np.landscape.2020 <- rbind(elk.np.landscape.2020, indiv.landscape.summary.1)
  
}

#_____________________________________________________________________________________________________________
# 6b. 2021 - Elk that did not give birth ----
#_____________________________________________________________________________________________________________

# define elk who were not pregnant, whose calf died, or who gave birth outside of the calving season
elk.np.2021 <- as.vector(c(46391, 46399, 45508, 46396, 45497, 46392, 45506,
                           45502, 45469, 101956, 101959, 103185, 103250, 45499, 103244, 103239, 103248))

# for loop to run through each elk that gave birth
elk.np.landscape.2021 <- data.frame()

for (x in elk.np.2021) {
  
  CollarID <- x
  
  indiv.data <- elk.data %>% filter(Animal == CollarID)
  
  # filter days to all focal days
  indiv.data.1 <- indiv.data %>% filter(t < (end.time.2 + 2*24*60*60) & t >= (start.time.2 - 2*24*60*60))
  
  # create a SPDF
  indiv.spdf <- SpatialPointsDataFrame(coords = indiv.data.1[ ,c("x", "y")],
                                       proj4string = projection,
                                       data = indiv.data.1)
  
  # transform to correct crs
  indiv.spdf.proj <- spTransform(indiv.spdf, raster.proj)
  
  # sample from each raster
  indiv.spdf.proj@data$canopy <- raster::extract(canopy, indiv.spdf.proj, method = "simple")
  
  indiv.spdf.proj@data$TRI <- raster::extract(TRI, indiv.spdf.proj, method = "simple")
  
  indiv.spdf.proj@data$dEdge <- raster::extract(dEdge, indiv.spdf.proj, method = "simple")
  
  # convert to data.frame
  indiv.landscape <- indiv.spdf.proj@data
  
  # add day DOY variable
  indiv.landscape.summary <- indiv.landscape %>% mutate(day = as.Date(t, tz = "America/New_York")) %>%
    mutate(DOY = as.integer(difftime(day, startdate.2021, units = "days")) + 1)
  
  # remove 'day' variable and add CollarID
  indiv.landscape.summary <- indiv.landscape.summary %>% dplyr::select(DOY, canopy, TRI, dEdge) %>%
    mutate(CollarID = CollarID)
  
  # if one of the DOYs in the sequence isn't there, add a blank column
  for (y in 133:197) {
    
    if (y %notin% indiv.landscape.summary$DOY) {
      indiv.landscape.summary <- rbind(indiv.landscape.summary, data.frame(DOY = y, 
                                                                           canopy = NA,
                                                                           TRI = NA, 
                                                                           dEdge = NA,
                                                                           CollarID = CollarID))
    }
    
  }
  
  # for loop which calculates 5-day averages of each variable
  indiv.landscape.summary.1 <- data.frame()
  
  for (z in 133:197) {
    
    # subset data
    focal.points <- indiv.landscape.summary %>% filter(DOY %in% c(z - 2, z, z + 2))
    
    # bind into a df with the DOY
    focal.summary <- data.frame(Animal = CollarID,
                                canopy.5day = mean(focal.points$canopy, na.rm = TRUE),
                                TRI.5day = mean(focal.points$TRI, na.rm = TRUE),
                                dEdge.5day = mean(focal.points$dEdge, na.rm = TRUE),
                                DOY = z)
    
    # bind to master df
    indiv.landscape.summary.1 <- rbind(indiv.landscape.summary.1, focal.summary)
    
  }
  
  # keep only those DOYs we need
  indiv.landscape.summary.1 <- indiv.landscape.summary.1 %>% filter(DOY > 134 & DOY < 197)
  
  elk.np.landscape.2021 <- rbind(elk.np.landscape.2021, indiv.landscape.summary.1)
  
}

#_____________________________________________________________________________________________________________
# 7. Merge all dataframes ----
#_____________________________________________________________________________________________________________

# 2020
elk.np.2020 <- data.frame(DOY = 136:197,
                          CollarID = elk.np.steps.2020$Animal,
                          sl.3day = elk.np.steps.2020$sl.3day,
                          sl.post7 = elk.np.calf.steps.2020$sl.post7,
                          mcp = elk.np.mcp.2020$MCP,
                          canopy.5day = elk.np.landscape.2020$canopy.5day,
                          TRI.5day = elk.np.landscape.2020$TRI.5day,
                          dEdge.5day = elk.np.landscape.2020$dEdge.5day)

# 2021
elk.np.2021 <- data.frame(DOY = 135:196,
                          CollarID = elk.np.steps.2021$Animal,
                          sl.3day = elk.np.steps.2021$sl.3day,
                          sl.post7 = elk.np.calf.steps.2021$sl.post7,
                          mcp = elk.np.mcp.2021$MCP,
                          canopy.5day = elk.np.landscape.2021$canopy.5day,
                          TRI.5day = elk.np.landscape.2021$TRI.5day,
                          dEdge.5day = elk.np.landscape.2021$dEdge.5day)

#_____________________________________________________________________________________________________________
# 8. rbind together and write to .csv ----
#_____________________________________________________________________________________________________________

elk.np <- rbind(elk.np.2020, elk.np.2021)

# write to csv
write.csv(elk.np, "elk_np.csv")
