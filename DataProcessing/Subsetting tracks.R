rm(list=ls(all=TRUE))
krill <- read.csv("C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\MASTER XYZ FACTOR FILE (Jan 4 2021).csv")
head(krill)
str(krill)
krill$Krill.Track <- as.factor(krill$Krill.Track)
krill$Date <- as.factor(krill$Date)
krill$View <- as.factor(krill$View)
krill$Flow.Rate <- as.factor(krill$Flow.Rate)
krill$Chla <- as.factor(krill$Chla)
krill$D_V_T <- paste(krill$Date, krill$View, krill$Krill.Track, sep='_') ## Date_view_track factor created
krill$D_V_T <- as.factor(krill$D_V_T)
krill$D_V <- paste(krill$Date, krill$View, sep='_') ## Date_view factor created
krill$D_V <- as.factor(krill$D_V)
krill <- na.omit(krill)
str(krill)
head(krill)
tail(krill)


#### subset into individual krill tracks -----  CHANGE CSV FILE NAME!!!!!!!!!

track1 <- subset(krill, D_V_T=="20191127_8_4", select=Date:z)
head(track1)
tail(track1)
track1[is.na(track1)] <- 0
write.table(track1, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\Ind. Tracks\\20191127_view8_track4.csv", sep = ",", row.names = FALSE, col.names = TRUE, )

library (trajr)

trj <- TrajFromCoords(track1, xCol = 9, yCol = 10, timeCol = 3, fps = 30,
                      spatialUnits = "mm", timeUnits = "s")
trj1 <- TrajFromCoords(track1, xCol = 10, yCol = 11, timeCol = 3, fps = 30,
                       spatialUnits = "mm", timeUnits = "s")

##  ----------------------------------------------change row length to number of obs. MINUS 2!!!
angles <- data.frame(xy.angles = rep(as.numeric(NA),4954), yz.angles = rep(as.numeric(NA),4954))

angles$xy.angles <- TrajAngles(trj, lag = 1, compass.direction = NULL)
angles$yz.angles <- TrajAngles(trj1, lag = 1, compass.direction = NULL)
angles$xyDC <- TrajDirectionalChange(trj, nFrames = 1)  ## 3 = every 3rd frame, 1 = every frame
angles$yzDC <- TrajDirectionalChange(trj1, nFrames = 1)
angles$xyDist <- TrajDistance(trj, startIndex = 1, endIndex = 1:(nrow(trj)-2)) ## total distance from start row to end row (change to different segment lengths: currently set to every frame)
angles$yzDist <- TrajDistance(trj1, startIndex = 1, endIndex = 1:(nrow(trj1)-2)) ##e.g. if EndIndex = 5 gives distance of first 5 segment lengths
angles$xyDur <- TrajDuration(trj, startIndex = 1, endIndex = 1:(nrow(trj1)-2)) ## total duration from start row to end row (change to different segment lengths: currently set to every frame)
angles$yzDur <- TrajDuration(trj1, startIndex = 1, endIndex = 1:(nrow(trj1)-2)) ##e.g. if EndIndex = 5 gives duration of first 5 segment lengths
angles$xy.EMax <- TrajEmax(trj, eMaxB = FALSE, compass.direction = NULL) ## maximum expected displacement (measure of straightness - 0 = sinuous and 9999999 = straight), EMaxA = unscaleable measure, EMaxB = scaled to distance units
angles$yz.EMax <- TrajEmax(trj1, eMaxB = FALSE, compass.direction = NULL)
angles$xy.length <- TrajLength(trj, startIndex = 1, endIndex = nrow(trj)) ##total length of trajectory (can be changed but can't get to work)
angles$yz.length <- TrajLength(trj1, startIndex = 1, endIndex = nrow(trj1)) #(can be changed but can't get to work)
angles$xy.MeanVectorTurnAngles <- TrajMeanVectorOfTurningAngles(trj, compass.direction = NULL)
angles$yz.MeanVectorTurnAngles <- TrajMeanVectorOfTurningAngles(trj1, compass.direction = NULL)
angles$xy.MeanVelocity <- TrajMeanVelocity(trj, startIndex = 1, endIndex = nrow(trj)) #(can be changed but can't get to work)
angles$yz.MeanVelocity <- TrajMeanVelocity(trj1, startIndex = 1, endIndex = nrow(trj1)) #(can be changed but can't get to work)
angles$xy.SpeedIntervals <- TrajSpeedIntervals(trj, fasterThan = 0.11, slowerThan = NULL, interpolateTimes = TRUE)

r <- TrajDerivatives(trj)
xyspeed <- r$speed
xyspeedTimes <- r$speedTimes
angles$xyacceleration <- r$acceleration
angles$xyaccelerationTimes <- r$accelerationTimes


r2 <- TrajDerivatives(trj1)

yzspeed <- r2$speed
yzspeedTimes <- r2$speedTimes
angles$yzacceleration <- r2$acceleration
angles$yzaccelerationTimes <- r2$accelerationTimes

head(angles)
tail(angles)
write.table(track1, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\20191127_view8_track4_angles.csv", sep = ",", row.names = FALSE, col.names = TRUE, )


library(dplyr)
library(readr)
df <- list.files(path="yourpath", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

