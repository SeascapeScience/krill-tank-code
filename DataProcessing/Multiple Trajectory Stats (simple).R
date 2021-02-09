####    Multiple Trajectory analysis in simple code    ######

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
tail(krill)

#### subset into individual krill tracks -----  CHANGE CSV FILE NAME!!!!!!!!!

track1 <- subset(krill, D_V_T=="20191127_1_7", select=Date:z)
head(track1)
tail(track1)
track1[is.na(track1)] <- 0
write.table(track1, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\Ind. Tracks\\20191127_view1_track7.csv", sep = ",", row.names = FALSE, col.names = TRUE, )

## Set working directory

# The files are all located somewhere under this directory
rootDir <- 'C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\Ind. Tracks\\'

### Set filenames
fileNames <- c(##"20191119_view1_track1.csv", "20191119_view1_track2.csv", "20191119_view1_track3.csv", "20191119_view1_track4.csv", "20191119_view1_track5.csv", 
               ##"20191119_view2_track1.csv") 
  "20191120_view2_track1.csv", "20191120_view2_track2.csv", "20191120_view2_track3.csv", "20191120_view2_track4.csv",
            "20191121_view2_track1.csv", "20191121_view2_track2.csv", "20191121_view2_track3.csv", "20191121_view2_track4.csv", "20191121_view2_track5.csv", "20191121_view2_track6.csv") ##,
            ##"20191127_view1_track1.csv", "20191127_view1_track2.csv", "20191127_view1_track3.csv", "20191127_view1_track4.csv", "20191127_view1_track5.csv", "20191127_view1_track6.csv", "20191127_view1_track7.csv")


## Set which columns are the x and y co-ordinates and time column  ###### 9,10 for x,y, 10,11 for y,z
csvStruct <- list(x = 10, y = 11)

library (trajr)
trjs <- TrajsBuild(fileNames, fps = 30, scale = NULL, spatialUnits = "m", timeUnits = "s",
                   csvStruct = csvStruct, rootDir = rootDir)
head(trjs)

# Define a function which calculates some statistics of interest for a single trajectory ##### (NEEDS WORK) #######
characteriseTrajectory <- function(trjs) {
  
  xy.angle <- TrajAngles(trjs, lag = 1, compass.direction = NULL)
  xy.DC <- TrajDirectionalChange(trjs, nFrames = 1)  ## 3 = every 3rd frame, 1 = every frame
  xy.Dist <- TrajDistance(trjs, startIndex = 1, endIndex = 1:(nrow(trjs)-2)) ## total distance from start row to end row (change to different segment lengths: currently set to every frame)
  ##e.g. if EndIndex = 5 gives distance of first 5 segment lengths
  xy.Dur <- TrajDuration(trjs, startIndex = 1, endIndex = 1:(nrow(trjs)-2)) ## total duration from start row to end row (change to different segment lengths: currently set to every frame)
  ##e.g. if EndIndex = 5 gives duration of first 5 segment lengths
  ##xy.length <- TrajLength(trjs, startIndex = 1, endIndex = nrow(trjs)) ##total length of trajectory (can be changed but can't get to work)
  ##xy.MeanVectorTurnAngles <- TrajMeanVectorOfTurningAngles(trjs, compass.direction = NULL)
  ##xy.MeanVelocity <- TrajMeanVelocity(trjs, startIndex = 1, endIndex = nrow(trjs)) #(can be changed but can't get to work)
  
  # Measures of speed
  r <- TrajDerivatives(trjs)
  xyacceleration <- r$acceleration
  xyaccelerationTimes <- r$accelerationTimes
  
  # Measures of straightness
  ##sinuosity <- TrajSinuosity2(trjs)
  ##Emax <- TrajEmax(trjs)
  

  # Return a list with all of the statistics for this trajectory
  list(xy.angle = xy.angle,
       xy.DC = xy.DC,
       xy.Dist = xy.Dist,
       xy.Dur = xy.Dur,
       ##xy.length = xy.length,
       ##xy.MeanVectorTurnAngles = xy.MeanVectorTurnAngles,
       ##xy.MeanVelocity = xy.MeanVelocity,
       xyacceleration = xyacceleration,
       xyaccelerationTimes = xyaccelerationTimes)
       ##sinuosity = sinuosity)
       ##Emax = Emax)  ##straightness of line (0 = sinuous, > 9999 = straight)
       }

stats <- TrajsMergeStats(trjs, characteriseTrajectory)
head(stats)
str(stats)
write.table(stats, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\FlowUp_All_Track_Stats_yz (Jan 4 2021).csv", sep = ",", row.names = FALSE, col.names = TRUE, )

###############################################################################################################

mean(stats$xy.angle)
sd(stats$xy.angle)
range(stats$xy.angle)

mean(stats$xy.DC)
sd(stats$xy.DC)
range(stats$xy.DC)

mean(stats$xy.Dist)
sd(stats$xy.Dist)
range(stats$xy.Dist)

mean(stats$xy.Dur)
sd(stats$xy.Dur)
range(stats$xy.Dur)

mean(stats$xyacceleration)
sd(stats$xyacceleration)
range(stats$xyacceleration)

mean(stats$xyaccelerationTimes)
sd(stats$xyaccelerationTimes)
range(stats$xyaccelerationTimes)



#### Speed Intervals, can set to be above, or below, or within certain speed limits

xy.SpeedIntervals <- TrajSpeedIntervals(trjs, fasterThan = 0.11, slowerThan = NULL, interpolateTimes = TRUE)
head(xy.SpeedIntervals)


####### Plotting Speed Intervals with sections highlighted
## S3 method for class 'TrajSpeedIntervals'
plot(x, slowerThanColour = "red",
     fasterThanColour = "green", highlightColor = "#0000FF1E",
     xlab = sprintf("Time (%s)", TrajGetTimeUnits(attr(x, "trajectory"))),
     ylab = sprintf("Speed (%s/%s)", TrajGetUnits(attr(x, "trajectory")),
                    TrajGetTimeUnits(attr(x, "trajectory"))), ...)
