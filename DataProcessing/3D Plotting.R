##### 3D plotting ######
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


#####  3D analysis  ######

## look at speed of krill [ something like x(n) - x(n-1) = difference in x axis location = speed of x axis travel??]
### or could use frame number as time (as it is relative to time) to generate [delta(x)/frame number] = distance/time = speed

##turns/turning [ maybe some kind of delta(x) * delta (y) * delta (z)? could pass a command of if (delta (x,y,z) is == 0 then find delta (other two factors))]


######  have downloaded BSA into R models folder just incase  #####

head(krill)
krill$dx <- NA  ## movement on the x axis
krill$dy <- NA  ## movement on the y axis
krill$dz <- NA  ## movement on the z axis
krill$d <- NA ## total movement
head(krill)
tail(krill)

for (i in 1:(nrow(krill))){
  x1= krill$x[i]
  x2= krill$x[i+1]
  krill$dx[i] = x2-x1

  y1= krill$y[i]
  y2= krill$y[i+1]
  krill$dy[i] = y2-y1
  
  z1= krill$z[i]
  z2= krill$z[i+1]
  krill$dz[i] = z2-z1
  
  krill$d[i] = sqrt(krill$dx[i]*krill$dx[i] + krill$dy[i]*krill$dy[i] + krill$dz[i]*krill$dz[i])
  print (i)}

write.table(krill, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\MASTER XYZ FACTOR FILE with d values (Jan 4 2021).csv", sep = ",",row.names = FALSE, col.names = TRUE, )



head(krill)
tail(krill)
krill$vx <- NA  ## velocity on the x axis
krill$vy <- NA  ## velocity on the y axis
krill$vz <- NA  ## velocity on the z axis
krill$v <- NA ## total velocity
head(krill)

for (i in 1:(nrow(krill)-1)){
  krill$vx[i] = (krill$dx[i]/krill$d[i])*30
  krill$vy[i] = (krill$dy[i]/krill$d[i])*30   ### frames per second
  krill$vz[i] = (krill$dz[i]/krill$d[i])*30
  krill$v[i] = (krill$vx[i]+krill$vy[i]+krill$vz[i])
  print (i)}  
  
krill[is.na(krill)] <- 0
tail(krill)
head(krill)

########################### plots ######################################

#### basic plots of movement along each axis for each krill
par(mar=c(4,4,1,1))
plot(krill$Frame, krill$y, t='p', lwd=1, col= c(krill$Krill.Track), ylab="Y Axis", xlab="Frame Number")
legend("topleft", legend=levels(krill$Krill.Track), col= c(1:5), pch = 19)

par(mar=c(4,4,1,1))
plot(krill$Frame, krill$x, t='p', lwd=1, col= c(krill$Krill.Track), ylab="X Axis", xlab="Frame Number")
legend("bottomleft", legend=levels(krill$Krill.Track), col= c(1:5), pch = 19)

par(mar=c(4,4,1,1))
plot(krill$Frame, krill$z, t='p', lwd=1, col= c(krill$Krill.Track), ylab="Z Axis", xlab="Frame Number")
legend("topleft", legend=levels(krill$Krill.Track), col= c(1:5), pch = 19)


#### 3D plot of krill tracks, with colours matched to base plots
library(plotly)

p <- plot_ly(krill, x = krill$x, y = krill$y, z = krill$z, color = krill$Krill.Track, colors = c('black', 'red', 'green', 'darkblue', 'lightblue')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'X axis'),
                      yaxis = list(title = 'Y axis'),
                      zaxis = list(title = 'Z axis')))

p

par(mfrow=c(2,2))
plot(krill$D_V_T, krill$dx, ylab="X Axis Movement", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$dy, ylab="Y Axis Movement", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$dz, ylab="Z Axis Movement", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$d, ylab="Total Movement", xlab="Date_View_Track Number")

plot(krill$D_V_T, krill$vx, ylab="X Axis Velocity", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$vy, ylab="Y Axis Velocity", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$vz, ylab="Z Axis Velocity", xlab="Date_View_Track Number")
plot(krill$D_V_T, krill$v, ylab="Total Velocity", xlab="Date_View_Track Number")

plot(krill$Frame, krill$vx, col=krill$Krill.Track, ylab = "X Axis Velocity", xlab = "Frame Number")
plot(krill$Frame, krill$vy, col=krill$Krill.Track, ylab = "Y Axis Velocity", xlab = "Frame Number")
plot(krill$Frame, krill$vz, col=krill$Krill.Track, ylab = "Z Axis Velocity", xlab = "Frame Number")
plot(krill$Frame, krill$v, col=krill$Krill.Track, ylab = "Total Velocity", xlab = "Frame Number")

plot(krill$Frame, krill$dx, col=krill$Krill.Track, ylab = "X Axis Movement", xlab = "Frame Number")
plot(krill$Frame, krill$dy, col=krill$Krill.Track, ylab = "Y Axis Movement", xlab = "Frame Number")
plot(krill$Frame, krill$dz, col=krill$Krill.Track, ylab = "Z Axis Movement", xlab = "Frame Number")
plot(krill$Frame, krill$d, col=krill$Krill.Track, ylab = "Total Movement", xlab = "Frame Number")

###################################################################

library(ggplot2)
g <- ggplot(data = krill, aes(x = krill$D_V_T, y = krill$v)) + 
  geom_boxplot(aes(fill = krill$Krill.Track), width = 0.8) + theme_bw()
g

p <- ggplot(data = krill, aes(x = krill$Frame, y = krill$v, group = krill$View, color = krill$View))
p <- p + geom_point(aes(shape = krill$Krill.Track), cex=3)
p


######################################################################

########################## save the dataframe as a .csv file!!!

#######################################################################


write.table(krill, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\MASTER XYZ FACTOR FILE with d and v values (Jan 4 2021).csv", sep = ",",row.names = FALSE, col.names = TRUE, )


#########################################################################

##################

##################     Power Analysis

############################################################################
library (pwr)
pwr.anova.test(k=160, f=.1, sig.level=.05, power=.8)  ## k = number of groups, f = effect size (values of 0.2, 0.5, and 0.8 represent small, medium, and large effect sizes respectively), sig.level = sig.level, power = power



######################################

############  means, sd and ranges

######################################
mean_D_V <- aggregate(krill[, 14:21], list(krill$D_V_T), mean) ## aggregates the data in columns 12 to 19 by the listed groups (D_V_T) using the mean function
mean_D_V
write.table(mean_D_V, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\Mean values for D_V_T groups (Jan 4 2021).csv", sep = ",",row.names = FALSE, col.names = TRUE, )

sd_D_V <- aggregate(krill[, 14:21], list(krill$D_V_T), sd) ## aggregates the data in columns 12 to 19 by the listed groups (D_V_T) using the sd function
sd_D_V
write.table(sd_D_V, file = "C:\\Users\\nicoleh3\\Documents\\Post-doc\\Data\\.csv files\\SD values for D_V_T groups (Jan 4 2021).csv", sep = ",",row.names = FALSE, col.names = TRUE, )

