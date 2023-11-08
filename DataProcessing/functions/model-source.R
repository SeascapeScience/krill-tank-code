library('plotly')
library(randomForest)

# First version of a simulation, generic krill swimming around
krilltankinit <- function(xsize = 256,
                          ysize = 332,
                          zsize = 179,
                          xi = 100,
                          yi = 100,
                          zi = 100,
                          psi = 0,
                          theta = 0,
                          v = 0.1,
                          nt = 10000,
                          dt = 1)
{
  dataout <- matrix(data=NA,nrow=nt,ncol=3)
  dataout[1,1:3] <- c(xi,yi,zi)
  
  mu <- 0
  sigma <- 0.5
  
  
  for (i in 1:(nt-1))
  {
    v <- v + rnorm(1, mean = mu, sd = sigma)/100
    #v=v+(mu+sigma*tan(pi*(rand(1,1)-1/2)))/100; # Cauchy (matlab)
    psi <- psi + (runif(1)-.5)*5;
    theta <- theta + (runif(1)-.5)*5;
    dataout[i+1,1] <- dataout[i,1] + v * dt * cos(pi*psi/180)
    dataout[i+1,2] <- dataout[i,2] + v * dt * sin(pi*psi/180)
    dataout[i+1,3] <- dataout[i,3] + v * dt * sin(pi*theta/180)
    dataout[i+1,1] <- max(min(dataout[i+1,1],xsize),0)
    dataout[i+1,2] <- max(min(dataout[i+1,2],ysize),0)
    dataout[i+1,3] <- max(min(dataout[i+1,3],zsize),0)
  }
  return(dataout)
}

# Second version of a simulation, adding experimental conditions
krilltankinit2 <- function(xsize = 256,  # Size of tank (mm)
                          ysize = 332,
                          zsize = 179,
                          xi = 100,  # Initial position (mm)
                          yi = 100,
                          zi = 100,
                          psi = 0,  # Initial turn angle
                          theta = 0,
                          v = 0.1,  # Initial velocity (mm/s)
                          nt = 10000,  # Number of time steps
                          dt = 1,  # Lenght of time step (s)
                          flow.rate = 0,
                          chloro = 0,
                          guano = 0,
                          light = 0,
                          filein = 'notebook13-rf-2023.11.08data.RData')
{
  dataout <- matrix(data=NA,nrow=nt,ncol=3)
  dataout[1,1:3] <- c(xi,yi,zi)
  
  params <- getparams(filein = filein,
                      flow.rate = flow.rate,
                      chloro = chloro,
                      guano = guano,
                      light = light)
  vel.slope <- as.numeric(params[1])
  vel.intercept <- as.numeric(params[2])
  vel.sigma <- as.numeric(params[3])
  v.slope <- as.numeric(params[4])
  v.intercept <- as.numeric(params[5])
  v.sigma <- as.numeric(params[6])
  h.slope <- as.numeric(params[7])
  h.intercept <- as.numeric(params[8])
  h.sigma <- as.numeric(params[9])
  #mu <- 0
  #sigma <- 0.5
  
  
  for (i in 1:(nt-1))
  {
    v <- log10(v)
    v <- v * vel.slope + vel.intercept + rnorm(1, mean = 0, sd = vel.sigma)
    v <- 10 ^ v
    #v <- v + rnorm(1, mean = mu, sd = sigma)/100
    #v=v+(mu+sigma*tan(pi*(rand(1,1)-1/2)))/100; # Cauchy (matlab)
    #psi <- psi + (runif(1)-.5)*5;
    #theta <- theta + (runif(1)-.5)*5;
    psi <- psi * h.slope + h.intercept + rnorm(1, mean = 0, sd = h.sigma)
    theta <- theta * v.slope + v.intercept + rnorm(1, mean=0, sd = v.sigma)
    #dataout[i+1,1] <- dataout[i,1] + v * dt * cos(pi*psi/180)
    #dataout[i+1,2] <- dataout[i,2] + v * dt * sin(pi*psi/180)
    #dataout[i+1,3] <- dataout[i,3] + v * dt * sin(pi*theta/180)
    dataout[i+1,1] <- dataout[i,1] + v * dt * cos(psi) * sin(theta)
    dataout[i+1,2] <- dataout[i,2] + v * dt * sin(psi) * sin(theta)
    dataout[i+1,3] <- dataout[i,3] + v * dt * sin(theta)
    dataout[i+1,1] <- max(min(dataout[i+1,1],xsize),0)
    dataout[i+1,2] <- max(min(dataout[i+1,2],ysize),0)
    dataout[i+1,3] <- max(min(dataout[i+1,3],zsize),0)
  }
  return(dataout)
}

# Simulate a "swarm"
krillswarm <- function(nkrill = 100,  # Number of krill
                       xsize = 25600,  # Size of tank (mm)
                       ysize = 33200,
                       zsize = 17900,
                       xi = 10000,  # Initial position (mm)
                       yi = 10000,
                       zi = 10000,
                       psi = 0,  # Initial turn angle
                       theta = 0,
                       v = 0.1,  # Initial velocity (mm/s)
                       nt = 1000,  # Number of time steps
                       dt = 1,  # Lenght of time step (s)
                       flow.rate = 0,
                       chloro = 0,
                       guano = 0,
                       light = 0,
                       filein = 'notebook13-rf-2023.10.23data.RData')
{
  swarm <- matrix(data=NA,nrow=nkrill,ncol=3)
  for (i in 1:nkrill)
  {
    dataout <- krilltankinit2(nt=nt,
                              xsize=xsize,ysize=ysize,zsize=zsize,
                              xi=xi,yi=yi,zi=zi,
                              psi = psi,theta = theta,v = v,  
                              dt = dt,  
                              flow.rate = flow.rate,
                              chloro = chloro,
                              guano = guano,
                              light = light,
                              filein = filein)
    swarm[i,] <- dataout[nt,]
  }
  fig <- plot_ly(x=swarm[,1], y=swarm[,2], z=swarm[,3])
  fig
}

# Plots a krill path in 3D
krilltankplot <- function(datain = c(NA,NA,NA))
{
  fig <- plot_ly(x=datain[,1], y=datain[,2], z=datain[,3],
                 mode='lines')
  fig
  return(fig)
}

# Given a set of experimental conditions and a random forest model
# Returns the statistical parameters of swimming
getparams <- function(filein = 'notebook13-rf-2023.11.08data.RData',
                      flow.rate = 0,
                      chloro = 0,
                      guano = 0,
                      light = 0)
{
  load(filein)
  df.in <- data.frame(flow.rate = flow.rate, chlorophyll = chloro, guano = guano, light = light)
  # Autocorrelation parameters for velocity
  fit.vel.slope <- predict(conditions.rf.vel.slope,df.in)
  fit.vel.intercept <- predict(conditions.rf.vel.intercept,df.in)
  fit.vel.sigma <- predict(conditions.rf.vel.sigma,df.in)
  # Autocorrelation parameters for vertical heading
  fit.v.slope <- predict(conditions.rf.v.slope,df.in)
  fit.v.intercept <- predict(conditions.rf.v.intercept,df.in)
  fit.v.sigma <- predict(conditions.rf.v.sigma,df.in)
  # Autocorrelation parameters for horizontal heading
  fit.h.slope <- predict(conditions.rf.h.slope,df.in)
  fit.h.intercept <- predict(conditions.rf.h.intercept,df.in)
  fit.h.sigma <- predict(conditions.rf.h.sigma,df.in)
  # Compile all parameters
  df.out <- data.frame(fit.vel.slope = fit.vel.slope,
                       fit.vel.intercept = fit.vel.intercept,
                       fit.vel.sigma = fit.vel.sigma,
                       fit.v.slope = fit.v.slope,
                       fit.v.intercept = fit.v.intercept,
                       fit.v.sigma = fit.v.sigma,
                       fit.h.slope = fit.h.slope,
                       fit.h.intercept = fit.h.intercept,
                       fit.h.sigma = fit.h.sigma)
  return(df.out)
}

