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


krilltankinit2 <- function(xsize = 256,
                          ysize = 332,
                          zsize = 179,
                          xi = 100,
                          yi = 100,
                          zi = 100,
                          psi = 0,
                          theta = 0,
                          v = 0.1,
                          nt = 10000,
                          dt = 1,
                          flow.rate = 0,
                          chloro = 0,
                          guano = 0,
                          light = 0,
                          filein = 'notebook13-rf-24.07.RData')
{
  dataout <- matrix(data=NA,nrow=nt,ncol=3)
  dataout[1,1:3] <- c(xi,yi,zi)
  
  params <- getparams(filein = filein,
                      flow.rate = flow.rate,
                      chloro = chloro,
                      guano = guano,
                      light = light)
  slope <- params[1]
  intercept <- params[2]
  sigma <- params[3]
  #mu <- 0
  #sigma <- 0.5
  
  
  for (i in 1:(nt-1))
  {
    v <- v * slope + intercept + rnorm(1, mean = 0, sd = sigma)
    #v <- v + rnorm(1, mean = mu, sd = sigma)/100
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
krilltankplot <- function(datain = c(NA,NA,NA))
{
  fig <- plot_ly(x=datain[,1], y=datain[,2], z=datain[,3],
                 mode='lines')
  fig
  return(fig)
}

getparams <- function(filein = 'notebook13-rf-24.07.2023data.RData',
                      flow.rate = 0,
                      chloro = 0,
                      guano = 0,
                      light = 0)
{
  load(filein)
  df.in <- data.frame(flow.rate = flow.rate, chlorophyll = chloro, guano = guano, light = light)
  fit.slope <- predict(conditions.rf.slope,df.in)
  fit.intercept <- predict(conditions.rf.intercept,df.in)
  fit.sigma <- predict(conditions.rf.sigma,df.in)
  df.out <- data.frame(fit.slope = fit.slope,
                       fit.intercept = fit.intercept,
                       fit.sigma = fit.sigma)
  return(df.out)
}

