# Functions for the parameterization stuff

library(ggplot2)
library(GGally)
library(hexbin)
library(diptest)
library(tidymodels)
library(randomForest)
library('matrixStats')

getconditions <- function(cc.totaldata = NA)
{
  frs <- unique(cc.totaldata$Flow.rate)
  chls <- unique(cc.totaldata$Chlorophyll)
  guans <- unique(cc.totaldata$Guano)
  lights <- unique(cc.totaldata$Light)
  conditions <- expand.grid(frs,chls,guans,lights)
  return(conditions)
}

getautocorrparams <- function(datain = c(NA,NA))
{
  # Make time series offset by 1 step
  d0 <- datain[1:length(datain)-1]
  d1 <- datain[2:length(datain)]
  df <- data.frame(d0,d1)
  # Run statistics
  fit <- lm(d1 ~ d0 + 1, data = df)
  c <- cor.test(d0,d1)
  d <- dip.test(velocity)
  # Get parameters from statistical tests
  slope <- fit$coefficients[2]
  intercept <- fit$coefficients[1]
  resid <- sd(fit$residuals)  
  rho <- c$estimate
  dip <- d$p.value
  # List to output
  out <- list(slope = slope,
              intercept = intercept,
              resid = resid,
              rho = rho,
              dip = dip)
  return(out)
}
