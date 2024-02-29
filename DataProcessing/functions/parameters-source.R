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
