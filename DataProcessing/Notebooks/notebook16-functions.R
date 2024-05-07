##notebook16-functions

rf.skill<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 ## column 1 to input
    )
{
  rf_mod <- ## creates random forest model
  rand_forest(trees = trees) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

  rf_fit <- ## fits random forest model to whole dataset
  rf_mod %>% 
  fit(df[[col1]] ~ flow * chl * guano * light, data = df)
  rf_fit

  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df[[col1]], rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
  return(c.e)
  }



rf.skill.test<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1, ## column 1 to input
    prop = 0.75,
    strata = NULL,
    do.plot = FALSE
)
{
   rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(df[[col1]] ~ flow * chl * guano * light, data = df)
  rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df[[col1]], rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
 
  rf_split <- initial_split(df %>% select(flow, chl, guano, light, ave.v, dip.test), strata = strata, prop = prop) 
  rf_train <- training(rf_split)
  ##creates training and testing datasets
  rf_test  <- testing(rf_split)
  
  
  ##rf_rec <-
    #recipe(response ~ flow * chl * guano * light, data = rf_train)
    ##update_role(D_V_T, new_role = "ID") ## can be used to ID tracks/conditions with issues in full df
    
  rf_fit_train <- ## fits random forest model to training dataset
    rf_mod %>% 
    fit(rf_train[[col1]] ~ flow * chl * guano * light, data = rf_train)
  rf_fit_train
  
  rf_pred_train <- predict(rf_fit_train, rf_test) ## compares to test data
  rf_test$pred <- rf_pred_train$.pred
  cor_train <- cor.test(rf_test[[col1]], rf_pred_train$.pred)  ## gives correlation coef
  c.e.t <- cor_train$estimate
  
  if (do.plot == TRUE){
  p = ggplot(rf_test, aes(rf_test[[col1]], rf_test$pred)) +
    geom_point() + 
    ggtitle(paste('correlation coef = ', c.e.t, sep = ''))  ## observed vs predicted
    print(p)
  }
  
    ## how to plot conditions and node purity from tidymodels rf and not R package rf??
  
  
  
  
  
    # Random forest package version - doing dip test
  conditions.rf <- randomForest(df[[col1]] ~ flow * chl * guano * light, data = df,
                                ntree = 1000,
                                importance=TRUE,
                                proximity=TRUE)
  #print(conditions.rf)
  #round(importance(conditions.rf), 2)
  #sqrt(conditions.rf$mse[which.min(conditions.rf$mse)]) 
  plot(conditions.rf)
  varImpPlot(conditions.rf)
  reprtree:::plot.getTree(conditions.rf)
  
  return(c.e.t)
  }


##  p = ggplot(rf_test, aes(rf_test$response, rf_test$pred)) +
##geom_point() + 
##ggtitle(paste('correlation coef = ', c.e.t, sep = ''))  ## observed vs predicted
##p.list <- as.list(p)
#p
#return(p.list)
