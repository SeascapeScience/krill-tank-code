##notebook16-functions

rf.skill<- function(
    df = NA, ## df is dataframe that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    prop = 0.75,
    strata = NULL
    )
{
  rf_mod <- ## creates random forest model
  rand_forest(trees = trees) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

  rf_fit <- ## fits random forest model to whole dataset
  rf_mod %>% 
  fit(response ~ flow * chl * guano * light, data = df)
  rf_fit

  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$response, rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
  return(c.e)
  
  rf_split <- initial_split(df %>% select(flow, chl, guano, light, response), strata = strata, prop = prop) 
  rf_train <- training(rf_split)
    ##creates training and testing datasets
  rf_test  <- testing(rf_split)
  
  
  rf_fit_train <- ## fits random forest model to training dataset
    rf_mod %>% 
    fit(response ~ flow * chl * guano * light, data = rf_train)
  rf_fit_train
  
  rf_pred_train <- predict(rf_fit_train, rf_test) ## compares to test data
  rf_test$pred <- rf_pred_train$.pred
  cor_train <- cor.test(rf_test$response, rf_pred_train$.pred)  ## gives correlation coef
  c.e.t <- cor_train$estimate
  return(c.e.t)
  
  p <- ggplot(rf_test, aes(rf_test$response, rf_test$pred)) +
    geom_point() + 
    ggtitle(paste('correlation coef = ', c.e.t, sep = ''))  ## observed vs predicted
  p.list <- as.list(p)
  p
  return(p.list)
  }
