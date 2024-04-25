##notebook16-functions

rf.skill<- function(
    df = NA, ## df is dataframe that must have columns (flow, light, guano, chl, response)
    trees = 1000
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
}
