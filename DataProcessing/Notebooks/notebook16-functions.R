##notebook16-functions

rf.skill<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = NA ## column 1 to input
    )
{
  names(df)[names(df)==col1] <- 'resp'
  
  rf_mod <- ## creates random forest model
  rand_forest(trees = trees) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

  rf_fit <- ## fits random forest model to whole dataset
  rf_mod %>% 
  fit(resp ~ flow * chl * guano * light, data = df)
  rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$resp, rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
  return(c.e)
  }

rf.skill.test<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = NA, ## column 1 to input
    prop = 0.75,
    strata = NULL,
    do.plot = FALSE
)
{
  names(df)[names(df)==col1] <- 'resp'
  
   rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(resp ~ flow * chl * guano * light, data = df)
  rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$resp, rf_pred$.pred)    ## gives correlation coef
  c.e <- cor$estimate
 
  rf_split <- initial_split(df %>% select(flow, chl, guano, light, resp), strata = strata, prop = prop) 
  rf_train <- training(rf_split)
  ##creates training and testing datasets
  rf_test  <- testing(rf_split)
  
  rf_fit_train <- ## fits random forest model to training dataset
    rf_mod %>% 
    fit(resp ~ flow * chl * guano * light, data = rf_train)
  rf_fit_train
  
  rf_pred_train <- predict(rf_fit_train, rf_test) ## compares to test data
  rf_test$pred <- rf_pred_train$.pred
  cor_train <- cor.test(rf_test$resp, rf_pred_train$.pred) # gives correlation coef
  c.e.t <- cor_train$estimate
  
  if (do.plot == TRUE){
    p = ggplot(rf_test, aes(rf_test$resp, rf_test$pred)) +
    geom_point() + 
    ggtitle(paste('correlation coef = ', c.e.t, sep = ''))  ## observed vs predicted
    print(p)
    
    ## Random forest package version - doing dip test
    conditions.rf <- randomForest(resp ~ flow * chl * guano * light, data = df,
                                  ntree = 1000,
                                  importance=TRUE,
                                  proximity=TRUE)
    #print(conditions.rf)
    ##round(importance(conditions.rf), 2)
    ##sqrt(conditions.rf$mse[which.min(conditions.rf$mse)]) 
    plot(conditions.rf)
    varImpPlot(conditions.rf)
    reprtree:::plot.getTree(conditions.rf)
  }
  
  rmse(rf_test, truth = resp, estimate = pred)
  rf_metrics <- metric_set(rmse, rsq, mae) ## random mean square error, r square value and mean absolute error
  rf.metrics <- as.data.frame(rf_metrics(rf_test, truth = resp, estimate = pred))
  
  
## return(rf.metrics)
  
 rf_wf <- 
   workflow() %>%
   add_model(rf_mod) %>%
    add_formula(resp ~ flow * chl * guano * light)
  
  tune_spec <- 
   decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
   ) %>% 
   set_engine("rpart") %>% 
   set_mode("regression")
 
   tree_grid <- grid_regular(cost_complexity(),
    tree_depth(),
      levels = 5)
 tree_grid
 
 rf_folds <- vfold_cv(rf_train)
 
 normalized_rec <- 
   recipe(resp ~ ., data = rf_train) %>% 
   step_normalize(~ ., data = rf_train) 
 
 library(rules)
 library(baguette)
 
 linear_reg_spec <- 
   linear_reg(penalty = tune(), mixture = tune()) %>% 
   set_engine("glmnet")
 
 cart_spec <- 
   decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
   set_engine("rpart") %>% 
   set_mode("regression")
 
 bag_cart_spec <- 
   bag_tree() %>% 
   set_engine("rpart", times = 50L) %>% 
   set_mode("regression")
 
 rf_spec <- 
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
   set_engine("ranger") %>% 
   set_mode("regression")
 
 cubist_spec <- 
   cubist_rules(committees = tune(), neighbors = tune()) %>% 
   set_engine("Cubist") 
 
model_vars <- 
    workflow_variables(outcomes = resp, 
                       predictors = everything())
  
no_pre_proc <- 
    workflow_set(
      preproc = list(simple = model_vars), 
      models = list(CART = cart_spec, CART_bagged = bag_cart_spec,
                    RF = rf_spec, Cubist = cubist_spec)
    )
no_pre_proc

with_features <- 
    workflow_set(
      preproc = list(simple = model_vars),
      models = list(linear_reg = linear_reg_spec)
    )
  
all_workflows <- 
    bind_rows(no_pre_proc, with_features) %>% 
    # Make the workflow ID's a little more simple: 
    mutate(wflow_id = gsub("(simple_)", "", wflow_id))
all_workflows
  
grid_ctrl <-
    control_grid(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  
grid_results <-
    all_workflows %>%
    workflow_map(
      seed = 1503,
      resamples = rf_folds,
      grid = 25,
      control = grid_ctrl
    )
 
grid_results
  
grid_results %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  select(model, .config, rmse = mean, rank)
  
autoplot(
  grid_results,
  rank_metric = "rmse",  # <- how to order models
  metric = "rmse",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
   ) +
  geom_text(aes(y = mean - 0.1, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(0, 0.3)) +
  theme(legend.position = "none")

autoplot(grid_results, id = "RF", metric = "rmse")

library(finetune)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <-
  all_workflows %>%
  workflow_map(
    "tune_race_anova",
    seed = 1503,
    resamples = rf_folds,
    grid = 25,
    control = race_ctrl
  )

race_results

autoplot(
  race_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 0.1, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(0, 0.3)) +
  theme(legend.position = "none")


matched_results <- 
  rank_results(race_results, select_best = TRUE) %>% 
  select(wflow_id, .metric, race = mean, config_race = .config) %>% 
  inner_join(
    rank_results(grid_results, select_best = TRUE) %>% 
      select(wflow_id, .metric, complete = mean, 
             config_complete = .config, model),
    by = c("wflow_id", ".metric"),
  ) %>%  
  filter(.metric == "rmse")

library(ggrepel)

matched_results %>% 
  ggplot(aes(x = complete, y = race)) + 
  geom_abline(lty = 3) + 
  geom_point() + 
  geom_text_repel(aes(label = model)) +
  coord_obs_pred() + 
  labs(x = "Complete Grid RMSE", y = "Racing RMSE") 






 ##tune_wf <- workflow() %>%
  ##add_model(tune_spec) %>%
  ##add_formula(resp ~ flow * chl * guano * light)
 
  ##tune_res <- 
  ##tune_wf %>% 
  ## tune_grid(
  ## resamples = rf_folds,
  ## grid = tree_grid
  ## )
 
  ##tune_res
  
  autoplot(tune_res, rank_metric = "rmse", metric = "rmse", select_best = T)
 
 #tune_res %>% 
  ## collect_metrics()%>%
  ## mutate(tree_depth = factor(tree_depth)) %>%
  ## ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  ## geom_line(size = 1.5, alpha = 0.6) +
  ## geom_point(size = 2) +
  ## facet_wrap(~ .metric, scales = "free", nrow = 2) +
  ## scale_x_log10(labels = scales::label_number()) +
  ## scale_color_viridis_d(option = "plasma", begin = .9, end = 0)
 
 ##tune_res %>%
   #show_best(metric = "rsq")
    
 ##best_tree <- tune_res %>%
   #select_best(metric = "rsq")
 
  ## how to plot conditions and node purity from tidymodels rf and not R package rf??

 ## return(c.e.t)
  }


rf.fit<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = NA ## column 1 to input
)
{
  names(df)[names(df)==col1] <- 'resp'
  
  rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(resp ~ flow * chl * guano * light, data = df)
  rf_fit
  
  return(rf_fit$fit$r.squared)
  
  ##  rf_fit$fit$prediction.error  predicted error of total model, 1 per run same as r-squared
  ## pull P value and average over 10 runs??
  ## pull node purity
}
