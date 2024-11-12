##notebook16-functions
#' merge parameters and conditions
#' @param X chr file name of previously saved data
#' @param keep_cond chr names of conditions to keep
#' @param keep_param chr names of parameters to keep
#' @return tibble
merge_data = function(X = "~/Bigelow/Data/ParameterizationModel.15.07.24.Rdata", 
                      keep_cond = c("flow.rate", "chlorophyll", "guano", "light"), 
                      keep_param = c("velocity mean")){
  load(X)
  parameters = as_tibble(parameters) %>%
    select(all_of(keep_param))
  df = conditions %>% 
    as_tibble() %>%
    select(all_of(keep_cond)) %>%
     bind_cols(parameters)
  df = df %>%
    mutate(version = make_version(X = "v1", n = n()), .before = 1)
  df<-na.omit(df)
  attr(df, "parameters") = keep_param
  attr(df, "conditions") = keep_cond
  return(df)
}
  
  
  
rf.skill<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = NA ## column 1 to input
    )
{
  ##names(df)[names(df)==col1] <- 'resp'
  df = df %>%
    rename(resp := !!col1)
  cond = attr(df, "conditions")
  df_cond = select(df, all_of(c('resp', cond)))
  
  rf_mod <- ## creates random forest model
  rand_forest(trees = trees) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

  rf_fit <- ## fits random forest model to whole dataset
  rf_mod %>% 
  fit(resp ~ ., data = df_cond)
  #rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$resp, rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
  return(c.e)
  }

rf.skill.test<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = NA, ## column 1 to input
    prop = PROP,
    strata = STRATA,
    do.plot = FALSE
)
{
  ##names(df)[names(df)==col1] <- 'resp'
  
  df = df %>%
    rename(resp := !!col1)
  cond = attr(df, "conditions")
  df_cond = select(df, all_of(c('resp', cond)))
  
  rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(resp ~ ., data = df_cond)
  #rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$resp, rf_pred$.pred)  ## gives correlation coef
  c.e <- cor$estimate
 
  rf_split <- initial_split(df_cond %>% select(all_of(c('resp','flow.rate', 'chlorophyll', 'light', 'guano'))), strata = strata, prop = prop) 
  rf_train <- training(rf_split)
  ##creates training and testing datasets
  rf_test  <- testing(rf_split)
  
  rf_fit_train <- ## fits random forest model to training dataset
    rf_mod %>% 
    fit(resp ~ ., data = rf_train)
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
    conditions.rf <- randomForest(resp ~ ., data = df,
                                  ntree = trees,
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
  return(c.e.t)
}


rf.fit<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 2000,
    col1 = colnames(df)[5] ## column 1 to input
)
{
  ##names(df)[names(df)==col1] <- 'resp'
  
  
  df = df %>%
    rename(resp := !!col1)
  cond = attr(df, "conditions")
  df_cond = select(df, all_of(c('resp', cond)))
  
  
  rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(resp ~ ., data = df_cond)
  rf_fit
  
  return(rf_fit$fit$r.squared)
  
  ##  rf_fit$fit$prediction.error  predicted error of total model, 1 per run same as r-squared
  ## pull P value and average over 10 runs??
  ## pull node purity
}


model.test<- function(
    df = NA, ## df is data frame that must have columns (flow, light, guano, chl, response)
    trees = 1000,
    col1 = "velocity mean", ## column 1 to input
    prop = 0.75,
    strata = NULL,
    do.plot = TRUE
)
{
  ##names(df)[names(df)==col1] <- 'resp'
  
  df = df %>%
    rename(resp := !!col1)
  cond = attr(df, "conditions")
  df_cond = select(df, all_of(c('resp', cond)))
  
  rf_mod <- ## creates random forest model
    rand_forest(trees = trees) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_fit <- ## fits random forest model to whole dataset
    rf_mod %>% 
    fit(resp ~ ., data = df_cond)
  rf_fit
  
  rf_pred <- predict(rf_fit, df)  
  cor <- cor.test(df$resp, rf_pred$.pred)    ## gives correlation coef
  c.e <- cor$estimate
  
  rf_split <- initial_split(df_cond %>% select(all_of(c('flow.rate', 'chlorophyll', 'guano', 'light', 'resp'))), strata = STRATA, prop = PROP) 
  rf_train <- training(rf_split)
  ##creates training and testing datasets
  rf_test  <- testing(rf_split)
  
  rf_fit_train <- ## fits random forest model to training dataset
    rf_mod %>% 
    fit(resp ~ ., data = rf_train)
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
    conditions.rf <- randomForest(resp ~ ., data = df,
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
  
  rf_wf <- 
    workflow() %>%
    add_model(rf_mod) %>%
    add_formula(resp ~ .)
  
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
  #no_pre_proc
  
  with_features <- 
    workflow_set(
      preproc = list(simple = model_vars),
      models = list(linear_reg = linear_reg_spec)
    )
  
  all_workflows <- 
    bind_rows(no_pre_proc, with_features) %>% 
    # Make the workflow ID's a little more simple: 
    mutate(wflow_id = gsub("(simple_)", "", wflow_id))
  #all_workflows
  
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
  
  #grid_results
  
  grid_results %>% 
    rank_results() %>% 
    filter(.metric == "rsq", ) %>% 
    select(model, .config, rsq = mean, rank)
  
  autoplot(
    grid_results,
    rank_metric = "rsq",  # <- how to order models
    metric = "rsq",       # <- which metric to visualize
    select_best = TRUE     # <- one point per workflow
  ) +
    geom_text(aes(y = mean - 0.1, label = wflow_id), angle = 90, hjust = 1) +
    lims(y = c(-0.1, 1)) +
    theme(legend.position = "none")
  
  autoplot(grid_results, id = "RF", metric = "rsq")
  
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
      metrics = metric_set(rsq, rmse),
      grid = 25,
      control = race_ctrl
    )%>%
    charlier::write_RDS("mean vel model.rds") ##add col1 name here
  
  race_results
  
  ##how to produce complete output from dataframe model
  ##how to package them so we can iterate over them
  ##configuration file (txt)
  
  autoplot(race_results)
  
  autoplot(
    race_results,
    rank_metric = "rsq",  
    metric = "rsq",       
    select_best = TRUE    
  ) +
    geom_text(aes(y = mean - 0.1, label = wflow_id), angle = 90, hjust = 1) +
    lims(y = c(-0.1, 1)) +
    theme(legend.position = "none")
  
  
  matched_results <- 
    rank_results(race_results, select_best = TRUE, rank_metric = "rsq") %>% 
    select(wflow_id, .metric, race = mean, config_race = .config) %>% 
    inner_join(
      rank_results(grid_results, select_best = TRUE) %>% 
        select(wflow_id, .metric, complete = mean, 
               config_complete = .config, model),
      by = c("wflow_id", ".metric"),
    ) %>%  
    filter(.metric == "rsq")
  
  autoplot(race_results, metric = "rsq") +
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    theme(legend.position = "none")
  
  rsq_indiv_estimates <- 
    collect_metrics(race_results, summarize = FALSE) %>% 
    filter(.metric == "rsq") 
  
  rsq_indiv_estimates %>% drop_na()
  
  matched_results %>% 
    ggplot(aes(x = complete, y = race)) + 
    geom_abline(lty = 3) + 
    geom_point() + 
    geom_text_repel(aes(label = model)) +
    coord_obs_pred() + 
    labs(x = "Complete Grid RSQ", y = "Racing RSQ") 

  best_results <- 
    race_results %>%
    extract_workflow_set_result("RF") %>% 
    select_best(metric = "rsq")
  best_results
  
  rank_results(race_results, rank_metric = "rsq")
  
  RF_test_results <- 
    race_results %>% 
    extract_workflow("RF") %>% 
    finalize_workflow(best_results)%>% ## see if we can fix this, if not 
    last_fit(split = rf_split)
  
  collect_metrics(RF_test_results)
  
  RF_test_results %>% 
    collect_predictions() %>% 
    ggplot(aes(x = resp, y = .pred)) + 
    geom_abline(color = "gray50", lty = 2) + 
    geom_point(alpha = 0.5) +
    theme_classic()+
    coord_obs_pred() + 
    labs(x = "observed", y = "predicted")
  
 ## tune random forest model with full data set
  
  return(conditions.rf)
}



model_metrics = function(p_name = 'velocity mean', df = NULL, trees = 1000, prop = 0.8, strata = NULL, n_runs = 10, verbose = FALSE){
  if(FALSE){
    p_name = 'velocity mean'
    df = df
    trees = N_TREES
    prop = PROP
    strata = STRATA
    n_runs = N_RUNS
  }
  rr = lapply(seq_len(n_runs), 
              function(i, data = NULL){
                if(verbose)cat("interation =", i, 
                               "\n")
                data = select(data, -all_of("version"))
                c.e <- rf.skill(df = data, trees = trees, col1 = p_name) 
                c.e.t <- rf.skill.test(df = data, trees = trees, col1 = p_name,
                                       prop = prop, strata = strata, do.plot = TRUE)   ## training data
                rsq1 <- rf.fit(df = data, trees = trees, col1 = p_name)
                output.test <- model.test(df = data, trees = trees, col1 = p_name,
                                          prop = prop, strata = strata, do.plot = TRUE)
                return(tibble(parameter = p_name, 
                              corr.est = c.e,
                              corr.est.train = c.e.t,
                              rsq = rsq1,
                              conditions.rf = list(output.test$importance)
                ))
              }, data = df) %>% 
    bind_rows()
  return(rr)
}

