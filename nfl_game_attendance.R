# Topic: NFL Attendance 
# Theme: Predictive Modeling using tidymodels


# Libraries ---------------------------------------------------------------

  library(tidyverse)
  library(tidymodels)

# DATA IMPORT -------------------------------------------------------------

# Get the Data

  attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
  standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
  games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')
  
  # Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
  # PLEASE NOTE TO USE 2020 DATA YOU NEED TO UPDATE tidytuesdayR from GitHub
  
  # Either ISO-8601 date or year/week works!
  
  # Join
  atd_jnd <- 
    attendance %>% 
    left_join(standings)
  set.seed("42")

# EXPLORE DATA ------------------------------------------------------------

  names(atd_jnd)
  
  atd_jnd %>% 
    ggplot() +
    geom_histogram(aes(weekly_attendance), position = "identity") +
    facet_wrap(~year)
  
  # By each team, who made it to the playoffs and how did attendance track?
  atd_jnd %>% 
    filter(!is.na(weekly_attendance)) %>% 
    mutate(team_order = fct_reorder(team_name, weekly_attendance, .fun = median)) %>% 
    ggplot(aes(x = weekly_attendance, y = team_order, fill = playoffs)) +
    geom_boxplot()
  
  # Histogram of margin of victory
  atd_jnd %>% 
    distinct(team_name, year, margin_of_victory, playoffs) %>% 
    ggplot(aes(margin_of_victory, fill = playoffs)) +
    geom_histogram(alpha = 0.7, position = "identity")

# TRAIN MODEL -------------------------------------------------------------

  # Modeling weekly attendance (ignoring weeks off)
  atd_df <- 
    atd_jnd %>% 
    filter(!is.na(weekly_attendance)) %>% 
    select(weekly_attendance, team_name, year, week, 
           margin_of_victory, strength_of_schedule, 
           playoffs)
  
  # Going to first, split the data up into train and test
  # strata = split data evenly into something I have in dataset
  # Here, splitting evening between those who made playoffs and didn't
  atd_df_split <- atd_df %>% 
    initial_split(strata = playoffs)
  
  # Split up data
  nfl_train <- training(atd_df_split)
  nfl_test <- testing(atd_df_split)
  
  # Build a simple model (OLS) - tidy models framework is composible, consistent, extensible
  # Can use STAN or packages in the engine
  
  # Have the model specification, so now we can fit to it
  lm_spec <- 
    linear_reg(mode = "regression") %>% 
    set_engine(engine = "lm" )
  
  # Once model is specified, we can then fit the model
  lm_fit <- 
    lm_spec %>% 
    fit(weekly_attendance ~ ., data = nfl_train)
  
  tidy(lm_fit) %>% arrange(desc(estimate))
  
  

# RANDOM FOREST -----------------------------------------------------------

  # Now lets fit a random forest
  # Specify
  rf_spec <- rand_forest(mode = "regression") %>% 
    set_engine("ranger")
  
  # Fit
  rf_fit <- rf_spec %>% 
    fit(weekly_attendance ~ ., data = nfl_train)
  
  tidy(rf_fit)
  

# EVALUATE ----------------------------------------------------------------

  # Training data, let's evaluate how two models do on training data
  
  # Combine the real data to the training and test
  results_train <- lm_fit %>% 
    predict(new_data = nfl_train) %>% 
    mutate(truth = nfl_train$weekly_attendance,
           model = "lm") %>% 
    bind_rows(rf_fit %>% 
                predict(new_data = nfl_train) %>% 
                mutate(truth = nfl_train$weekly_attendance,
                       model = "rf"))
  
  results_test <- 
    lm_fit %>% 
    predict(new_data = nfl_test) %>% 
    mutate(truth = nfl_test$weekly_attendance,
           model = "lm") %>% 
    bind_rows(rf_fit %>% 
                predict(new_data = nfl_test) %>% 
                mutate(truth = nfl_test$weekly_attendance,
                       model = "rf"))
  
  # Yardstick to measure and compare models
  # RF did better for the testing data
    results_train %>% 
      group_by(model) %>% 
      rmse(truth = truth, estimate = .pred)
  
  # About the same for both on the testing data
    results_test %>% 
      group_by(model) %>% 
      rmse(truth = truth, estimate = .pred)
    

# PLOT results ------------------------------------------------------------

  results_test %>% 
      mutate(train = "testing") %>% 
      bind_rows(results_train %>% mutate(train = "training")) %>% 
      ggplot(aes(truth, .pred, color = model)) +
      geom_abline(lty = 2, color = "grey20") +
      geom_point(alpha = 0.5) +
      facet_wrap(~train)


# OPTIONS to TRY ----------------------------------------------------------

  # Can try resampling the training data; Point is to get a better estimate
  # as to how the model performs on new data from the training data
    
  # CROSS-VALIDATION  
  set.seed("42")
  nfl_folds <- vfold_cv(nfl_train, strata = playoffs)
  
  # Folds keep track of which observations fit in which fold
  # Process: Fit a model on 9 folds, evaluate on the 10th (rotating through data)
  # Repeat this process. 
  
  rf_result <- fit_resamples(
    rf_spec,
    weekly_attendance ~ ., 
    nfl_folds,
    control = control_resamples(save_pred = TRUE)
  )

  
  # Next, going to take the rf_results and pipe to a function called collect_metrics
  # RSME is higher here - can understand how model performs on new data better
  rf_result %>% 
    collect_metrics()
  
  # Let's visualize results
  rf_result %>% 
    unnest(.predictions) %>% 
    ggplot(aes(weekly_attendance, .pred, color = id)) +
    geom_abline() +
    geom_point()
  