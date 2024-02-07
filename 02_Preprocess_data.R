# Introduction to tidy models
# 02_preprocess_data

# Libraries ---------------------------------------------------------------

  library(tidymodels)      # for the recipes package, along with the rest of tidymodels
  
  # Helper packages
  library(nycflights13)    # for flight data
  library(skimr)           # for variable summaries


# Look at the data --------------------------------------------------------

  set.seed(43)

  flight_data <- 
    flights %>%   
    mutate(
      # Convert the arrival delay to a factor
      arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
      arr_delay = factor(arr_delay),
      # We will use the date (not date-time) in the recipe below
      date = lubridate::as_date(time_hour)
    ) %>% 
    # Include the weather data
    inner_join(weather, by = c("origin", "time_hour")) %>% 
    # Only retain the specific columns we will use
    select(dep_time, flight, origin, dest, air_time, distance, 
           carrier, date, arr_delay, time_hour) %>% 
    # Exclude missing data
    na.omit() %>% 
    # For creating models, it is better to have qualitative columns
    # encoded as factors (instead of character strings)
    mutate_if(is.character, as.factor)
  
  # How many flights are delayed more than 30 minutes? What does the distribution of delays look like?
  flight_data %>% names()
  flight_data 

  flight_data %>% 
    filter(carrier == "AA") %>% 
    mutate(month = lubridate::month(date), 
           year = lubridate::year(date)) %>% 
    ggplot(aes(y = carrier, x = month, color = arr_delay)) +
    geom_point(position = position_jitter(), size = 1, alpha = 0.5) +
    facet_wrap(~arr_delay)
  
  flight_data %>% 
    count(arr_delay) %>% 
    mutate(prop = n/sum(n))

  str(flight_data)  


# MODEL PRE-PROCESSING ----------------------------------------------------

  # Going to build a logistic regression to estimate correlates of a delayed flight
  # arr_delay is a factor variable
  
  # Need to make these into binaries, but some categories have few occurrences; may break model
  flight_data %>% 
    skimr::skim(dest, carrier) 
  
  # DATA SPLITTING
  # Need to split up our data so we have something to test our model on
  set.seed(42)

  data_split <- initial_split(flight_data)  
  
  # Create data frames for the two sets:
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  

# CREATE RECIPE AND ROLES -------------------------------------------------

  # We can specify a recipe function to capture the model setup
  # the "." argument means use all columns in the data frame
  flights_rec <- 
    recipe(arr_delay ~ ., data = train_data)
  
  # Use the `update_role()` function
  # The "ID" role can serve as a way to uniquely identify a row?
  # These ID columns are retained in the data but not used as part of the model.
  flights_rec <- 
    recipe(arr_delay ~ ., data = train_data) %>% 
    update_role(flight, time_hour, new_role = "ID") 
  
  summary(flights_rec)
  

# CREATE FEATURES ---------------------------------------------------------

  # Modify the model to extract information from the date column
  # day of week, the month, and whether or not the day is a holiday
  # Also are creating dummies for the remaining vars
  flights_rec <- 
    recipe(arr_delay ~ ., data = train_data) %>% 
    update_role(flight, time_hour, new_role = "ID") %>% 
    step_date(date, features = c("dow", "month")) %>%               
    step_holiday(date, 
                 holidays = timeDate::listHolidays("US"), 
                 keep_original_cols = FALSE) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    # remove variables that only contain a single value
    step_zv(all_predictors())
  

# FIT THE MODEL!!! --------------------------------------------------------

  # Build the model specification
  lr_mod <- 
    logistic_reg() %>% 
    set_engine("glm")
  
  # Steps to estimate
  #1) Process the recipe for the training data
  #2) Apply the recipe to the training set
  #3) Apply the recipe to the test set
  
  # a model workflow, which pairs a model and recipe together. 
  flights_wflow <- 
    workflow() %>% 
    add_model(lr_mod) %>% 
    add_recipe(flights_rec)
  
  # NOW FIT THE MODEL
  flights_fit <- 
    flights_wflow %>% 
    fit(data = train_data)
  
  flights_fit %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% glamr::prinf()
  

# PREDICT -----------------------------------------------------------------

  # Our goal was to predict whether a plane arrives more than 30 minutes late. We have just:
  # Built the model (lr_mod),
  # Created a preprocessing recipe (flights_rec),
  # Bundled the model and recipe (flights_wflow), and
  # Trained our workflow using a single call to fit().  
  
  # Use the trained workflow to predict with unseen test data.
  predict(flights_fit, test_data, type = "prob")
  
  
  flights_aug <- 
    augment(flights_fit, test_data)
  
  # Use  the area under the ROC curve as our metric, computed using roc_curve() and roc_auc() from the yardstick package.
  flights_aug %>% 
    roc_curve(truth = arr_delay, .pred_late) %>% 
    autoplot()
  
  flights_aug %>% 
    roc_auc(truth = arr_delay, .pred_late)
  