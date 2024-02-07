# Introduction to tidy models
# 01_Build_a_model

# Libraries ---------------------------------------------------------------


library(tidymodels)
library(readr)
library(broom.mixed)
library(dotwhisker)
library(tidyverse)

  theme_set(glitr::si_style())
# LOAD  DATA --------------------------------------------------------------

  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
    
  urchins <- 
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
    setNames(c("food_regime", "initial_volume", "width")) %>% 
    # Factors are very helpful for modeling, so we convert one column
    mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
  
  urchins %>% count(food_regime)
  urchins %>% names()

# EDA PLOTS ---------------------------------------------------------------

  # We want to know what the final width is of the urchin based on feeding regimes
  # We also want to condition (hold constant) the initial volume 
  
  urchins %>% 
    ggplot(aes(x = initial_volume, y = width, group = food_regime, 
               color = food_regime)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    scale_color_viridis_d(option = "plasma", end = .7) +
    theme_minimal()
  

# BUILD AND FIT MODEL -----------------------------------------------------

  # ANOVA is a decent starting point due to both a continous and categorical predictor
  # With differing slopes, we should build a model that allows for interactions.
  
  # Set model to fit 
  lm_mod <- linear_reg()
  
  # Think about what engine to use (https://parsnip.tidymodels.org/reference/linear_reg.html)
  
  # use the fit() function to actually estimate/train the model
  lm_fit <- 
    lm_mod %>% 
    fit(width ~ initial_volume * food_regime, data = urchins)
  
  tidy(lm_fit)
  
  # Now create a dot and whisker plot
  tidy(lm_fit) %>% 
    dwplot(dot_args = list(size = 2, color = "black"),
           whisker_args = list(color = "black"),
           vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) 
  

# PREDICT WITH A MODEL ----------------------------------------------------

  # Want to make a plot of the mean body size of urchins that started with initial volume of 20ml
  new_points <- expand.grid(initial_volume = 20, 
                            food_regime = c("Initial", "Low", "High"))
  
  # Use the predict function to get the predicted values for each feeding regime
  mean_pred <- predict(lm_fit, new_data = new_points)
  
  # Now generate confidence intervals for the predicted points
  conf_int_pred <- predict(lm_fit, 
                           new_data = new_points, 
                           type = "conf_int")
  
  # Combine and plot
  plot_data <- 
    new_points %>% 
    bind_cols(mean_pred) %>% 
    bind_cols(conf_int_pred)
  
  ggplot(plot_data, aes(x = food_regime)) + 
    geom_point(aes(y = .pred)) + 
    geom_errorbar(aes(ymin = .pred_lower, 
                      ymax = .pred_upper),
                  width = .2) + 
    labs(y = "urchin size")
  

# BAYESIAN APPROACH -------------------------------------------------------

  # Assume priors are bell shaped and fairly wide using a Cauch distribution
  # set the prior distribution
  prior_dist <- rstanarm::student_t(df = 1)
  
  set.seed(42)
  
  # make the parsnip model
  bayes_mod <-   
    linear_reg() %>% 
    set_engine("stan", 
               prior_intercept = prior_dist, 
               prior = prior_dist) 
  
  # train the model
  bayes_fit <- 
    bayes_mod %>% 
    fit(width ~ initial_volume * food_regime, data = urchins)
  
  print(bayes_fit, digits = 5)
  
  # Update the parameter table 
  tidy(bayes_fit, conf.int = TRUE)
  
  # Plot the data after fitting it
  bayes_plot_data <- 
    new_points %>% 
    bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
    bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))
  
  ggplot(bayes_plot_data, aes(x = food_regime)) + 
    geom_point(aes(y = .pred)) + 
    geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
    labs(y = "urchin size") + 
    ggtitle("Bayesian model with t(1) prior distribution")
  