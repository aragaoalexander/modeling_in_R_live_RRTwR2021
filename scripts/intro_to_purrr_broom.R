## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for using
## purrr and broom packages for running multiple models, but a 
## nested list structure. This tutorial was originally designed 
## by Nicholas Potter during the Fall 2020 Reproducible Research 
## Techniques with R workshop.
## The script can be broken up into the following sections: 
## 1. A primer on the broom package
## 2. Using purrr to build multiple models

# Load the packages and data
library(tidyverse)
library(broom)

bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

# 1. A primer on the broom package ----------------------------------------

# Format some data
bike_data_cleaned <- bike_data %>%
  mutate(weather_type = factor(weathersit,
                               levels = c(1,2,3,4),
                               labels = c("Clear", "Cloudy", "Rainy", "Snowy")))

# Build the linear model

# Extracting coefficients with the tidy function

# Extracting model performance with glance

# Extract residuals

# Plotting estimates from the model



# 2. Using purrr to build multiple models ---------------------------------

# Format the data



# Create a nested dataframe



# Plot the estimates with standard error



# Plotting regression segments 


