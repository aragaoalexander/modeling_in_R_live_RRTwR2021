## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for different 
## types of modeling. The script can be broken into a few main 
## sections: 
## 1. One-way Linear Modeling
## 2. Multiple Linear Regression
## 3. Non-linear Modeling
## 3.1 Polynomial Regression
## 3.2 General Additive Models
## 3.3 Logistic Regression

# Install necessary packages (for the entire script)
install.packages("tidyverse")
install.packages("mgcv")
install.packages("polynom")
install.packages("hexbin")

library(tidyverse)
library(mgcv)
library(polynom)
library(hexbin)

# Load and inspect the data

bike_data <- read.csv("data/day.csv",
                      header = TRUE)

head(bike_data)

ggplot(bike_data, aes(atemp*50, (cnt))) +
  geom_point() +
  xlab("Feeling Temperature") +
  ylab("Total bikes rented")


# 1. One-way linear regression --------------------------------------------

# Create a subset of the data 

bike_linear_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50) %>%
  select(feeling_temperature, cnt)

# Build a linear model

linear_model <- lm(formula = cnt ~ feeling_temperature,
                   data = bike_linear_data)

# Assess the model

summary(linear_model)

# Assess model object structure

str(linear_model)

# Extract R-squared

r_squared <- summary(linear_model)$r.squared

# Extract p-value

p_value <- summary(linear_model)$coefficients[ 2, 4]

# Extract slope

slope <- summary(linear_model)$coefficients[2,1]

# Extract y-intercept

y_intercept <- summary(linear_model)$coefficients[1,1]

# Challenge Number 1
## 1. Extract model residuals and
## 2. Assess the residuals' distribution?
## Analysis of choice, but assess the residuals.

bike_lm_residuals <- as.data.frame(linear_model$residuals[1:731], col.names = ("residuals"))

bike_lm_residuals <- bike_lm_residuals %>%
  transmute(residuals = `linear_model$residuals[1:731]`) %>%
  mutate(instant = bike_data$instant)

hist(bike_lm_residuals$residuals)

# Getting predictions from a linear model for the 
# data values contained in the dataset

bike_linear_data$predicted_cnt <- predict(linear_model)
head(bike_linear_data)

# Visualize residual deviation from the modeled predictions 3 ways

# 1. Visualize model and points 

bike_linear_data %>%
  ggplot(aes(x = feeling_temperature,y = cnt))+
  geom_point(alpha = 0.33)+
  geom_smooth(method ="lm", se = TRUE)+
  theme_minimal()

# 2. Group points into hexbins

bike_linear_data %>%
  ggplot(mapping = aes(x = feeling_temperature, y = cnt))+
  geom_hex(bins = 10)+
  geom_smooth(method = "lm", se = TRUE)+ #se is stdr error
  scale_fill_viridis_c(option = "plasma", name = "Number of\nPoints")+
  theme_minimal()

# 3. Lines and color points for residuals. 
# We will build this plot incrementally.

bike_linear_data %>% 
  ggplot(aes(x = feeling_temperature, y = cnt))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_segment(aes(xend = feeling_temperature, yend = predicted_cnt), alpha = 0.2)+
  theme_minimal()


## Challenge Number 2
## Challenge Number 2a: Create a new data frame of temperatures
## from 0 to 50 with increments of, and predict values for 
## the new dataset. 
## Challenge Number 2b: Plot those new predictions.  
## Challenge 2c: Add a label in the plot with the linear model
## formula. 


# 2. Multiple Linear Regression -------------------------------------------

# Multiple continuous predictors

# Create a new dataset for multiple linear regression 
# with continuous variables. 


# First make a model where interactions are considered. 


# Assess the pairs plot for cross-correlations


# Create a function to put correlation value in the upper panel.
# This function can be found in the documentation for pairs.


# Rerun pairs with upper panel showing correlation values.


# Second make a model where interactions are NOT considered. 



# Calculate the AIC of each linear model
# i.e., all interactions vs no interactions


## Challenge Number 3
## Challenge Number 3a: Make a model that considers feeling_temperature,
## humidity, and the interaction of humidity and windiness on cnt
## Challenge Number 3b: Is this model, better, worse, or 
## effectively the same as the previous models based on model performance?



# Mutliple linear regression with continuous and categorical predictors

# Create data subset


# Check to be sure that there are no NAs


# Build the model


# 3. Non-Linear Modeling --------------------------------------------------


# 3.1 Polynomial regression -----------------------------------------------

# Create a special dataset


# Remind ourselves what these data looked like

# Build the model



# Quick Challenge: Create a dataset with feeling_temperature ranges from 0 to 45 by 0.25 increments
# And predict bike ridership using the cubic function we just created. 


# Build the model


# 3.2 General Additive Models ---------------------------------------------

# Build the model 
# We will rerun this same R code multiple times using different 
# values for gamma. 
# Remember: Gamma is like a wiggliness factor (per Gavin Simpson),
# where gamma closer to zero is very wiggly, and gamma greater than 1
# less wiggly. 



# 3.3 Logistic Regresssion ------------------------------------------------

# First look at some data that we will be modeling

# Build the model 


## Challenge 4: Knowing what we've learned, plot this model for values of 
## 0 through 7000 by steps of 10, and add the fitted model probabilities 
## to the plot. (Hint: values from a logistic are as log odds, so check 
## documentation for how to convert this automatically in the "predict" 
## function. 
