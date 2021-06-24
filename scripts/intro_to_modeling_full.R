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
#install.packages("tidyverse")
#install.packages("mgcv")
#install.packages("polynom")
#install.packages("hexbin")

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
  transmute(residuals = linear_model$residuals[1:731]) %>%
  mutate(instant = bike_data$instant)


hist(bike_lm_residuals$residuals)
# Getting predictions from a linear model for the 
# data values contained in the dataset

bike_linear_data$predicted_cnt <- predict(linear_model)
head(bike_linear_data)

bike_linear_data$residuals <- residuals(linear_model)
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
  geom_smooth(method = "lm", se = TRUE)+ #se is stdr error #method is linear model
  scale_fill_viridis_c(option = "plasma", name = "Number of\nPoints")+
  theme_minimal()

# 3. Lines and color points for residuals. 
# We will build this plot incrementally.

bike_linear_data %>% 
  ggplot(mapping = aes(x = feeling_temperature,
                       y = cnt))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_segment(mapping = aes(xend = feeling_temperature, 
                             yend = predicted_cnt),
               alpha = 0.2)+
  geom_point(mapping = aes(color = abs(residuals)))+
  scale_color_viridis_c(option = "plasma", name = "Residual\nTemperature")
  theme_minimal()


## Challenge Number 2
## Challenge Number 2a: Create a new data frame of temperatures
## from 0 to 50 with increments of, and predict values of counts (cnt) for 
## the new dataset. 
## Challenge Number 2b: Plot those new predictions.  
## Challenge 2c: Add a label in the plot with the linear model
## formula. 
  
prediction_data <- data.frame(feeling_temperature = seq(from = 0, to = 50, by = 0.25))

head(prediction_data)

prediction_data$predicted_cnt <- predict(object = linear_model,
                                         newdata = prediction_data)

head(prediction_data)

ggplot()+
  geom_point(data = bike_linear_data, mapping = (aes(x = feeling_temperature,
                                                     y = cnt)), alpha = 0.33)+
  geom_point(data = prediction_data, mapping = (aes(x = feeling_temperature,
                                                    y = predicted_cnt)), shape = 18)+
  annotate(geom = "label", label = paste("count = ", round(x = slope, digits = 2), "(Feeling Temperature) + ", round(y_intercept,2)),
           x = 15, y = 7500, size = 4) +
  xlab("Feeling Temperature") +
  ylab("Total Bikes Rented")+
  theme_minimal()

# 2. Multiple Linear Regression -------------------------------------------

# Multiple continuous predictors



# Create a new dataset for multiple linear regression 
# with continuous variables. 

multiple_bike_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50,
         humidity = hum * 100,
         windiness = windspeed * 67) %>%
  select(feeling_temperature, humidity, windiness, cnt)

head(multiple_bike_data)

# First make a model where interactions are considered. 

multiple_linear_model <- lm(formula = cnt ~ feeling_temperature*humidity*windiness, data = multiple_bike_data)

summary(multiple_linear_model)

# Assess the pairs plot for cross-correlations

pairs(multiple_bike_data[, 1:3])


# Create a function to put correlation value in the upper panel.
# This function can be found in the documentation for pairs.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Rerun pairs with upper panel showing correlation values.

pairs(multiple_bike_data[, 1:3], upper.panel = panel.cor)

# Second make a model where interactions are NOT considered. 

multiple_linear_model_no_inter <- lm(formula = cnt ~ feeling_temperature + humidity + windiness,
                                     data = multiple_bike_data)

summary(multiple_linear_model_no_inter)


# Calculate the AIC of each linear model
# i.e., all interactions vs no interactions

AIC(multiple_linear_model)
AIC(multiple_linear_model_no_inter)

## Challenge Number 3
## Challenge Number 3a: Make a model that considers feeling_temperature,
## humidity, and the interaction of humidity and windiness on cnt
## Challenge Number 3b: Is this model, better, worse, or 
## effectively the same as the previous models based on model performance?

multiple_linear_model_cn3 <- multiple_bike_data %>% lm(formula = cnt ~ feeling_temperature + humidity + humidity*windiness - windiness)

summary(multiple_linear_model_cn3)

AIC(multiple_linear_model_cn3)

# Mutliple linear regression with continuous and categorical predictors

# Create data subset

mixed_bike_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50,
         humidity = hum*100,
         windiness = windspeed * 67,
         weather_type = ifelse(weathersit == 1, "Clear", NA),
         weather_type = ifelse(weathersit == 2, "Misty_Cloudy", weather_type),
         weather_type = ifelse(weathersit == 3, "Light_precip", weather_type),
         weather_type = ifelse(weathersit == 4, "Heavy_precip", weather_type),
         weather_type = as.factor(weather_type)) %>%
  select(feeling_temperature, humidity, windiness, weather_type, cnt)

# Check to be sure that there are no NAs

summary(mixed_bike_data)

# Build the model

mixed_type_model <- mixed_bike_data %>%
  lm(formula = cnt ~ feeling_temperature*humidity*windiness*weather_type)

summary(mixed_type_model)

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
