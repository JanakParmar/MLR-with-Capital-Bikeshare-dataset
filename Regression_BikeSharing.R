#import required packages
if(!require("ppcor")) install.packages("ppcor")
if(!require("car")) install.packages("car")
if(!require("dplyr")) install.packages("dplyr")
if(!require("fastDummies")) install.packages("fastDummies")
library(ppcor)
library(car)
library(dplyr)
library(fastDummies)

#import data from the Working Directory in R
data <- read.csv("data_bikeshare.csv", header = TRUE)
head(data)

#clean data by removing missing values/NAs and unnecessary columns
data <- na.omit(data)

#standardize data to ensure all the variables are on same scale
num_cols <- c("instant", "temp", "feeltemp", "humidity", "windspeed", "casual", "registered", "count")#include continuous variables only
std_data <- data %>% mutate(across(all_of(num_cols), ~as.numeric(scale(.))))

#calculating correlations between the variables in dataset
corr <- cor(data)#analyse only required values
  
#building multiple linear regression (OLS) using inbuilt 'lm' function
#this code presents the final model after number of iterations by step-wise removing non-significant variables

model <- lm(count ~ factor(season) + factor(time) + factor(workingday) + factor(weather) + feeltemp + humidity + windspeed, data = data)

#summarize the estimated model and store as 'sumr'
sumr <- summary.lm(model, correlation = TRUE) #'correlation' produces the correlation between estimated coefficients
print(sumr)

#calculate 'correlation coefficient (multiple R)' for the estimated model
r <- sqrt(sumr$r.squared)
print(r)

#to obtained standardized regression coefficients
model_std <- lm(count ~ factor(season) + factor(time) + factor(workingday) + factor(weather) + feeltemp + humidity + windspeed, data = std_data)

### NOTE: standardized coefficients can also be obtained by multiplying unstandardized coefficients by the ratio of the ### 
### standard deviations of the independent and dependent variables ###

#producing confidence intervals for estimated model parameters
ci <- confint.lm(model, level = 0.95) #at 95% confidence level

#analysis of variance for the estimated model
#producing sum of squares, mean square, F-stat (and P values) comparing the mean square for the row to the residual mean square
anova(model)

#calculating zero-order correlation for the included variables in fitted model
data_dum <- fastDummies::dummy_cols(data, select_columns = c("season", "time", "workingday", "weather"),
                                    remove_first_dummy = TRUE, remove_selected_columns = TRUE)
zero_order <- cor(data_dum, method = "pearson")

vif(model) #to check the variation inflation factors of included variables

#calculating semi-partial correlation for the included variables in fitted model
sp <- spcor(data_dum, method = "pearson")
sp_corr <- sp[["estimate"]]

#plot the important graphs from estimated model
plot(model)
#for Predicted vs. Observed Values 
plot(x=predict(model), y=data$count, xlab = 'Predicted Values', ylab = "Observed Values", main = "Observed vs. Predicted Values")
abline(a=0, b=1) #creates regression line
