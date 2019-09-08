
rm(list = ls())
library(tidyverse)
library(caret)
library(glmnet)
library(lmtest)
library(zoo)
library(car)
library(MLmetrics)
library(boot)
library(lmtest)
library(MASS)
setwd("C:/Users/40101505/Desktop/UBS/")

columns <- c("date","GDP","Consumption","Investment","Government","DPI","CPI","ML","Tbill","Unemployment","Population","Inflation","Interest")
data <- read.table("data.txt", sep="|",stringsAsFactors = FALSE,header = FALSE, skip = 1)
colnames(data) <- columns

# There are two NA values in the data in Inflation and Interest Columns
# We replace these NA values wiht the subsequent values in the data because we dont want to replace it using mean or median because we have macroeconomic data and it doesnt make sense to replace it with mean or median even though we are differencing
data <- na.locf(data, fromLast = T)

# Deleting the date column
data <- data[,-c(1)]


# First Differencing
temp<-data %>% mutate_at(vars(GDP:Interest),funs(.-lag(.)))
temp_data <- na.omit(temp)


# Trying to get the best model using AIC as the metric
modellm <- lm(Consumption~.,data = temp_data)
modelaic <- stepAIC(modellm,direction ="both")

# Final model which has all the variables and intercept with significant p-values 
modelthruaic <- lm(Consumption ~  GDP+  ML , data = temp_data )
summary(modelthruaic)

# Checking multicollinearity in the variables
vif(modelthruaic)


# Train - Test split and as we know that this is data where time is important, so keeping the chronological order intact
set.seed(42)
train_data <- temp_data[c(1:164),]
test_data <- temp_data[c(165:203),]

# Fitting a linear model with same variables on Training data
linear_Mod_2 <- lm(Consumption ~ GDP +  ML  , data = train_data)
summary(linear_Mod_2)

# Predicting Consumption values on test data 
predictions <- predict(linear_Mod_2, test_data)

# As we had done the differencing, to calculate the MAPE, we must do inverse transforming
predictions[1] <- predictions[1] + data[164,2]
test_data$Consumption <- data$Consumption[c(165:203)]
for( i in c(2:length(predictions))){
  predictions[i] <- predictions[i-1] + predictions[i]
}

MAPE(predictions, test_data$Consumption)
RMSE(predictions, test_data$Consumption)


# MAPE = 0.0286217
# RMSE = 183.4314

# Checking Normality, autocorrelation and heteroscedasticity of residuals

mean(modelthruaic$residuals)

plot(modelthruaic)

acf(modelthruaic$residuals)
lmtest::dwtest(modelthruaic)

bptest(Consumption~GDP+Unemployment,data = temp_data)
     



