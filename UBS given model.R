
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
library(gvlma)
setwd("C:/Users/40101505/Desktop/UBS/")


####------------------------------------- Task 1 ----------------------------------------------###


columns <- c("date","GDP","Consumption","Investment","Government","DPI","CPI","ML","Tbill","Unemployment","Population","Inflation","Interest")
data <- read.table("data.txt", sep="|",stringsAsFactors = FALSE,header = FALSE, skip = 1)
colnames(data) <- columns
data <- na.locf(data, fromLast = T)
data <- data[,-c(1)]
temp<-data %>% mutate_at(vars(GDP:Interest),funs(.-lag(.)))
temp_data <- na.omit(temp)
modellm <- lm(Consumption~DPI+Unemployment,data = temp_data)
summary(modellm)
vif(modellm)

# Accuracy of results
# To calcuate MAPE and RMSE
# dividing the data into 80-20 (train-test split)
set.seed(42)
train_data <- temp_data[c(1:164),]
test_data <- temp_data[c(165:203),]
linear_Mod_2 <- lm(Consumption ~ DPI + Unemployment , data = train_data)
summary(linear_Mod_2)

predictions_basemodel <- predict(linear_Mod_2,test_data)

predictions_basemodel[1] <- predictions_basemodel[1] + data[164,2]
test_data$Consumption <- data$Consumption[c(165:203)]
for( i in c(2:length(predictions_basemodel))){
  predictions_basemodel[i] <- predictions_basemodel[i-1] + predictions_basemodel[i]
}
MAPE(predictions_basemodel, test_data$Consumption)
RMSE(predictions_basemodel, test_data$Consumption)
# MAPE was found to be 0.0455
# RMSE on test data was found to be 337.6009

###------------------------------------------ Task 2 ------------------------------------------###
plot(modellm$residuals)

q3 <- quantile(temp_data$Consumption,0.75)
q1 <- quantile(temp_data$Consumption,0.25)
iqr <- q3-q1
lowval <- q1 - 1.5* iqr
highval <- q3 + 1.5 * iqr  

Outliers_num <- sum(temp_data$Consumption < lowval) + sum(temp_data$Consumption > highval)
# 6 outliers

# Lets Remove Outliers
#rm(list = ls())
data_no_outlier <- temp_data[temp_data$Consumption > lowval & highval > temp_data$Consumption,]
modellm_no_outlier <- lm(Consumption~DPI+Unemployment,data = data_no_outlier)
summary(modellm_no_outlier)
vif(modellm_no_outlier)
set.seed(42)
train_data_no_outlier <- data_no_outlier[c(1:157),]
test_data_no_outlier <- data_no_outlier[c(158:197),]
linear_Mod_no_outlier <- lm(Consumption ~ DPI + Unemployment , data = data_no_outlier)
summary(linear_Mod_no_outlier)

predictions_basemodel_no_outlier <- predict(linear_Mod_no_outlier,test_data_no_outlier)

predictions_basemodel_no_outlier[1] <- predictions_basemodel_no_outlier[1] + data_no_outlier[157,2]
values_ <- data_no_outlier$Consumption[c(158:197)]
#data_no_outlier$Consumption <- data_no_outlier$Consumption[c(158:197)]
for( i in c(2:length(predictions_basemodel_no_outlier))){
  predictions_basemodel_no_outlier[i] <- predictions_basemodel_no_outlier[i-1] + predictions_basemodel_no_outlier[i]
}
MAPE(predictions_basemodel_no_outlier, values_)
RMSE(predictions_basemodel_no_outlier, values_)

# MAPE was found to be 13.23
# RMSE was found to be 662.18

# Checking the Consumption distribution before removing outliers and after removing outliers
boxplot(temp_data$Consumption,data_no_outlier$Consumption)

# Lets do imputations on outliers
ifelse(temp_data$Consumption < lowval | temp_data$Consumption > highval, mean(data_no_outlier$Consumption),temp_data$Consumption)
modellm_2 <- lm(Consumption~DPI+Unemployment,data = temp_data)
summary(modellm_2)
vif(modellm_2)

# Accuracy of results
# To calcuate the MAPE
set.seed(42)
train_data <- temp_data[c(1:164),]
test_data <- temp_data[c(165:203),]
linear_Mod_3 <- lm(Consumption ~ DPI + Unemployment , data = train_data)
summary(linear_Mod_3)

predictions_basemodel <- predict(linear_Mod_3,test_data)

predictions_basemodel[1] <- predictions_basemodel[1] + data[164,2]
test_data$Consumption <- data$Consumption[c(165:203)]
for( i in c(2:length(predictions_basemodel))){
  predictions_basemodel[i] <- predictions_basemodel[i-1] + predictions_basemodel[i]
}
MAPE(predictions_basemodel, test_data$Consumption)
RMSE(predictions_basemodel, test_data$Consumption)

# Even if we replace the outliers by mean, there is no change in the MAPE value and the RMSE value.

# Outliers dont have any significant effect on our model.


###--------------------------------------- Task 3 ---------------------------------------------###
# Approach: Durbin - Watson Test
lmtest::dwtest(modellm)

# We reject the null hypothesis here which implies that there is some autocorrelation
# DW value was found to be 1.7671, which shows that there is a positive correlation in the errors.


###-------------------------------------Task 4------------------------------------------------###
# Bootstrapping of Standard Errors


boot.fn = function(temp_data,index){
  return(coef(lm(Consumption ~ DPI + Unemployment, data = temp_data,subset = index)))
  
}
boot.fn(temp_data,1:204)
boot(temp_data, statistic = boot.fn, R = 10000)

# To check concretely

bstar <- NULL
r2 <- NULL
standarderrors <- NULL
aic <- NULL
n <- length(temp_data$Consumption); 
B <- 10000
for(draw in 1:B)
{
  # Randomly sample from the rows of kars, with replacement
  Dstar <- temp_data[sample(1:n,size=n,replace=T),]
  model <- lm(Consumption ~ DPI + Unemployment, data=Dstar)
  X <- summary(model)$coef
  standarderrors <- rbind(standarderrors,X[,2]) 
  bstar <- rbind( bstar,coef(model))
  r2 <- rbind(r2,summary(model)$r.square)
  aic <- rbind(aic,AIC(model))
} 
bstar <- as.data.frame(bstar)
sd(bstar$DPI)
sd(bstar$Unemployment)
sd(bstar$`(Intercept)`)

boot(data = temp_data, statistic = lm(Consumption ~ DPI + Unemployment, temp_data) , R= 10000)



# Standard Deviation for DPI coefficients = 0.05597325 (0.04778)
# Standard Deviation for Unemployment coefficients = 4.285596 (3.79216)
# Standard Deviation for Intercept coefficients = 2.311031 (1.91084)

# The standard error is an estimate of the standard deviation of the coefficient, 
# the amount it varies across cases. 
# We see that standard errors have increased after bootstrapping. With large standard errors
# the confidence interval becomes wider leading to less precise estimates of slope parameters



bptest(Consumption~DPI+Unemployment,data = temp_data)
ncvTest(modellm)     
     
spreadLevelPlot(modellm)
# p-value is lesser than 0.05, hence null hypothesis is rejected which implies that our model is heteroskedastic

# Assumption 2 : The mean of residuals is zero
mean(modellm$residuals)

cor.test(temp_data$DPI, modellm$residuals)
cor.test(temp_data$Unemployment, modellm$residuals)

vif(modellm)
acf(modellm$residuals)
# Assumption 10 : Normality of residuals
plot(modellm)

# Check assumptions automatically
gvlma::gvlma(modellm)
















