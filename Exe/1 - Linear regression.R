# Lab: Linear Regression

install.packages("MASS")
install.packages("ISLR2")

### RUN CODE - LINEbyLINE CTRL + ENTER
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot


### Add Libraries
library(MASS)
library(ISLR2)
set.seed(1) # seed for random number generaotr

## Simple Linear Regression

### View Data set
View(Boston)
head(Boston)
?Boston
dim(Boston)

### Fit Linear Model: mdev = b0 + b1*lstat + e
lm_fit <- lm(medv ~ lstat, data = Boston)

# View fitted results
lm_fit
summary(lm_fit)
lm_fit$coefficients
# names(lm.fit)
# coef(lm.fit)

# Compute confident interval (CI) on coefficient
?confint
confint(lm_fit, level = 0.98)

# Get prediction with CI over new observation 
predict(lm_fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")

# Plot linear regression results
plot(Boston$lstat, Boston$medv,pch = "+", ylab = "medv - median value of owner-occupied homes in $1000s", 
       xlab = "lstat - ower status of the population (percent).", 
       main = "Linear regression")
legend("topright",legend=c("Sampling point"),pch = "+")

# Add predicted value 
abline(lm_fit, lwd = 3, col = "red")

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit)
# A strong pattern in the residuals indicates non-linearity in the data
# plot(predict(lm.fit), residuals(lm.fit))
# plot(predict(lm.fit), rstudent(lm.fit))

# If the residual plot indicates that there are non-linear associations in the
# data, then a simple approach is to use non-linear transformations of the
# predictors; one possible solution is to transform the response;

# Leverage misure

# Put plotting arrangement back to its original state
par(mfrow = c(1, 1))
plot(hatvalues(lm_fit), col="blue", 
     ylab = "leverage",
     xlab = "Observation",
     main = "leverage") # 1/n < leverage < 1 


## Multiple Linear Regression

# y =b0 + b1*lstat + b2*age + e
lm_fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm_fit)

# y = b*X + e (perform a regression using all of the predictors)
lm_fit <- lm(medv ~ ., data = Boston)
s <- summary(lm_fit)
R2 = s$adj.r.squared
RSE = s$sigma

# update regression, remove age predictor (large p-value)
lm_fit <- update(lm_fit, ~ . - age)

## Interaction Terms & Non-linear Transformations of the Predictors
summary(lm(medv ~ lstat + age + lstat:age, data = Boston))
summary(lm(medv ~ lstat + I(lstat^2), data = Boston))

# plot residual
lm_fit <- lm(medv ~ lstat + I(lstat^2) + log(rm), data = Boston); 
par(mfrow = c(1,2))

# plot 1
plot(lm_fit$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(lm_fit$residuals),digits = 4),
                                      "- var:", round(var(lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

# plot 2 
hist(lm_fit$residuals,40,
     xlab = "Residual",
     main = "Empirical residual distribution") 


## Qualitative Predictors
head(Carseats)
View(Carseats)

# Given a qualitative variable such as Shelveloc , R generates dummy variables
# automatically in this way: 
contrasts(Carseats$ShelveLoc)

# Regression
lm_fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
             data = Carseats)
summary(lm_fit)

### Validation method 

View(Auto)
dim(Auto)

# Validation Method
?sample
set.seed(2)
train <- sample (392 , 196, replace = FALSE)

lm_fit <- lm(mpg ~ horsepower , data = Auto , subset = train)
err = (Auto$mpg - predict(lm_fit, Auto ))^2

trai_err = mean(err[train])
test_err = mean(err[-train])        

# quadratic fit
lm_fit_quad <- lm ( mpg ~ poly ( horsepower , 2, raw = TRUE) , data = Auto ,
                    subset = train )
test_quad = mean(( Auto$mpg - predict (lm_fit_quad,Auto)) [-train ]^2)

# cubic fit
lm_fit_cubic <- lm( mpg ~ poly ( horsepower, 3, raw = TRUE) , data = Auto ,
                    subset = train )
test_cubic = mean(( Auto$mpg - predict (lm_fit_cubic,Auto)) [-train ]^2)

lm_fit_4 <- lm( mpg ~ poly ( horsepower, 4, raw = TRUE) , data = Auto ,
                    subset = train )
test_4 = mean(( Auto$mpg - predict (lm_fit_cubic,Auto)) [-train ]^2)

lm_fit_5 <- lm( mpg ~ poly ( horsepower, 5, raw = TRUE) , data = Auto ,
                subset = train )
test_5 = mean(( Auto$mpg - predict (lm_fit_cubic,Auto)) [-train ]^2)

par(mfrow = c(1, 1))
plot(1:5,c(test_err,test_quad,test_cubic,test_4,test_5), type = "b", col="blue",
     ylab = "Test MSE",
     xlab = "Flexibility",
     main = "Validation Test MSE")


# Leave-one-out cross-validation (LOOCV) method
library(boot)
#?glm()
#?cv.glm() :: delta (1x2) vector contain the cross-validation results, The
# first element is the standard k-fold CV estimate. The second is a biascorrected 
# version

set.seed(1)
loo_cv <- rep (0 , 10)
for(i in 1:10){
  glm_fit <- glm( mpg ~ poly ( horsepower , i ) , data = Auto )
  loo_cv[ i ] <- cv.glm(Auto , glm_fit ,K = dim(Auto)[1])$delta[1]
} 

plot(1:10,loo_cv,type = "b",col = "blue",
     ylab = "CV error",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation")

# k-fold validation method (Biad variace - trade off k=5, k=10)
set.seed(1)
kfold <- rep (0 , 10)
for(i in 1:10) {
  glm_fit <- glm(mpg ~ poly ( horsepower , i ) , data = Auto )
  kfold[ i ] <- cv.glm( Auto , glm_fit , K = 10)$delta[1]
}

lines(1:10,kfold,type = "b",col = "red", lty = 2)
legend("topright",legend=c("LOOCV","k = 10"),col = c("blue","red"),lty = 1:2)

