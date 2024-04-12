## LAB 3 -> Non linear regression 
library(MASS)
library(ISLR2)

model <- lm(mpg ~ horsepower, data = Auto)
fitted <- predict.lm(model,data.frame(horsepower = 1:230),
                     type = "response",
                     level = 0.95,
                     interval = "prediction"
                     )


plot(fitted[,1],type = 'l',ylab = "mpg",xlab = "horsepower",
     main = "Predict value - CI")
# Confidence bands
lines(fitted[,2],lty=2,col = 'red')
lines(fitted[,3],lty=2,col = 'red')
points(Auto$horsepower,Auto$mpg)


# Step functions
# break the range of X into bins, and fit a different constant in each bin

?Wage

plot(Wage$age,Wage$wage)
plot(Wage$education, Wage$wage)

table(cut(Wage$age,4))

step_fun <- lm(wage ~ cut(age,4) + education,data = Wage);
summary(step_fun)

# Spline function-> use the splines functions
# bs() -> basis function; default: cubic spline with 3dof
# ns() -> natural spline 
library ( splines )
age_domain <- seq(min(Wage$age),max(Wage$age),1);
fit <- lm(wage ~ bs(age,knots = c(20,35,38,40,42,45,60)),data = Wage)
pred <- predict ( fit , newdata = list ( age =  age_domain) , se = T )
plot (Wage$age, Wage$wage , col = " gray " )
lines( age_domain , pred$fit , lwd = 2)
lines( age_domain , pred$fit + 1.96 * pred$se , lty = "dashed" )
lines( age_domain , pred$fit - 1.96 * pred$se , lty = "dashed" )

fit <- lm(wage ~ bs(age,df=30),data = Wage)
# Specifica i gradi di libertà
pred <- predict ( fit , newdata = list ( age =  age_domain) , se = T )
plot (Wage$age, Wage$wage , col = " gray " )
lines( age_domain , pred$fit , lwd = 2)

# Smothing Spline
# Idea: evitare la scelta della locazione dei knots
fit2 <- smooth.spline ( Wage$age , Wage$wage , cv = TRUE )
fit2$df # select the lambda required to have this $df
plot(fit2)

# Local Regression
# Requires to define a kernel and this one defines the number of data point to fit
fit <- loess( wage ~ age , span = .1 , data = Wage )
fit2 <- loess( wage ~ age , span = .5 , data = Wage )
plot(Wage$age, Wage$wage , col = " gray " )
lines( age_domain , predict ( fit , data.frame( age = age_domain ) ) ,
        col = " red " , lwd = 2)
lines( age_domain , predict ( fit2 , data.frame ( age = age_domain ) ) ,
          col = " blue " , lwd = 2)

# GAM
# install.packages('gam')
# install.packages('akima')
library(foreach)
library(gam)
library(akima)
# s() -> smoothing spline
train <- sample(dim(Wage)[1], round(dim(Wage)[1]*0.7))

gam_model <- gam(wage ~ s(year,4) + s(age,5) + education, data = Wage[train,]); 
plot(gam_model,se=TRUE)

pred_value <- predict(gam_model,newdata = Wage[-train,])
plot(Wage$wage[-train],pred_value)

# add local regression with span = 0.7
gam_model <- gam(wage ~ s(year,4) + lo(age, span = 0.7) + education,data = Wage[train,])

# add interaction term 
gam_model <- gam(wage ~ s(year,4) + lo(age,education, span = 0.7) + education,data = Wage[train,])
plot(gam_model,se=TRUE)


## Example of GAM model in more complex Datasets
require(dplyr)

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

# 1) Import dataset & add header information
setwd("G:/Il mio Drive/Universita/Didattica/Dalmine/Statistical Learning/Script R/")
Data <- read.csv("Dataset/Bike-Sharing-Dataset/hour.csv");
View(Data)

Data$dteday <- NULL
Data$instant <- NULL
Data$registered <- NULL; 
Data$casual <- NULL; 

# replace int value with "Categorical" value (provide by README file)

#Data$weathersit[Data$weathersit == 1] <- "Clear";
#Data$weathersit[Data$weathersit == 2] <- "Mist";
#Data$weathersit[Data$weathersit == 3] <- "Light Rain";
#Data$weathersit[Data$weathersit == 4] <- "Heavy Rain";

# Convert integer in Categorical
## yr -> step function
#Data$mnth <- as.factor(Data$mnth);
## hr -> natural spline  
Data$yr <- as.factor(Data$yr)
Data$holiday <- as.factor(Data$holiday)
Data$weekday <- as.factor(Data$weekday)
Data$workingday <- as.factor(Data$workingday)  
Data$weathersit <- as.factor(Data$weathersit)


# simple descriptive statistics (grpstat)
str(Data);
summary(Data); 
boxplot(Data$temp)
barplot(tabulate(Data$weekday)); 


# Data$cnt # count of total rental bikes including both casual and registered

# gam model: spline + linear + step functions + local regression + polinomial & iteraction

## yr -> step function
## mnth -> step function
## hr -> natural spline  
## holiday -> step function
## weekday -> step function 
## workingday -> step function 
## weathersit -> step function 
## temp -> smothing spline
## hum -> natural spline 
## windspeed -> local regression
## cnt -> ~y

#    
model_gam <- gam(cnt~ yr + cut(mnth,breaks=c(1,4,8,12)) + ns(hr,4) + holiday +
                   + weekday + workingday + weathersit +s(temp,5) + ns(hum,4) + lo(windspeed,span =0.4), data=Data);

plot(model_gam, se = TRUE)


