rm(list = ls())
graphics.off()  
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
library(gplots)
library(akima)
library(boot)
library(MASS)
set.seed(22);

##### DATASET SETUP ######
df <- read.csv("./Dataset/Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

df$Blood.Pressure<-as.character(df$Blood.Pressure) # rendo i valori stringhe

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # standardizzo

colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"



##### SUDDIVIDO IN TEST E TRAIN (70-30) #####
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))


###### BOOTSTRAP LINEARE ###### 

# perform bootstrap simulation
fun_boot <- function(formula, data, index){
  # uso dataset di train quello ottenuto dal resample
  lm_fit <- lm(formula, data = data[index,])
  # uso stesso dataset di validazione per tutti
  pred_value <- predict(lm_fit,newdata = df[-train,])
  MSE_TEST <- mean((df$Sleep.Duration[-train]-pred_value)^2)  
  #return (summary(lm_fit)$r.squared)
  
}

result <- boot(df[train,],fun_boot,R = 100, formula=Sleep.Duration ~ .);
View(result)
plot(result)
mean(result[["t"]])
min(result[["t"]])
max(result[["t"]])


# compare results with summary(model)
# Why are different? -> the standard formulas rely on certain assumptions (they 
# depend on the unknown parameter sigma, there is a non-linear relationship in
# the data and so the residuals from a linear fit will be inflated). 

plot(Auto$horsepower,Auto$mpg, pch='x',col="blue",
     ylab = "MPG", xlab = "Horsepower", main = "Non-Linear relationship", )
grid(10, 10, lwd = 2)


model <- lm(mpg ~ horsepower + I(horsepower^2),Auto)
summary(model)

fun_boot <- function(data,index){
  linear <- lm(mpg ~ horsepower + I(horsepower^2),data = data, subset = index);
  return (linear$coefficients)
}

boot(Auto,fun_boot,R = 1000);

## Example 3: Variance of estimator of the sample mean
X = rnorm(1000,0,1);
mean(X)
hist(X,20)

boot_fun <- function(data,index){
  mean(data[index])
}

# ~ N(u,sigma^2/n)
boot(X,boot_fun,R=1000)



