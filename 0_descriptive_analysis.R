library(caret)
library(glmnet)
library(dplyr)
library(tree)
library ( ISLR2 )

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

mean(df$Age)
sd(df$Age)

mean(df$Heart.Rate)
sd(df$Heart.Rate)

mean(df$Daily.Steps)
sd(df$Daily.Steps)

mean(df$Sleep.Duration)
sd(df$Sleep.Duration)

mean(df$Stress.Level)
sd(df$Stress.Level)

mean(df$Quality.of.Sleep)
sd(df$Quality.of.Sleep)


mean(df$Physical.Activity.Level)
sd(df$Physical.Activity.Level)

par(cex.axis = 2, cex.main = 2)  
hist(df$Sleep.Duration, main = "Sleep duration",  xlab = "Sleep duration")

par(cex.axis = 2, cex.main = 2)  
hist(df$Age, main = "Age",  xlab = "Age")

par(cex.axis = 2, cex.main = 2)  
hist(df$Quality.of.Sleep, main = "Quality of Sleep",  xlab = "Quality of Sleep")

par(cex.axis = 2, cex.main = 2)  
hist(df$Heart.Rate, main = "heart rate",  xlab = "hearth rate")

par(cex.axis = 2, cex.main = 2)  
hist(df$Daily.Steps, main = "daily steps",  xlab = "daily steps")


par(cex.axis = 2, cex.main = 2)  
hist(df$Stress.Level, main = "stress level",  xlab = "stess level")



barplot(table(df$Occupation), main = "Occupation Distribution")
barplot(table(df$Blood.Pressure), main = "Blood pressure")
