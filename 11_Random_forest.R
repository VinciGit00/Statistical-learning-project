library(caret)
library(glmnet)
library(dplyr)

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Types of the columns of the dataset:
# Gender: character
# Age:integer
# Occupation: character
# Sleep.Duration: numeric
# Physical.Activity.Level:integer
# Stress.Level: integer
# BMI.Category: character
# Blood.Pressure: character 
# Heart.Rate: integer
# Daily.Steps: integer
# Sleep.Disorder: character

# Transformation to dummy variables
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

## Removing one element for each dummy
# They were removed:
# GenderMale
# OccupationDoctor
# Blood.Pressure115/78
# BMI.CategoryObese
# Sleep.DisorderInsomnia

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))

df <- df %>% mutate_all(~(scale(.) %>% as.vector))
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))

df <- df[train_lines, ]
