library(caret)
library(glmnet)
library(dplyr)
library(tree)
library ( ISLR2 )

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")
df$Blood.Pressure<-as.character(df$Blood.Pressure) # rendo i valori stringhe
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

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))

df <- df %>% mutate_all(~(scale(.) %>% as.vector))
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))

names(df)[names(df) == 'OccupationSales Representative'] <- 'OccupationSalesRepresentative'
names(df)[names(df) == 'OccupationSoftware Engineer'] <- 'OccupationSoftwareEngineer'
names(df)[names(df) == 'BMI.CategoryNormal Weight'] <- 'BMI.CategoryNormalWeight'
names(df)[names(df) == 'Sleep.DisorderSleep Apnea'] <- 'Sleep.DisorderSleepApnea'
# Divisione dei dati in set di addestramento e test
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))
train_df <- df[train_lines, ]
test_df <- df[-train_lines, ]

# Tree method

# train model
tree_model <- tree( Sleep.Duration ~ . ,data= train_df, split = "gini")

# show result 
summary(tree_model)
plot(tree_model)
text(tree_model,pretty = 0)

pred_value <- predict(tree_model, newdata = test_df)
table(pred_value, test_df$Sleep.Duration)

res <- pred_value - test_df$Sleep.Duration
mean_res <- mean(res)
mse <- mean((res)^2) 

shapiro_test <- shapiro.test(res)
print("Shapiro-Wilk test per la normalità dei residui:")
print(shapiro_test)

# Disegna l'istogramma dei residui
hist(res, main = "Istogramma dei Residui", xlab = "Residui")

# Stampa un messaggio se i residui non sono normalmente distribuiti
if (shapiro_test$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}