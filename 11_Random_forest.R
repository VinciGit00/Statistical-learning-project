library(caret)
library(glmnet)
library(dplyr)

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")
df$Blood.Pressure <- as.character(df$Blood.Pressure) # rendo i valori stringhe
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

library(randomForest)

#mtry = Number of variables randomly sampled as candidates at each split.
#ntree = Number of trees to grow.
bagg_model <- randomForest(Sleep.Duration ~ . ,data = train_df)

yhat <- predict(bagg_model, newdata = df[-train_lines,])

mse_train <- mean((yhat - df$Sleep.Duration[-train_lines])^2)

yhat_test <- predict(bagg_model, newdata = test_df)

mse_test <- mean((yhat_test - test_df$Sleep.Duration)^2)

residuals_mean <- mean(yhat_test - test_df$Sleep.Duration)

residuals_variance <- var(yhat_test - test_df$Sleep.Duration)

# how to change number of tree? 
# add ntree oprion 
bagg_model <- randomForest(Sleep.Duration ~ . ,data = train_df,
                           mtry = ncol(df)-1, importance = TRUE, ntree = 100)

importance(bagg_model)

# Random Forest
# default p = sqrt(m) -> see doc 
forest_model <- randomForest(Sleep.Duration ~ . ,data = train_df,
                             mtry = floor(sqrt(ncol(df)-1)), importance = TRUE, ntree = 100)

yhat_test_forest <- predict(forest_model, newdata = test_df)

res_forest <- yhat_test_forest - test_df$Sleep.Duration

mean_res_forest <- mean(res_forest)

mse_forest <- mean((res_forest)^2) 

importance(forest_model)

shapiro_test_forest <- shapiro.test(res_forest)
print("Shapiro-Wilk test per la normalità dei residui:")
print(shapiro_test_forest)

# Disegna l'istogramma dei residui
hist(res_forest, main = "Residual histogram", xlab = "Residuals")

# Stampa un messaggio se i residui non sono normalmente distribuiti
if (shapiro_test_forest$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}
R2_train <- 1 - sum((forest_model$fitted.values - train_df$Sleep.Duration)^2) / sum((mean(train_df$Sleep.Duration) - train_df$Sleep.Duration)^2)

# Aggiunta delle metriche richieste
print(paste("MSE Train:", mse_train))
print(paste("MSE Test:", mse_test))
print(paste("Residuals Mean:", residuals_mean))
print(paste("Residuals Variance:", residuals_variance))
shapiro_test_forest <- shapiro.test(res_forest)
print("Shapiro-Wilk test per la normalità dei residui:")
print(shapiro_test_forest)

ks_test_forest <- ks.test(res_forest, "pnorm")
print("Kolmogorov-Smirnov test per la normalità dei residui:")
print(ks_test_forest)

# QQ Plot
qqnorm(res_forest)
qqline(res_forest)