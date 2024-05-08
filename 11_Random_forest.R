library(caret)
library(glmnet)
library(dplyr)
library(randomForest)

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")
df$Blood.Pressure <- as.character(df$Blood.Pressure) # rendo i valori stringhe

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

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

# Random Forest
forest_model <- randomForest(Sleep.Duration ~ . ,data = train_df,
                             mtry = floor(sqrt(ncol(df)-1)), importance = TRUE, ntree = 100)

yhat_test_forest <- predict(forest_model, newdata = test_df)

res_forest <- yhat_test_forest - test_df$Sleep.Duration

mean_res_forest <- mean(res_forest)

mse_forest <- mean((res_forest)^2) 

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

# Calcolo dell'R^2 test library(caret)
r_squared_test <- cor(yhat_test_forest, test_df$Sleep.Duration)^2
print(paste("R-squared test library(caret):", r_squared_test))

# Calcolo dell'lr^2 test library(caret)
lr_squared_test <- cor(yhat_test_forest, test_df$Sleep.Duration, method = "pearson")^2
print(paste("lr-squared test library(caret):", lr_squared_test))
