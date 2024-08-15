# Rimuove tutte le variabili dall'ambiente di lavoro
rm(list = ls())

# Installa il pacchetto caret, se non è già installato
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Carica le librerie necessarie
library(splines)
library(caret)

# Lettura dei dati
df <- read.csv("C:/Statistical-learning-project/Dataset/Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Trasformazione in variabili dummy
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)
df_dummies <- predict(dummy_transform, newdata = df)
df <- cbind(df, df_dummies)
df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

# Rimozione delle variabili dummy non utilizzate
df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))

# Suddividi il dataset in training e test set
set.seed(22)
train_indices <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

###### (regressore: Stress.Level) ###### 

# Fit con spline cubica sui dati di training
fit_stress <- lm(Sleep.Duration ~ bs(Stress.Level, knots = c(1,2,3,4,5,6,7,8,9)), data = df_train)

# Predizioni sui dati di test
pred_stress_test <- predict(fit_stress, newdata = df_test)

# Calcolo dell'R² sui dati di test
actual_values_stress_test <- df_test$Sleep.Duration
rss_stress <- sum((pred_stress_test - actual_values_stress_test)^2)  # Residual Sum of Squares
tss_stress <- sum((actual_values_stress_test - mean(actual_values_stress_test))^2)  # Total Sum of Squares
r_squared_stress_test <- 1 - (rss_stress / tss_stress)

# Stampa dell'R² sui dati di test per Stress.Level
print(paste("R-squared for Stress Level model (test data):", r_squared_stress_test))

###### (regressore: Heart.Rate) ###### 

# Fit con spline cubica sui dati di training
fit_heart <- lm(Sleep.Duration ~ bs(Heart.Rate, knots = c(65,70,72,75,80,85)), data = df_train)

# Predizioni sui dati di test
pred_heart_test <- predict(fit_heart, newdata = df_test)

# Calcolo dell'R² sui dati di test
actual_values_heart_test <- df_test$Sleep.Duration
rss_heart <- sum((pred_heart_test - actual_values_heart_test)^2)  # Residual Sum of Squares
tss_heart <- sum((actual_values_heart_test - mean(actual_values_heart_test))^2)  # Total Sum of Squares
r_squared_heart_test <- 1 - (rss_heart / tss_heart)

# Stampa dell'R² sui dati di test per Heart Rate
print(paste("R-squared for Heart Rate model (test data):", r_squared_heart_test))
