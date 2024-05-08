# Rimuove tutte le variabili dall'ambiente di lavoro
rm(list = ls())

# Installa il pacchetto caret, se non è già installato
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Carica le librerie necessarie
library(splines)
library(caret)

df <- read.csv("C:/Statistical-learning-project/Dataset/Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Tipi delle colonne del dataset:
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

# Trasformazione in variabili dummy
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

## Rimozione di un elemento per ciascuna variabile dummy
# Sono state rimosse:
# GenderMale
# OccupationDoctor
# Blood.Pressure115/78
# BMI.CategoryObese
# Sleep.DisorderInsomnia

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))
View(df)

# SPLINES START 

###### (regressore: Stress.Level) ###### 

# Definizione dei range del dataset su cui è definita la spline
stress_domain <- seq(min(df$Stress.Level), max(df$Stress.Level), 1)

# bs -> cubic spline
fit <- lm(Sleep.Duration ~ bs(Stress.Level, knots = c(1,2,3,4,5,6,7,8,9)), data = df)

pred <- predict(fit, newdata = list(Stress.Level = stress_domain), se = TRUE)
plot(df$Stress.Level, df$Sleep.Duration, col = "gray")
lines(stress_domain, pred$fit, lwd = 2)
lines(stress_domain, pred$fit + 1.96 * pred$se, lty = "dashed")
lines(stress_domain, pred$fit - 1.96 * pred$se, lty = "dashed")

# Calcolo del R²
summary(fit)$r.squared

###### (regressore: Heart.Rate) ###### 
heart_domain <- seq(min(df$Heart.Rate), max(df$Heart.Rate), 1)

fit <- lm(Sleep.Duration ~ bs(Heart.Rate, knots = c(65,70, 72, 75, 80, 85)), data = df)

pred <- predict(fit, newdata = list(Heart.Rate = heart_domain), se = TRUE)
plot(df$Heart.Rate, df$Sleep.Duration, col = "gray")
lines(heart_domain, pred$fit, lwd = 2)
lines(heart_domain, pred$fit + 1.96 * pred$se, lty = "dashed")
lines(heart_domain, pred$fit - 1.96 * pred$se, lty = "dashed")

# Calcolo del R²
summary(fit)$r.squared
