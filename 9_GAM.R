# Rimuove tutte le variabili dall'ambiente di lavoro
rm(list = ls())
graphics.off()  

# Carica le librerie necessarie
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
library(akima)
library(MASS)
set.seed(22)

##### DATASET SETUP ######
# ATTENZIONE! IL DATASET DEVE AVERE LE ENTRY DELLA COLONNA BLOOD PRESSURE SENZA LE "/"
df <- read.csv("./Dataset/Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

df$Blood.Pressure <- as.character(df$Blood.Pressure) # rendo i valori stringhe

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))

# Tolgo spazi vuoti dai nomi delle colonne
colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # standardizzo

# Suddividi in test e train (70-30)
set.seed(22)  # Imposta un seme per la riproducibilità
train_indices <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

##### CREO MODELLO GAM #####
model_gam <- gam(Sleep.Duration ~ s(Age) + s(Physical.Activity.Level) + s(Stress.Level) + s(Heart.Rate) + s(Daily.Steps) + 
                  GenderFemale + OccupationAccountant + OccupationEngineer + OccupationLawyer + OccupationManager + 
                  OccupationNurse + OccupationSales.Representative + OccupationSalesperson + OccupationScientist + 
                  OccupationSoftware.Engineer + OccupationTeacher + BMI.CategoryNormal + BMI.CategoryNormal.Weight + 
                  BMI.CategoryOverweight + Blood.Pressure11575 + Blood.Pressure12080 + Blood.Pressure12580 + 
                  Blood.Pressure13085 + Blood.Pressure13590 + Blood.Pressure14095 + Sleep.DisorderNone + 
                  Sleep.DisorderSleep.Apnea, 
                  data = df_train)

summary(model_gam)
plot(model_gam, se = TRUE)

##### CALCOLO R² DI TEST #####
# Predizioni sui dati di test
pred_value_test <- predict(model_gam, newdata = df_test)

# Calcola l'R² sui dati di test
actual_values_test <- df_test$Sleep.Duration
rss_test <- sum((pred_value_test - actual_values_test)^2)  # Residual Sum of Squares
tss_test <- sum((actual_values_test - mean(actual_values_test))^2)  # Total Sum of Squares
r_squared_test <- 1 - (rss_test / tss_test)

# Stampa dell'R² sui dati di test
print(paste("R-squared for GAM model (test data):", r_squared_test))

# Calcolo del MSE sui dati di test
MSE_test <- mean((actual_values_test - pred_value_test)^2)
print(paste("MSE for GAM model (test data):", MSE_test))

###### ANALISI RESIDUI ######
res_mean <- mean(model_gam$residuals)
hist(model_gam$residuals)
ks.test(model_gam$residuals, 'pnorm') # test normalità kolgomorov-smirnov (se D > 0.05 allora dati sono normali)
