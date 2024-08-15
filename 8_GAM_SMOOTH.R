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
#library(mgcv)
set.seed(22)

##### DATASET SETUP ######
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

# Tolgo colonne pressione con pochi sample
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`, `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))

##### SUDDIVIDO IN TEST E TRAIN (70-30) #####
set.seed(22)  # Imposta un seme per la riproducibilità
train_indices <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

##### CREO MODELLO GAM #####
model_gam <- gam(Sleep.Duration ~ s(Age) + s(Physical.Activity.Level) + s(Stress.Level) + s(Heart.Rate) + s(Daily.Steps) + 
                  GenderFemale + OccupationAccountant + OccupationEngineer + OccupationLawyer + OccupationNurse + 
                  OccupationSalesperson + OccupationTeacher + BMI.CategoryNormal + BMI.CategoryNormal.Weight + 
                  BMI.CategoryOverweight + Blood.Pressure11575 + Blood.Pressure12080 + Blood.Pressure12580 + 
                  Blood.Pressure13085 + Blood.Pressure13590 + Blood.Pressure14095 + Sleep.DisorderNone + 
                  Sleep.DisorderSleep.Apnea, 
                  data = df_train)

# Predizioni sui dati di test
pred_value <- predict(model_gam, newdata = df_test)

# Calcolo dell'R² sui dati di test
actual_values_test <- df_test$Sleep.Duration
rss_test <- sum((pred_value - actual_values_test)^2)  # Residual Sum of Squares
tss_test <- sum((actual_values_test - mean(actual_values_test))^2)  # Total Sum of Squares
r_squared_test <- 1 - (rss_test / tss_test)

# Stampa dell'R² sui dati di test
print(paste("R-squared for GAM model (test data):", r_squared_test))
