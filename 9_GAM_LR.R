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

View(df)

##### SUDDIVIDO IN TEST E TRAIN (70-30) #####
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))

##### CREO MODELLO GAM #####
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

# Smoothing splines
b <-   paste0("Sleep.Duration ~ ", paste0("lo(", use, ")", collapse= "+"), "+", paste0(dontuse, collapse="+"))

model_gam <- gam(Sleep.Duration ~ lo(Age) + lo(Physical.Activity.Level) + lo(Stress.Level) + lo(Heart.Rate) + lo(Daily.Steps) + GenderFemale + OccupationAccountant + OccupationEngineer + OccupationLawyer + OccupationManager + OccupationNurse + OccupationSales.Representative + OccupationSalesperson + OccupationScientist + OccupationSoftware.Engineer + OccupationTeacher + BMI.CategoryNormal + BMI.CategoryNormal.Weight + BMI.CategoryOverweight + Blood.Pressure11575 + Blood.Pressure11776 + Blood.Pressure11875 + Blood.Pressure11876 + Blood.Pressure11977 + Blood.Pressure12080 + Blood.Pressure12179 + Blood.Pressure12280 + Blood.Pressure12580 + Blood.Pressure12582 + Blood.Pressure12683 + Blood.Pressure12884 + Blood.Pressure12885 + Blood.Pressure12984 + Blood.Pressure13085 + Blood.Pressure13086 + Blood.Pressure13186 + Blood.Pressure13287 + Blood.Pressure13588 + Blood.Pressure13590 + Blood.Pressure13991 + Blood.Pressure14090 + Blood.Pressure14095 + Blood.Pressure14292 + Sleep.DisorderNone + Sleep.DisorderSleep.Apnea, data = df[train,])

summary(model_gam)
plot(model_gam, se = TRUE)

##### CALCOLO MSE #####
pred_value <- predict(model_gam, newdata = df[-train,])
plot(df$Sleep.Duration[-train], pred_value) # confronto i valori previsti con quelli di test
MSE <- mean(model_gam$residuals^2)

###### ANALISI RESIDUI ######
res_mean <- mean(model_gam$residuals)
hist(model_gam$residuals)
ks.test(model_gam$residuals, 'pnorm') # test normalità Kolmogorov-Smirnov (se pvalue > 0.05 allora dati sono normali)

# Calcolo del R²
r_squared <- summary(model_gam)$r.squared
r_squared
