rm(list = ls())

library(splines)
library(caret)
require(dplyr)
graphics.off()  
library(foreach)
library(gam)
library(akima)


# ATTENZIONE! IL DATASET DEVE AVERE LE ENTRY DELLA COLONNA BLOOD PRESSURE SENZA LE "/"
df <- read.csv("C:/Statistical-learning-project/Dataset/Sleep_health_and_lifestyle_dataset_adjusted.csv")

df$Blood.Pressure<-as.character(df$Blood.Pressure) # rendo i valori stringhe

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))


# tolgo spazi vuoti da nomi colonne
colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # standardizzo

View(df)

# SUDDIVIDO IN TEST E TRAIN
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))

# CREO MODELLO GAM
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

# splines di grado 4
a <-   paste0("Sleep.Duration ~ ", paste0("s(",use,",4)", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")
# smoothing splines
b <-   paste0("Sleep.Duration ~ ", paste0("s(",use,")", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")


#model_gam <- gam(Sleep.Duration ~ s(Age,4)+s(Physical.Activity.Level,4)+s(Stress.Level,4)+s(Heart.Rate,4)+s(Daily.Steps,4)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df[train,])  
model_gam <- gam(Sleep.Duration ~ s(Age)+s(Physical.Activity.Level)+s(Stress.Level)+s(Heart.Rate)+s(Daily.Steps)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data= df[train,])


summary(model_gam)
plot(model_gam, se = TRUE)

# CALCOLO MSE
pred_value <- predict(model_gam,newdata = df[-train,])
plot(df$Sleep.Duration[-train],pred_value) # confronto i valori previsti con quelli di test
MSE <- mean(model_gam$residuals^2)









