rm(list = ls())

library(splines)
library(caret)
require(dplyr)
graphics.off()  
library(foreach)
library(gam)
library(akima)



df <- read.csv("C:/Statistical-learning-project/Dataset/Sleep_health_and_lifestyle_dataset_adjusted.csv")

df$Blood.Pressure<-as.character(df$Blood.Pressure)


dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))
View(df)
colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"

dim(df)

column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

a <-   paste0("Sleep.Duration ~ ", paste0("s(",use,",4)", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")

## ATTENZIONE NON SONO STATI USATI I DUMMY DI PRESSIONE PERCHE' LO SLASH VERTICALE ROMPE TUTTO
model_gam <- gam(Sleep.Duration ~ s(Age,4)+s(Physical.Activity.Level,4)+s(Stress.Level,4)+s(Heart.Rate,4)+s(Daily.Steps,4)+GenderFemale+GenderMale+OccupationAccountant+OccupationDoctor+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryObese+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11578+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderInsomnia+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df)  


plot(model_gam, se = TRUE)

