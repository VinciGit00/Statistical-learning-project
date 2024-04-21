rm(list = ls())
graphics.off()  
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
library(akima)
library(MASS)
#install.packages('boot')
library(boot)
set.seed(22);

##### DATASET SETUP ######
df <- read.csv("./Dataset/Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

df$Blood.Pressure<-as.character(df$Blood.Pressure) # rendo i valori stringhe

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # standardizzo

colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"

#View(df)

##### K FOLD #####
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

b <-  paste0("Sleep.Duration ~ ", paste0("poly(",use,", i)", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")

## LOOCV ##
a <- 10
loo_cv <- rep (0 , a)
for(i in 1:a) {
  glm_fit <- glm(Sleep.Duration ~ poly(Age, i)+poly(Physical.Activity.Level, i)+poly(Stress.Level, i)+poly(Heart.Rate, i)+poly(Daily.Steps, i)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data=df )
  loo_cv[ i ] <- cv.glm( df , glm_fit , K = dim(df)[1])$delta[1]
}
plot(1:a,loo_cv,type = "b",col = "blue",
     ylab = "CV error",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation"
)

## KFOLD ##
kfold <- rep (0 , a)
for(i in 1:a) {
  glm_fit <- glm(Sleep.Duration ~ poly(Age, i)+poly(Physical.Activity.Level, i)+poly(Stress.Level, i)+poly(Heart.Rate, i)+poly(Daily.Steps, i)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data=df )
  kfold[ i ] <- cv.glm( df , glm_fit , K = 10)$delta[1]
}

lines(1:a,kfold,type = "b",col = "red", lty = 10)
legend("topright",legend=c("kfold","k = 10"),col = c("blue","red"),lty = 1:2)
MSE_kfold <- mean(loo_cv)
MSE_loocv <- mean(kfold)
