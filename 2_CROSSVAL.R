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

#############################
# Tolgo colonne pressione con pochi sample
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`,  `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))
#############################


##### K FOLD #####
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

b <-  paste0("Sleep.Duration ~ ", paste0("poly(",use,", i)", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")
b1 <-  paste0("Sleep.Duration ~ ", paste0(use, collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")


## LOOCV ##

mses <- list()
ctrl <- trainControl(method = "LOOCV")
model <- train(Sleep.Duration ~ Age+Physical.Activity.Level+Stress.Level+Heart.Rate+Daily.Steps+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p1 <- model$results$RMSE^2

model <- train(Sleep.Duration ~ poly(Age, 2)+poly(Physical.Activity.Level, 2)+poly(Stress.Level, 2)+poly(Heart.Rate, 2)+poly(Daily.Steps, 2)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p2 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 3)+poly(Physical.Activity.Level, 3)+poly(Stress.Level, 3)+poly(Heart.Rate, 3)+poly(Daily.Steps, 3)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p3 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 4)+poly(Physical.Activity.Level, 4)+poly(Stress.Level, 4)+poly(Heart.Rate, 4)+poly(Daily.Steps, 4)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p4 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 5)+poly(Physical.Activity.Level, 5)+poly(Stress.Level, 5)+poly(Heart.Rate, 5)+poly(Daily.Steps, 5)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p5 <- model$results$RMSE^2

plot(1:5,mses,type = "b",col = "blue",
     ylab = "MSE",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation"
)







## KFOLD ##
mses <- list()
ctrl <- trainControl(method = "cv", number=10)
model <- train(Sleep.Duration ~ Age+Physical.Activity.Level+Stress.Level+Heart.Rate+Daily.Steps+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p1 <- model$results$RMSE^2

model <- train(Sleep.Duration ~ poly(Age, 2)+poly(Physical.Activity.Level, 2)+poly(Stress.Level, 2)+poly(Heart.Rate, 2)+poly(Daily.Steps, 2)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p2 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 3)+poly(Physical.Activity.Level, 3)+poly(Stress.Level, 3)+poly(Heart.Rate, 3)+poly(Daily.Steps, 3)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p3 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 4)+poly(Physical.Activity.Level, 4)+poly(Stress.Level, 4)+poly(Heart.Rate, 4)+poly(Daily.Steps, 4)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p4 <- model$results$RMSE^2


model <- train(Sleep.Duration ~ poly(Age, 5)+poly(Physical.Activity.Level, 5)+poly(Stress.Level, 5)+poly(Heart.Rate, 5)+poly(Daily.Steps, 5)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df, method = "lm", trControl = ctrl)
print(model)
mses$p5 <- model$results$RMSE^2

plot(1:5,mses,type = "b",col = "blue",
     ylab = "MSE",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation"
)















kfold <- rep (0 , a)
for(i in 1:a) {
  glm_fit <- glm(Sleep.Duration ~ poly(Age, i)+poly(Physical.Activity.Level, i)+poly(Stress.Level, i)+poly(Heart.Rate, i)+poly(Daily.Steps, i)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data=df )
  kfold[ i ] <- cv.glm( df , glm_fit , K = 10)$delta[1]
}

lines(1:a,kfold,type = "b",col = "red", lty = 10)
legend("topright",legend=c("kfold","k = 10"),col = c("blue","red"),lty = 1:2)
MSE_kfold <- mean(loo_cv)
MSE_loocv <- mean(kfold)
