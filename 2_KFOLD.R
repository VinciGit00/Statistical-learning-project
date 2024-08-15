rm(list = ls())
graphics.off()  
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
library(akima)
library(MASS)
install.packages('boot')
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

View(df)

##### K FOLD #####
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

b <-  paste0("Sleep.Duration ~ ", paste0("poly(",use,", 2, raw=TRUE)", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")

## K FOLD LINEARE ##
kfold <- rep (0 , 10)
for(i in 1:10) {
  lm_fit <- glm(Sleep.Duration ~ ., data = df)
  kfold[ i ] <- cv.glm( df , glm_fit , K = 10)$delta[1]
}

plot(1:10,kfold,type = "b",col = "red", lty = 2)
legend("topright",legend=c("LOOCV","k = 10"),col = c("blue","red"),lty = 1:2)
MSE <- mean(kfold)


## K FOLD POLINOMIALE ##
kfold <- rep (0 , 10)
for(i in 1:10) {
  glm_fit <- glm(Sleep.Duration ~ poly(Age, 2, raw=TRUE)+poly(Physical.Activity.Level, 2, raw=TRUE)+poly(Stress.Level, 2, raw=TRUE)+poly(Heart.Rate, 2, raw=TRUE)+poly(Daily.Steps, 2, raw=TRUE)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data=df )
  kfold[ i ] <- cv.glm( df , glm_fit , K = 10)$delta[1]
}

plot(1:10,kfold,type = "b",col = "red", lty = 2)
legend("topright",legend=c("LOOCV","k = 10"),col = c("blue","red"),lty = 1:2)
MSE <- mean(kfold)

##### TEST LR2 MODEL #####
# Splitting the dataset into training (80%) and test (20%) sets
set.seed(22)
train_indices <- createDataPartition(df$Sleep.Duration, p = 0.8, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Fit the linear regression model on the training data
lr2 <- glm(Sleep.Duration ~ ., data = train_data)

# Predicting Sleep Duration on the test set
predictions <- predict(lr2, newdata = test_data)

# Calculating R-squared on the test set
SSE <- sum((test_data$Sleep.Duration - predictions)^2)
SST <- sum((test_data$Sleep.Duration - mean(test_data$Sleep.Duration))^2)
test_R2 <- 1 - SSE/SST
print(paste("Test R-squared: ", test_R2))





