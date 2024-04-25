rm(list = ls())
graphics.off()  
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
#install.packages('gplots')
library(gplots)
library(akima)
library(MASS)
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


train <- sample(dim(df)[1], round(dim(df)[1]*0.7))


##### BACKWARD LINEAR REGRESSION ##### 
full.model <- lm(Sleep.Duration ~., data = df)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "forward", 
                      trace = FALSE)
summary(step.model)

b <- paste0("Sleep.Duration ~ ", paste0(names(step.model$coefficient)[-1], sep="", collapse="+"))


##### LINEAR MULTIPLE REGRESSION ##### 
lm_fit <- lm(Sleep.Duration ~ Age+Physical.Activity.Level+Stress.Level+Heart.Rate+Daily.Steps+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationManager+OccupationNurse+OccupationSales.Representative+OccupationSalesperson+OccupationScientist+OccupationSoftware.Engineer+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure11776+Blood.Pressure11875+Blood.Pressure11876+Blood.Pressure11977+Blood.Pressure12080+Blood.Pressure12179+Blood.Pressure12280+Blood.Pressure12580+Blood.Pressure12582+Blood.Pressure12683+Blood.Pressure12884+Blood.Pressure12885+Blood.Pressure12984+Blood.Pressure13085+Blood.Pressure13086+Blood.Pressure13186+Blood.Pressure13287+Blood.Pressure13588+Blood.Pressure13590+Blood.Pressure13991+Blood.Pressure14090+Blood.Pressure14095+Blood.Pressure14292+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data = df[train,])
summary(lm_fit) 


# Confidence intervals on coefficients
confint(lm_fit, level = 0.95) # ATTENZIONE!! ALCUNI COEFFICIENTI HANNO ZERO DENTRO INTERVALLO DI CONFIDENZA!!

##### CALCOLO MSE #####
pred_value <- predict(lm_fit,newdata = df[-train,], interval = "confidence")
plot(df$Sleep.Duration[-train],pred_value[,1]) # confronto i valori previsti con quelli di test
#plotCI(df$Sleep.Duration[-train], pred_value[,1], pred_value[,3], li=pred_value[2])
MSE_TRAIN <- mean(lm_fit$residuals^2) 
MSE_TEST <- mean((df$Sleep.Duration[-train]-pred_value)^2)  


##### LEVERAGE PLOT #####
## (influnza dei datapoint su stima modello, alto valore = outlier) ##
plot(hatvalues(lm_fit), col="blue", 
     ylab = "leverage",
     xlab = "Observation",
     main = "leverage") # 1/n < leverage < 1 


###### ANALISI RESIDUI ######
res_mean <- mean(lm_fit$residuals)
hist(lm_fit$residuals,40,
     xlab = "Residual",
     main = "Empirical residual distribution") 
plot(lm_fit$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(lm_fit$residuals),digits = 4),
                                      "- var:", round(var(lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)
ks.test(lm_fit$residuals, 'pnorm') # test normalità kolgomorov-smirnov (se pvalue > 0.05 allora dati sono normali)


