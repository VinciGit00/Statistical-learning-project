rm(list = ls())
graphics.off()  
library(splines)
library(caret)
require(dplyr)
library(foreach)
library(gam)
library(akima)
library(MASS)
#library(mgcv)
set.seed(22);


##### DATASET SETUP ######
# ATTENZIONE! IL DATASET DEVE AVERE LE ENTRY DELLA COLONNA BLOOD PRESSURE SENZA LE "/"
df <- read.csv("./Dataset/Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

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

#############################
# Tolgo colonne pressione con pochi sample
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`,  `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))

#############################

##### SUDDIVIDO IN TEST E TRAIN (70-30) #####
train <- sample(dim(df)[1], round(dim(df)[1]*0.7))


##### CREO MODELLO GAM #####
column_names <- colnames(df)
use <- column_names[c(1,3,4,5,6)] 
dontuse <- column_names[-c(1,2,3,4,5,6)] 

# Smoothing splines
b <-   paste0("Sleep.Duration ~ ", paste0("s(",use,")", collapse= "+"),"+", paste0(dontuse,collapse="+"),collapse="")

model_gam <- gam(Sleep.Duration ~ s(Age)+s(Physical.Activity.Level)+s(Stress.Level)+s(Heart.Rate)+s(Daily.Steps)+GenderFemale+OccupationAccountant+OccupationEngineer+OccupationLawyer+OccupationNurse+OccupationSalesperson+OccupationTeacher+BMI.CategoryNormal+BMI.CategoryNormal.Weight+BMI.CategoryOverweight+Blood.Pressure11575+Blood.Pressure12080+Blood.Pressure12580+Blood.Pressure13085+Blood.Pressure13590+Blood.Pressure14095+Sleep.DisorderNone+Sleep.DisorderSleep.Apnea, data= df[train,])
summary(model_gam)

plot(model_gam, se = TRUE)


##### CALCOLO MSE #####
pred_value <- predict(model_gam,newdata = df[-train,], interval = "confidence")
plot(df$Sleep.Duration[-train],pred_value[,1]) # confronto i valori previsti con quelli di test
#plotCI(df$Sleep.Duration[-train], pred_value[,1], pred_value[,3], li=pred_value[2])
MSE_TRAIN <- mean(model_gam$residuals^2) 
MSE_TEST <- mean((df$Sleep.Duration[-train]-pred_value)^2)  


###### ANALISI RESIDUI ######
res_mean <- mean(model_gam$residuals)
res_var <- var(model_gam$residuals)
hist(model_gam$residuals,50,
     xlab = "Residual",
     main = "Residual distribution") 
plot(model_gam$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(model_gam$residuals),digits = 4),
                                      "- var:", round(var(model_gam$residuals),digits = 2)))
qqnorm(model_gam$residuals, pch = 1, frame = FALSE)
qqline(model_gam$residuals, col = "steelblue", lwd = 2)

abline(c(0,0),c(0,length(model_gam$residuals)), col= "red", lwd = 2)
# test di normalità (if pvalue > 0.05 residuals have normal distribution)
shapiro.test(model_gam$residuals)
ks.test(model_gam$residuals, 'pnorm')










