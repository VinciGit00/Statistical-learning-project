rm(list = ls())
install.packages("caret")
library(splines)
library(caret)

df <- read.csv("C:/Statistical-learning-project/Dataset/Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Types of the columns of the dataset:
# Gender: character
# Age:integer
# Occupation: character
# Sleep.Duration: numeric
# Physical.Activity.Level:integer
# Stress.Level: integer
# BMI.Category: character
# Blood.Pressure: character 
# Heart.Rate: integer
# Daily.Steps: integer
# Sleep.Disorder: character

# Transformation to dummy variables
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

## Removing one element for each dummy
# They were removed:
# GenderMale
# OccupationDoctor
# Blood.Pressure115/78
# BMI.CategoryObese
# Sleep.DisorderInsomnia

df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))
View(df)










## SPLINES START 

###### (regressore: Stress.Level) ###### 

# definisco i range del dataset su cui è definita la spline
stress_domain <- seq(min(df$Stress.Level),max(df$Stress.Level),1);

# bs -> cubic spline
fit <- lm(Sleep.Duration ~ bs(Stress.Level, knots = c(1,2,3,4,5,6,7,8,9)),data = df)

pred <- predict ( fit , newdata = list (Stress.Level =  stress_domain) , se = T )
plot (df$Stress.Level, df$Sleep.Duration , col = " gray " )
lines( stress_domain, pred$fit , lwd = 2)
lines( stress_domain, pred$fit + 1.96 * pred$se , lty = "dashed" )
lines( stress_domain, pred$fit - 1.96 * pred$se , lty = "dashed" )


###### (regressore: Heart.Rate) ###### 
heart_domain <- seq(min(df$Heart.Rate),max(df$Heart.Rate),1);

fit <- lm(Sleep.Duration ~ bs(Heart.Rate, knots = c(65,70, 72, 75, 80, 85)),data = df)
#fit <- lm(Sleep.Duration ~ bs(Heart.Rate,df=30),data = df)

pred <- predict ( fit , newdata = list (Heart.Rate =  heart_domain) , se = T )
plot (df$Heart.Rate, df$Sleep.Duration , col = " gray " )
lines( heart_domain, pred$fit , lwd = 2)
lines( heart_domain, pred$fit + 1.96 * pred$se , lty = "dashed" )
lines( heart_domain, pred$fit - 1.96 * pred$se , lty = "dashed" )

# SMOOTHING SPLINE (INUTILE):
fit2 <- smooth.spline ( df$Heart.Rate, df$Sleep.Duration, cv = TRUE ) # facciamo cross validazione per stimare il valore del coefficiente di penalizzazione dei nodi
fit2$df # estraggo il valore di lambda ottimale
pred <- predict ( fit2 , newdata = list (Heart.Rate =  heart_domain) , se = T )
plot(fit2)


