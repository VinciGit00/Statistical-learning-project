library(gbm)
library(caret)
library(dplyr)
library(MASS)
library(boot)

set.seed(22)

##### DATASET SETUP ######
setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

df$Blood.Pressure <- as.character(df$Blood.Pressure) # rendo i valori stringhe

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

# Tolgo colonne pressione con pochi sample
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`,  `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))

# Suddividi in set di allenamento e test (70-30)
set.seed(22)
train_indices <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

# Modello GBM
ntree = 5000
boost_model <- gbm(Sleep.Duration ~ ., data = df_train, 
                   distribution = "gaussian", n.trees = ntree,
                   interaction.depth = 4, shrinkage = 0.01, verbose = F)

# Predizioni sui dati di test
yhat <- predict(boost_model, newdata = df_test, n.trees = ntree)

# Calcola l'R² sui dati di test
actual_values_test <- df_test$Sleep.Duration
rss_test <- sum((yhat - actual_values_test)^2)  # Residual Sum of Squares
tss_test <- sum((actual_values_test - mean(actual_values_test))^2)  # Total Sum of Squares
r_squared_test <- 1 - (rss_test / tss_test)

# Stampa dell'R² sui dati di test
print(paste("R-squared for GBM model (test data):", r_squared_test))

# Calcolo del MSE sui dati di test
MSE_test <- mean((actual_values_test - yhat)^2)
print(paste("MSE for GBM model (test data):", MSE_test))
