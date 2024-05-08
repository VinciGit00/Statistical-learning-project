library(caret)
library(glmnet)
library(dplyr)
library(tree)

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

#############################
# Tolgo colonne pressione con pochi sample
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`,  `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))
#############################

df <- df %>% mutate_all(~(scale(.) %>% as.vector))
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))

df <- df[train_lines, ]

names(df)[names(df) == 'OccupationSales Representative'] <- 'OccupationSalesRepresentative'
names(df)[names(df) == 'OccupationSoftware Engineer'] <- 'OccupationSoftwareEngineer'
names(df)[names(df) == 'BMI.CategoryNormal Weight'] <- 'BMI.CategoryNormalWeight'
names(df)[names(df) == 'Sleep.DisorderSleep Apnea'] <- 'Sleep.DisorderSleepApnea'
# Divisione dei dati in set di addestramento e test
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))
train_df <- df[train_lines, ]
test_df <- df[-train_lines, ]

# Tree method

# train model
tree_model <- tree( Sleep.Duration ~ . ,data= train_df, split = "gini")

# show result 
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0, cex = 0.5, pos = 2, offset = 0.5, adj = c(0.5, 0.5), digits = 4)

pred_value <- predict(tree_model, newdata = test_df)
table(pred_value, test_df$Sleep.Duration)

res <- pred_value - test_df$Sleep.Duration
mean_res <- mean(res)
mse <- mean((res)^2) 

shapiro_test <- shapiro.test(res)
print("Shapiro-Wilk test per la normalità dei residui:")
print(shapiro_test)

# Disegna l'istogramma dei residui
hist(res, main = "Istogramma dei Residui", xlab = "Residui")

# Stampa un messaggio se i residui non sono normalmente distribuiti
if (shapiro_test$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}

ks_test <- ks.test(res, "pnorm", mean = mean(res), sd = sd(res))
print("Kolmogorov-Smirnov test for normality of residuals:")
print(ks_test)

# Calculate R-squared for training set
R2_train <- 1 - sum((tree_model$fitted.values - train_df$Sleep.Duration)^2) / sum((mean(train_df$Sleep.Duration) - train_df$Sleep.Duration)^2)

# Calculate R-squared for testing set
R2_test <- 1 - sum((pred_value - test_df$Sleep.Duration)^2) / sum((mean(train_df$Sleep.Duration) - test_df$Sleep.Duration)^2)

# Mean Squared Error (MSE) for training set
MSE_train <- mean((tree_model$fitted.values - train_df$Sleep.Duration)^2)

# Mean Squared Error (MSE) for testing set
MSE_test <- mean((pred_value - test_df$Sleep.Duration)^2)

# Mean of Residuals
mean_res <- mean(res)

# Residual Variance
residual_variance <- var(res)

# Print the calculated metrics
print(paste("R-squared (Training):", R2_train))
print(paste("R-squared (Testing):", R2_test))
print(paste("MSE (Training):", MSE_train))
print(paste("MSE (Testing):", MSE_test))
print(paste("Mean of Residuals:", mean_res))
print(paste("Residual Variance:", residual_variance))

hist(res, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(res)
qqline(res)

# Calcolo dell'R^2 test library(caret)
r_squared_test <- cor(pred_value, test_df$Sleep.Duration)^2
print(paste("R-squared test library(caret):", r_squared_test))
