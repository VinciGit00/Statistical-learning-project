# Load required libraries
library(gam)
library(akima)
library(MASS)
library(boot)
library(glmnet)
library(caret)
set.seed(22)

##### DATASET SETUP ######
setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted_gam.csv")

df$Blood.Pressure <- as.character(df$Blood.Pressure) # Convert Blood Pressure values to strings

dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)

df_dummies <- predict(dummy_transform, newdata = df)

df <- cbind(df, df_dummies)

# Remove original categorical columns
df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

# Remove specific dummy variables
df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure11578`, BMI.CategoryObese, Sleep.DisorderInsomnia))

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # Standardize the dataset

# Rename columns to make them valid R variable names
colnames(df)[which(names(df) == "OccupationSales Representative")] <- "OccupationSales.Representative"
colnames(df)[which(names(df) == "BMI.CategoryNormal Weight")] <- "BMI.CategoryNormal.Weight"
colnames(df)[which(names(df) == "OccupationSoftware Engineer")] <- "OccupationSoftware.Engineer"
colnames(df)[which(names(df) == "Sleep.DisorderSleep Apnea")] <- "Sleep.DisorderSleep.Apnea"

#############################
# Remove columns with fewer samples
df <- subset(df, select = -c(`Blood.Pressure11776`, `Blood.Pressure11875`, `Blood.Pressure11876`, `Blood.Pressure11977`, `Blood.Pressure12179`, `Blood.Pressure12280`, `Blood.Pressure12582`, `Blood.Pressure12683`, `Blood.Pressure12884`, `Blood.Pressure12885`, `Blood.Pressure12984`, `Blood.Pressure13086`, `Blood.Pressure13186`, `Blood.Pressure13287`, `Blood.Pressure13588`, `Blood.Pressure13991`, `Blood.Pressure14090`, `Blood.Pressure14292`))
df <- subset(df, select = -c(`OccupationManager`, `OccupationSales.Representative`, `OccupationScientist`, `OccupationSoftware.Engineer`))
#############################

df <- df %>% mutate_all(~(scale(.) %>% as.vector)) # Re-standardize after removing columns

# Split the data into training and test sets
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))
df_train <- df[train_lines, ]
df_test <- df[-train_lines, ]

# Lasso regression
response_variable <- df_train$Sleep.Duration

# Train Lasso regression model with different lambda values
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
lasso_model <- train(x = df_train[, -which(names(df_train) == "Sleep.Duration")], y = response_variable, 
                     method = "glmnet", trControl = ctrl, tuneLength = 10, 
                     preProcess = c("center", "scale"))

# Display the best lambda value selected by caret
print(lasso_model$bestTune)

# Calculate the MSE on the test set
test_predictions <- predict(lasso_model, newdata = df_test)
test_actual_values <- df_test$Sleep.Duration
mse_test <- mean((test_predictions - test_actual_values)^2)
print(paste("Mean Squared Error (MSE) on the test set:", mse_test))

# Calculate R-squared
r_squared <- 1 - mse_test/var(test_actual_values)
print(paste("R-squared:", r_squared))

# Calculate deviance explained
deviance_explained <- 1 - (sum((test_actual_values - mean(test_actual_values))^2) / sum((test_actual_values - test_predictions)^2))
print(paste("Deviance explained:", deviance_explained))

# Calculate residual deviance
residual_deviance <- sum((test_actual_values - test_predictions)^2)
print(paste("Residual deviance:", residual_deviance))

# Calculate total deviance
total_deviance <- sum((test_actual_values - mean(test_actual_values))^2)
print(paste("Total deviance:", total_deviance))

# Calculate MSE on the training set
train_predictions <- predict(lasso_model, newdata = df_train)
train_actual_values <- df_train$Sleep.Duration
mse_train <- mean((train_predictions - train_actual_values)^2)
print(paste("Mean Squared Error (MSE) on the training set:", mse_train))

# Calculate residuals on the training set
train_residui <- train_actual_values - train_predictions
residual_mean_train <- mean(train_residui)
print(paste("Residuals mean on training set:", residual_mean_train))

residual_variance_train <- var(train_residui)
print(paste("Residuals variance on training set:", residual_variance_train))

# Calculate residuals on the test set
test_residui <- test_actual_values - test_predictions
residual_mean_test <- mean(test_residui)
print(paste("Residuals mean on test set:", residual_mean_test))

residual_variance_test <- var(test_residui)
print(paste("Residuals variance on test set:", residual_variance_test))

# Kolmogorov-Smirnov test for normality of residuals
ks_test <- ks.test(test_residui, "pnorm", mean = mean(test_residui), sd = sd(test_residui))
print("Kolmogorov-Smirnov test for normality of residuals:")
print(ks_test)

# Shapiro-Wilk test for normality of residuals
shapiro_test <- shapiro.test(test_residui)
print("Shapiro-Wilk test for normality of residuals:")
print(shapiro_test)

# Plot residuals
hist(test_residui, main = "Residual distribution", xlab = "Residual")
qqnorm(test_residui)
qqline(test_residui)
