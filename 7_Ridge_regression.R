# Load required libraries
library(caret)
library(glmnet)
library(dplyr)

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
train_lines <- sample(dim(df)[1], round(dim(df)[1] * 0.7))
df_train <- df[train_lines, ]
df_test <- df[-train_lines, ]

# Ridge regression
response_variable <- df_train$Sleep.Duration

# Define control parameters for Ridge regression
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train Ridge regression model with different lambda values
ridge_model <- train(x = df_train[, -which(names(df_train) == "Sleep.Duration")], 
                     y = response_variable, 
                     method = "glmnet", 
                     trControl = ctrl, 
                     tuneLength = 10, 
                     preProcess = c("center", "scale"),
                     tuneGrid = expand.grid(.alpha = 0, .lambda = seq(0.001, 1, length = 10)))  # Ridge regression: alpha = 0

# Display the best lambda value selected by caret
print(ridge_model$bestTune)

# Calculate the MSE on the test set
test_predictions <- predict(ridge_model, newdata = df_test)
test_actual_values <- df_test$Sleep.Duration
mse_test <- mean((test_predictions - test_actual_values)^2)
print(paste("Mean Squared Error (MSE) on the test set:", mse_test))

# Obtain model coefficients
coefficients <- coef(ridge_model$finalModel, s = ridge_model$bestTune$lambda)

# Print coefficients of variables
significant_coeffs <- coefficients[c(coefficients[, 1] != 0), ]
print(significant_coeffs)

# Number of variables in the dataset
num_variables_total <- ncol(df) - 1  # Excluding the response variable

# Number of significant variables in the model
num_variables_significant <- length(significant_coeffs)

if (num_variables_significant < num_variables_total) {
  cat("Sono state rimosse", num_variables_total - num_variables_significant, "variabili dal modello.\n")
} else if (num_variables_significant == num_variables_total) {
  cat("Non è stata rimossa nessuna variabile dal modello.\n")
} else {
  cat("Non ci sono abbastanza informazioni per determinare se sono state rimosse variabili.\n")
}

# List of original variables
original_variables <- c("GenderMale", "OccupationDoctor", "Blood.Pressure115/78", "BMI.CategoryObese", "Sleep.DisorderInsomnia")

# Find removed variables
variables_removed <- setdiff(original_variables, rownames(significant_coeffs))

if (length(variables_removed) > 0) {
  cat("Le seguenti variabili sono state rimosse dal modello:\n")
  print(variables_removed)
} else {
  cat("Non sono state rimosse variabili dal modello.\n")
}

# Calculate residuals for the model
residuals <- test_actual_values - test_predictions

# Test normality of residuals
shapiro_test <- shapiro.test(residuals)
print("Shapiro-Wilk test for normality of residuals:")
print(shapiro_test)

# Plot histogram of residuals
hist(residuals, main = "Residual distribution", xlab = "Residuals")

# Print a message if residuals are not normally distributed
if (shapiro_test$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}

# Add QQ plot of residuals
qqnorm(residuals)
qqline(residuals)

# Kolmogorov-Smirnov test for normality of residuals
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
print("Kolmogorov-Smirnov test for normality of residuals:")
print(ks_test)

# Train Ridge model using glmnet directly
ridge_model_glmnet <- cv.glmnet(x = as.matrix(df[, -which(names(df) == "Sleep.Duration")]), 
                                y = response_variable, 
                                alpha = 0,  # Ridge Regression (alpha = 0)
                                nfolds = 10, 
                                standardize = TRUE)  # Pre-process data
print(ridge_model_glmnet)

# Print lambda values plot
plot(ridge_model_glmnet)
print(ridge_model_glmnet$lambda.min)

# Calculate R-squared
r_squared_test <- cor(test_predictions, test_actual_values)^2
print(paste("R-squared test library(caret):", r_squared_test))
