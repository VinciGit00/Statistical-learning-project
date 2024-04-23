library(caret)

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

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

# Standardize predictors
preproc_values <- preProcess(df[, -which(names(df) == "Sleep.Duration")], method = c("center", "scale"))
df[, -which(names(df) == "Sleep.Duration")] <- predict(preproc_values, newdata = df[, -which(names(df) == "Sleep.Duration")])

# Split data into train and test sets
train_index <- createDataPartition(df$Sleep.Duration, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Ridge regression
# Define control parameters for Ridge regression
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train Ridge regression model with different lambda values
response_variable <- train_data$Sleep.Duration
ridge_model <- train(x = train_data[, -which(names(train_data) == "Sleep.Duration")], 
                     y = response_variable, 
                     method = "glmnet", 
                     trControl = ctrl, 
                     tuneLength = 10, 
                     preProcess = c("center", "scale"),
                     tuneGrid = expand.grid(.alpha = 0, .lambda = seq(0.001, 1, length = 10)))  # Ridge regression: alpha = 0

# Display the best lambda value selected by caret
print(ridge_model$bestTune)

# Calculate MSE of the best model
predictions <- predict(ridge_model, newdata = test_data)
mse <- mean((predictions - test_data$Sleep.Duration)^2)
print(paste("Mean Squared Error (MSE) of the best model:", mse))

# Get coefficients of the model
coefficients <- coef(ridge_model$finalModel, s = ridge_model$bestTune$lambda)

# Print coefficients of the variables
significant_coeffs <- coefficients[c(coefficients[, 1] != 0), ]
print(significant_coeffs)

# Number of total variables in the dataset
num_variables_total <- ncol(df) - 1  # Exclude the response variable

# Number of significant variables in the model
num_variables_significant <- length(significant_coeffs)

if (num_variables_significant < num_variables_total) {
  cat("The model removed", num_variables_total - num_variables_significant, "variables.\n")
} else if (num_variables_significant == num_variables_total) {
  cat("No variables were removed from the model.\n")
} else {
  cat("Not enough information to determine if variables were removed.\n")
}

# List of original variables
original_variables <- c("GenderMale", "OccupationDoctor", "Blood.Pressure115/78", "BMI.CategoryObese", "Sleep.DisorderInsomnia")

# Find removed variables
variables_removed <- setdiff(original_variables, rownames(significant_coeffs))

if (length(variables_removed) > 0) {
  cat("The following variables were removed from the model:\n")
  print(variables_removed)
} else {
  cat("No variables were removed from the model.\n")
}
