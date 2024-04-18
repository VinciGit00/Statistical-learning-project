# Load necessary packages
library(caret)
library(glmnet)

# Set the working directory and read the dataset
setwd("~/Github/Statistical-learning-project/Dataset")
df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

## Removing one element for each dummy
# They were removed:
# GenderMale
# OccupationDoctor
# Blood.Pressure115/78
# BMI.CategoryObese
# Sleep.DisorderInsomnia

# Transformation to dummy variables
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)
df_dummies <- predict(dummy_transform, newdata = df)
df <- cbind(df, df_dummies)

# Remove original categorical variables
df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

## Removing one level for each dummy variable
df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))

# Define control parameters for Ridge regression
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Ridge regression
# Train Ridge regression model with different lambda values
response_variable <- df$Sleep.Duration
ridge_model <- train(x = df[, -which(names(df) == "Sleep.Duration")], y = response_variable, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, by = 0.001)), preProcess = c("center", "scale"))

# Display the best lambda value selected by caret
print(ridge_model$bestTune)
