library(caret)
library(glmnet)
library(dplyr)

set.seed(22)

setwd("~/Github/Statistical-learning-project/Dataset")

df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

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

df <- df %>% mutate_all(~(scale(.) %>% as.vector))
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))

df <- df[train_lines, ]

# Lasso regression
# Define control parameters for Lasso regression
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Lasso regression
# Train Lasso regression model with different lambda values
response_variable <- df$Sleep.Duration
lasso_model <- train(x = df[, -which(names(df) == "Sleep.Duration")], y = response_variable, method = "glmnet", trControl = ctrl, tuneLength = 10, preProcess = c("center", "scale"))

# Display the best lambda value selected by caret
print(lasso_model$bestTune)

# Calcolo del MSE del modello migliore
test_data <- df[-train_lines, ]  # Utilizziamo i dati non usati per il training come dati di test
predictions <- predict(lasso_model, newdata = test_data)
actual_values <- test_data$Sleep.Duration
mse <- mean((predictions - actual_values)^2)
print(paste("Mean Squared Error (MSE) del modello migliore:", mse))

# Ottieni i coefficienti del modello
coefficients <- coef(lasso_model$finalModel, s = lasso_model$bestTune$lambda)

# Stampa i coefficienti delle variabili e indica se sono significativi
significant_coeffs <- coefficients[c(coefficients[,1] != 0),]
print(significant_coeffs)

# Numero totale di variabili nel dataset
num_variables_total <- ncol(df) - 1  # Escludiamo la variabile di risposta

# Numero di variabili significative nel modello
num_variables_significant <- length(significant_coeffs)

if (num_variables_significant < num_variables_total) {
  cat("Sono state rimosse", num_variables_total - num_variables_significant, "variabili dal modello.\n")
} else if (num_variables_significant == num_variables_total) {
  cat("Non è stata rimossa nessuna variabile dal modello.\n")
} else {
  cat("Non ci sono abbastanza informazioni per determinare se sono state rimosse variabili.\n")
}

# Elenco delle variabili originali
original_variables <- c("GenderMale", "OccupationDoctor", "Blood.Pressure115/78", "BMI.CategoryObese", "Sleep.DisorderInsomnia")

# Trova le variabili rimosse
variables_removed <- setdiff(original_variables, rownames(significant_coeffs))

if (length(variables_removed) > 0) {
  cat("Le seguenti variabili sono state rimosse dal modello:\n")
  print(variables_removed)
} else {
  cat("Non sono state rimosse variabili dal modello.\n")
}

# Calcolo dei residui del modello
residui <- actual_values - predictions

# Test di normalità dei residui
shapiro_test <- shapiro.test(residui)
print("Shapiro-Wilk test per la normalità dei residui:")
print(shapiro_test)

# Disegna l'istogramma dei residui
hist(residui, main = "Istogramma dei Residui", xlab = "Residui")

# Stampa un messaggio se i residui non sono normalmente distribuiti
if (shapiro_test$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}
