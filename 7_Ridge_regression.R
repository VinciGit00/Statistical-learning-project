library(caret)
library(glmnet)
library(dplyr)

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
train_lines <- sample(dim(df)[1], round(dim(df)[1] * 0.7))

df <- df[train_lines, ]

# Ridge regression
# Define control parameters for Ridge regression
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train Ridge regression model with different lambda values
response_variable <- df$Sleep.Duration
ridge_model <- train(x = df[, -which(names(df) == "Sleep.Duration")], 
                     y = response_variable, 
                     method = "glmnet", 
                     trControl = ctrl, 
                     tuneLength = 10, 
                     preProcess = c("center", "scale"),
                     tuneGrid = expand.grid(.alpha = 0, .lambda = seq(0.001, 1, length = 10)))  # Ridge regression: alpha = 0

# Display the best lambda value selected by caret
print(ridge_model$bestTune)

# Calcolo del MSE del modello migliore
test_data <- df[-train_lines, ]  # Utilizziamo i dati non usati per il training come dati di test
predictions <- predict(ridge_model, newdata = test_data)
actual_values <- test_data$Sleep.Duration
mse <- mean((predictions - actual_values)^2)
print(paste("Mean Squared Error (MSE) del modello migliore:", mse))

# Ottieni i coefficienti del modello
coefficients <- coef(ridge_model$finalModel, s = ridge_model$bestTune$lambda)

# Stampa i coefficienti delle variabili
significant_coeffs <- coefficients[c(coefficients[, 1] != 0), ]
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
hist(residui, main = "Residual distribution", xlab = "Residuals analysis")

# Stampa un messaggio se i residui non sono normalmente distribuiti
if (shapiro_test$p.value < 0.05) {
  cat("I residui non seguono una distribuzione normale (p-value < 0.05).\n")
} else {
  cat("I residui seguono una distribuzione normale (p-value >= 0.05).\n")
}

# Aggiungi il QQ plot dei residui
qqnorm(residui)
qqline(residui)

ks_test <- ks.test(residui, "pnorm", mean = mean(residui), sd = sd(residui))
print("Kolmogorov-Smirnov test for normality of residuals:")
print(ks_test)

# Addestramento del modello Ridge utilizzando glmnet
ridge_model <- cv.glmnet(x = as.matrix(df[, -which(names(df) == "Sleep.Duration")]), 
                         y = response_variable, 
                         alpha = 0,  # Ridge Regression (alpha = 0)
                         nfolds = 10, 
                         standardize = TRUE)  # Pre-processa i dati
print(ridge_model)

# Print lambda values plot
plot(ridge_model)
print(ridge_model$lambda.min)

# Calcolo dell'R^2 test library(caret)
r_squared_test <- cor(predictions, actual_values)^2
print(paste("R-squared test library(caret):", r_squared_test))
