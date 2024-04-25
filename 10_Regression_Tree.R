library(caret)
library(tree)
library(glmnet)
library(dplyr)

set.seed(22)

# Caricamento dei dati
df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Trasformazione delle variabili categoriche in dummy variables
dummy_transform <- dummyVars(~ Gender + Occupation + BMI.Category + Blood.Pressure + Sleep.Disorder, data = df)
df_dummies <- predict(dummy_transform, newdata = df)
df <- cbind(df, df_dummies)
df <- df[, !(names(df) %in% c("Gender", "Occupation", "BMI.Category", "Blood.Pressure", "Sleep.Disorder"))]

## Rimozione di una variabile per ogni dummy
# Variabili rimosse: GenderMale, OccupationDoctor, Blood.Pressure115/78, BMI.CategoryObese, Sleep.DisorderInsomnia
df <- subset(df, select = -c(GenderMale, OccupationDoctor, `Blood.Pressure115/78`, BMI.CategoryObese, Sleep.DisorderInsomnia))

# Standardizzazione delle variabili predittive
df <- df %>% mutate_all(~(scale(.) %>% as.vector))

# Divisione dei dati in set di addestramento e test
train_lines <- sample(dim(df)[1], round(dim(df)[1]*0.7))
train_df <- df[train_lines, ]
test_df <- df[-train_lines, ]

# Creazione del controllo di addestramento per la cross-validazione
ctrl <- trainControl(method = "cv", number = 10)

# Addestramento del modello
tree_model <- train(
  Sleep.Duration ~ . - Age,
  method = "tree",
  data = train_df,
  trControl = ctrl
)

# Stampa delle informazioni sul modello
print(tree_model)

# Valutazione delle prestazioni del modello sui dati di addestramento
predictions_train <- predict(tree_model, newdata = train_df)
confusionMatrix(predictions_train, train_df$Sleep.Duration)

# Valutazione delle prestazioni del modello sui dati di test
predictions_test <- predict(tree_model, newdata = test_df)
confusionMatrix(predictions_test, test_df$Sleep.Duration)

# Cross-validation per la selezione dei migliori parametri
print(tree_model)

# Potatura dell'albero (esempio con un'altezza massima di 5)
pruned_tree <- prune.tree(tree_model$finalModel, best = 5)
print(pruned_tree)
