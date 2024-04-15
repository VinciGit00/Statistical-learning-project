# Carica il pacchetto splines
library(splines)

# Lettura dei dati da un file CSV
df <- read.csv("Sleep_health_and_lifestyle_dataset_adjusted.csv")

# Lista per memorizzare i modelli per ciascun parametro testato
models <- list()

# Lista per memorizzare le devianze per ciascun parametro testato
deviances <- numeric()

# Lista per memorizzare i grafici CP per ciascun parametro testato
cp_plots <- list()

# Lista dei parametri da testare
parameters <- c("Age", "Physical.Activity.Level", "Stress.Level", "Heart.Rate", "Daily.Steps")

# Numero di gradi di libertà desiderato per il termine di smoothing
df_value <- 5

# Impostazione del parametro di smoothing
lambda <- 0.1

# Iterazione sui parametri
for (param in parameters) {
  # Creazione del termine di smoothing per il parametro corrente utilizzando smooth.spline()
  spline_fit <- with(df, smooth.spline(df[[param]], Sleep.Duration, df = df))
  
  # Definizione del modello lineare utilizzando il termine di smoothing
  model_formula <- as.formula(paste("Sleep.Duration ~ bs(", param, ", df=", df_value, ")", sep = ""))
  current_model <- lm(model_formula, data = df)
  
  # Salvataggio del modello
  models[[param]] <- current_model
  
  # Calcolo della devianza
  deviances <- c(deviances, deviance(current_model))
  
  # Calcolo del grafico CP
  cp_plots[[param]] <- spline_fit$y - predict(current_model)
}

# Trovare il parametro con la devianza minima
best_param <- parameters[which.min(deviances)]

# Stampare il modello migliore
print(models[[best_param]])

# Stampare il grafico CP del modello migliore
plot(cp_plots[[best_param]], main = paste("CP Plot for", best_param))
