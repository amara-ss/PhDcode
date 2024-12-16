

uniqueness_imp <- function(data, target_column_position, ntree = 500) {
  

  # Obtener el nombre y los datos de la columna objetivo
  target_column_name <- colnames(data)[target_column_position]
  target_data <- data[, target_column_position]
  
  # Determinar el tipo de problema (regresión o clasificación)
  problem_type <- ifelse(is.numeric(target_data), "regression", "classification")
  if (problem_type == "classification") {
    data[, target_column_position] <- as.factor(data[, target_column_position])
  }
  
  # Seleccionar las columnas predictoras
  predictor_columns <- setdiff(colnames(data), target_column_name)
  
  # Ajustar el modelo de bosque aleatorio con todas las variables
  full_model <- randomForest(as.formula(paste(target_column_name, "~ .")), data = data, ntree = ntree)
  
  # Calcular el error de predicción o tasa de error del modelo completo sobre las muestras OOB
  full_model_metric <- if (problem_type == "regression") {
    full_model$mse[length(full_model$mse)]
  } else {
    full_model$err.rate[nrow(full_model$err.rate), "OOB"]
  }
  
  # Inicializar un vector para almacenar el incremento en el error o tasa de error para cada variable
  increased_metric <- numeric(length(predictor_columns))
  
  # Calcular la importancia por unicidad para cada variable predictora
  for (i in seq_along(predictor_columns)) {
    var <- predictor_columns[i]
    
    # Imprimir el nombre de la variable actual
    print(var)
    
    # Entrenar un nuevo modelo excluyendo la variable actual
    reduced_formula <- as.formula(paste(target_column_name, "~ . -", var))
    reduced_model <- randomForest(reduced_formula, data = data, ntree = ntree)
    
    # Calcular el error de predicción o tasa de error del modelo reducido sobre las muestras OOB
    reduced_model_metric <- if (problem_type == "regression") {
      reduced_model$mse[length(reduced_model$mse)]
    } else {
      reduced_model$err.rate[nrow(reduced_model$err.rate), "OOB"]
    }
    
    # Calcular el incremento relativo en el error de predicción o tasa de error
    increased_metric[i] <- 100 * (reduced_model_metric - full_model_metric) / full_model_metric
  }
  
  # Crear un dataframe para mostrar los resultados
  result <- data.frame(Variable = predictor_columns, UniquenessImportance = increased_metric)
  result <- result[result$UniquenessImportance > 0, ]
  sorted_result <- result[order(-result$UniquenessImportance), ]
  
  # Crear gráfico de barras
  plot_title <- if (problem_type == "regression") {
    "Uniqueness Importance for Regression"
  } else {
    "Uniqueness Importance for Classification"
  }
  
  print(ggplot(sorted_result, aes(x = reorder(Variable, UniquenessImportance), y = UniquenessImportance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          theme_minimal() +
          labs(title = plot_title,
               x = "Variables",
               y = "Uniqueness Importance (%)") +
          theme(axis.text.x = element_text(angle = 0, hjust = 0.5)))
  
  # Retornar los resultados ordenados
  return(sorted_result)
}
