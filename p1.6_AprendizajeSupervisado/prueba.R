
# Carga las bibliotecas necesarias
library(gbm)
library(caTools)
library(corrplot)
library(viridis)
library(ggplot2)

# Configura las opciones
options(scipen = 999)
# establecer area de trabajo
setwd("C:/Users/alfon/Desktop/prueba en R")

# Lee tu archivo CSV en un dataframe de R
df.Wine <- read.csv(file = "WineQT.csv", header  = T, stringsAsFactors = F)

# Haz un boxplot de todas las variables de tu dataframe
par(mfrow=c(3, 4))
boxplot(df.Wine)

# Calcula las medias y desviaciones estándar de tus datos
medias <- colMeans(df.Wine)
desviacion_estandar <- apply(df.Wine, 2, sd)

# Define un umbral para detectar valores atípicos
umbral <-  2 

# Identifica los valores atípicos
valores_atipicos <- df.Wine[abs(df.Wine - medias) > umbral * desviacion_estandar]

# Reemplaza los valores atípicos por la media
df.Wine <- replace(df.Wine, df.Wine %in% valores_atipicos, medias)

# Divide tus datos en un conjunto de entrenamiento (80% de los datos) y un conjunto de prueba (20% de los datos)
Split <- sample.split(Y = df.Wine$alcohol, SplitRatio = 0.8)
df.Wine.Train <- subset(df.Wine, Split == T)
df.Wine.Test <- subset(df.Wine, Split == F)

# Calcula la matriz de correlación de tus variables y visualízala con un gráfico de corrplot
cor.Wine <- cor(df.Wine)
corrplot(cor.Wine,
        addCoef.col = "black",
        insig = "label_sig",
        method = "pie",
        type = "full",
        diag = F,
        col = viridis(n = 5, direction = 1),
        title = "Correlación WineQT")

# Entrena un modelo de Gradient Boosting Machine (GBM) en tus datos de entrenamiento
gbm.fit <- gbm(quality ~ ., data = df.Wine.Train, distribution = "gaussian", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, verbose = T)

# Muestra un resumen de tu modelo
summary(gbm.fit)

# Haz predicciones con tu modelo en los datos de prueba
predictions <- predict(gbm.fit, newdata = df.Wine.Test, n.trees = 1000)

# Calcula el error cuadrático medio (MSE) para evaluar el rendimiento de tu modelo
mse <- mean((df.Wine.Test$quality - predictions)^2)
print(paste("Error cuadrático medio: ", mse))

# Calcula el coeficiente de determinación (R^2) para evaluar el rendimiento de tu modelo
sst <- sum((df.Wine.Test$quality - mean(df.Wine.Test$quality))^2)
ssr <- sum((df.Wine.Test$quality - predictions)^2)
r2 <- 1 - (ssr / sst)
print(paste("Coeficiente de determinación R^2: ", r2))

# Crea un dataframe con las predicciones y los valores reales
df.Predictions <- data.frame(Predictions = predictions, Real = df.Wine.Test$quality)

# Crea un gráfico de dispersión de las predicciones vs los valores reales
ggplot(df.Predictions, aes(x = Real, y = Predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("Predicciones vs Valores Reales") +
  xlab("Valores Reales") +
  ylab("Predicciones")

# Crea un histograma de los residuales (diferencia entre las predicciones y los valores reales)
ggplot(df.Predictions, aes(x = Real - Predictions)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histograma de Residuales") +
  xlab("Residuales") +
  ylab("Frecuencia")


# Carga la biblioteca necesaria
library(gridExtra)

# Visualiza la importancia de las variables
importance <- summary(gbm.fit)
p1 <- ggplot(importance, aes(x = reorder(var, -rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Importancia de las Variables") +
  xlab("Variables") +
  ylab("Importancia Relativa")

# Visualiza el error de validación cruzada
best.iter <- gbm.perf(gbm.fit, method = "cv")
p2 <- ggplot(data = data.frame(trees = 1:gbm.fit$n.trees, cv.error = gbm.fit$cv.error), aes(x = trees, y = cv.error)) +
  geom_line() +
  geom_vline(xintercept = best.iter, linetype = 2, color = "red") +
  ggtitle("Error de Validación Cruzada") +
  xlab("Número de Árboles") +
  ylab("Error de Validación Cruzada")

# Combina los gráficos
grid.arrange(p1, p2, ncol = 2)



