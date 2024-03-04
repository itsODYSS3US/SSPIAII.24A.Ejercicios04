source("Lib.Preprocess.R")
options(scipen = 999)
df.Wine <- read.csv(file = "Datasets/WineQT.csv", header  = T, stringsAsFactors = F)

df.Wine$Id <-NULL



par(mfrow=c(3, 4))

#Datos atípicos
boxplot(df.Wine$quality, main="Quality")
boxplot(df.Wine$fixed.acidity, main="Fixed Acidity")
boxplot(df.Wine$volatile.acidity, main="Volatile Acidity")
boxplot(df.Wine$citric.acid, main="Citric Acid")
boxplot(df.Wine$residual.sugar, main="Residual Sugar")
boxplot(df.Wine$chlorides, main="Chlorides")
boxplot(df.Wine$free.sulfur.dioxide, main="Free Sulfur Dioxide")
boxplot(df.Wine$total.sulfur.dioxide, main="Total Sulfur Dioxide")
boxplot(df.Wine$density, main="Density")
boxplot(df.Wine$pH, main="pH")
boxplot(df.Wine$sulphates, main="Sulphates")
boxplot(df.Wine$alcohol, main="Alcohol")

boxplot(df.Wine)


# Calcular la media y la desviación estándar
# vector <- c()
# vector <- append(mean(df.Wine$quality), mean(df.Wine$fixed.acidity))
# vector

medias <- colMeans(df.Wine)
medias
desviacion_estandar <- apply(df.Wine, 2, sd)

#Obtener el número de columnas del dataset.
vector_medias <- numeric(length(df.Wine))

# Calcular la media de cada columna y almacenarla en el vector_medias.
for (i in 1:ncol(df.Wine)) {
  vector_medias[i] <- mean(df.Wine[, i])
}
vector_medias

vector_desviaciones_estandar <- numeric(length(df.Wine))
for(i in 1:ncol(df.Wine)){
    vector_desviaciones_estandar[i] <- sd(df.Wine[[i]])
}
vector_desviaciones_estandar

# desviacion_estandar <- apply(df.Wine, 2, sd)

# Definir un umbral para identificar valores atípicos
umbral <-  2 

# Identificar valores atípicos
datos_limpio <- df.Wine


for(i in 1:ncol(df.Wine)){
  
  valores_atipicos <- df.Wine[,i][abs(df.Wine[,i] - vector_medias[i]) > umbral * vector_desviaciones_estandar[i]]
  
  
  datos_limpio[,i][datos_limpio[,i] %in% valores_atipicos] <- vector_medias[i]
}
View(datos_limpio)


val_atipicos_fixedAcidity <- df.Wine$fixed.acidity[abs(df.Wine$fixed.acidity-vector_medias[1])> umbral * vector_desviaciones_estandar[1]]
datos_limpio$fixed.acidity[datos_limpio$fixed.acidity %in% val_atipicos_fixedAcidity] <- vector_medias[1]


val_atipicos_residualsugar <- df.Wine$residual.sugar[abs(df.Wine$residual.sugar-vector_medias[4])> 0.4 * vector_desviaciones_estandar[4]]
datos_limpio$residual.sugar[datos_limpio$residual.sugar %in% val_atipicos_residualsugar] <- vector_medias[4]

val_atipicos_chlorides <- df.Wine$chlorides[abs(df.Wine$chlorides-vector_medias[5])> 0.4 * vector_desviaciones_estandar[5]]
datos_limpio$chlorides[datos_limpio$chlorides %in% val_atipicos_chlorides] <- vector_medias[5]

# boxplot(datos_limpio$quality, main="Quality")
# boxplot(datos_limpio$fixed.acidity, main="Fixed Acidity")

# boxplot(datos_limpio$volatile.acidity, main="Volatile Acidity")
# boxplot(datos_limpio$citric.acid, main="Citric Acid")
# boxplot(datos_limpio$residual.sugar, main="Residual Sugar")
boxplot(datos_limpio$chlorides, main="Chlorides")
# boxplot(datos_limpio$free.sulfur.dioxide, main="Free Sulfur Dioxide")
boxplot(datos_limpio$total.sulfur.dioxide, main="Total Sulfur Dioxide")
# boxplot(datos_limpio$density, main="Density")
# boxplot(datos_limpio$pH, main="pH")
boxplot(datos_limpio$sulphates, main="Sulphates")
# boxplot(datos_limpio$alcohol, main="Alcohol")

summary(datos_limpio)

# Identifica los índices de los valores atípicos
indices_atipicos <- which(abs(df$variable - media) > umbral * desviacion)

# Reemplaza los valores atípicos por la media
df$variable[indices_atipicos] <- media


# Imprimir los resultados
print("Datos originales:")
print(datos)
print("Datos sin valores atípicos:")
print(datos_limpio)


#Entrenamiento y prueba
Split <- sample.split(Y = df.Wine$alcohol, SplitRatio = 0.8)
df.Wine.Train <- subset(df.Wine, Split == T)
df.Wine.Test <- subset(df.Wine, Split == F)

#Matriz de correlación.
cor.Wine <- cor(df.Wine)
corrplot(cor.Wine,
        addCoef.col = "black",
        insig = "label_sig",
        method = "pie",
        type = "full",
        diag = F,
        col = viridis(n = 5, direction = 1),
        title = "Correlación WineQT")

gbm.fit <- gbm(quality ~ ., data = df.Wine.Train, distribution = "gaussian", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, verbose = T)


summary(gbm.fit)

predictions <- predict(gbm.fit, newd = df.Wine.Train, distribution = "gaussian", n.trees = 1000)


