source("Lib.Preprocess.R")

df.Wine <- read.csv(file = "Datasets/WineQT.csv", header  = T, stringsAsFactors = F)

df.Wine$Id <-NULL



boxplot(df.Wine$quality)
boxplot(df.Wine$fixed.acidity)
boxplot(df.Wine$volatile.acidity)
boxplot(df.Wine$citric.acid)
boxplot(df.Wine$residual.sugar)
boxplot(df.Wine$chlorides)
boxplot(df.Wine$free.sulfur.dioxide)
boxplot(df.Wine$total.sulfur.dioxide)
boxplot(df.Wine$density)
boxplot(df.Wine$pH)
boxplot(df.Wine$sulphates)
boxplot(df.Wine$alcohol)

boxplot(df.Wine)
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


#Modelo
gbm.fit <- gbm(quality ~ ., data = df.Wine.Train, distribution = "gaussian", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, verbose = T)


summary(gbm.fit)
mdl.Predict <- predict(object = mdl.Regresor, newdata = df.Startups.Test)


repeat {
    # Realiza eliminación hacia adelante
    Auxiliar <- step(mdl.Regresor, direction = "forward")
    
    # Realiza eliminación hacia atrás
    Auxiliar <- step(Auxiliar, direction = "backward")
    save(Auxiliar, file = "Objetosrmarkdown")
    # Comprueba si se han agregado o eliminado variables
    if (identical(Auxiliar, mdl.Regresor)) {
        break  # Si no se han realizado cambios, termina el bucle
    } else {
        mdl.Regresor <- Auxiliar  # Actualiza el modelo con las nuevas características
    }
}

