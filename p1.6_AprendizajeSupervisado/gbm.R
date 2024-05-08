source("Lib.Preprocess.R")
set.seed(1991)
options(scipen = 999)
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

#density
#citric.acid
#total.sulfur.dioxide
gbm.fit <- gbm(quality ~ ., data = df.Wine.Train, distribution = "gaussian", n.trees = 500, interaction.depth = 4, shrinkage = 0.01, cv.folds = 5, verbose = T)
summary(gbm.fit)

gbm.fit2 <- gbm(quality ~ alcohol + volatile.acidity + sulphates, data = df.Wine.Train, distribution = "gaussian", n.trees = 500, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, verbose = T)
summary(gbm.fit2)

gbm.fit3 <- gbm(quality ~ density + citric.acid + total.sulfur.dioxide, data = df.Wine.Train, distribution = "gaussian", n.trees = 500, interaction.depth = 3, shrinkage = 0.01, cv.folds = 5, verbose = T)
summary(gbm.fit3)

mdl.Predict <- predict(object = gbm.fit, newdata = df.Wine.Test)
summary(mdl.Predict)
mdl.Predict2 <- predict(object = gbm.fit2, newdata = df.Wine.Test)
mdl.Predict3 <- predict(object = gbm.fit3, newdata = df.Wine.Test)


Y.pred <- ifelse(mdl.Predict >= 1.5 & mdl.Predict <= 2.5, 2,
                 ifelse(mdl.Predict > 2.5 & mdl.Predict <= 3.5, 3,
                        ifelse(mdl.Predict > 3.5 & mdl.Predict <= 4.5, 4,
                               ifelse(mdl.Predict > 4.5 & mdl.Predict <= 5.5, 5,
                                      ifelse(mdl.Predict > 5.5 & mdl.Predict <= 6.5, 6,
                                             ifelse(mdl.Predict > 6.5 & mdl.Predict <= 7.5, 7,
                                                    ifelse(mdl.Predict > 7.5 & mdl.Predict <= 8.5, 8, NA)))))))

Y.pred2 <- ifelse(mdl.Predict2 >= 1.5 & mdl.Predict2 <= 2.5, 2,
                 ifelse(mdl.Predict2 > 2.5 & mdl.Predict2 <= 3.5, 3,
                        ifelse(mdl.Predict2 > 3.5 & mdl.Predict2 <= 4.5, 4,
                               ifelse(mdl.Predict2 > 4.5 & mdl.Predict2 <= 5.5, 5,
                                      ifelse(mdl.Predict2 > 5.5 & mdl.Predict2 <= 6.5, 6,
                                             ifelse(mdl.Predict2 > 6.5 & mdl.Predict2 <= 7.5, 7,
                                                    ifelse(mdl.Predict2 > 7.5 & mdl.Predict2 <= 8.5, 8, NA)))))))

Y.pred3 <- ifelse(mdl.Predict3 >= 1.5 & mdl.Predict3 <= 2.5, 2,
                 ifelse(mdl.Predict3 > 2.5 & mdl.Predict3 <= 3.5, 3,
                        ifelse(mdl.Predict3 > 3.5 & mdl.Predict3 <= 4.5, 4,
                               ifelse(mdl.Predict3 > 4.5 & mdl.Predict3 <= 5.5, 5,
                                      ifelse(mdl.Predict3 > 5.5 & mdl.Predict3 <= 6.5, 6,
                                             ifelse(mdl.Predict3 > 6.5 & mdl.Predict3 <= 7.5, 7,
                                                    ifelse(mdl.Predict3 > 7.5 & mdl.Predict3 <= 8.5, 8, NA)))))))



matriz <- table(df.Wine.Test$quality, Y.pred)
matriz

matriz2 <- table(df.Wine.Test$quality, Y.pred2)
matriz2

matriz3 <- table(df.Wine.Test$quality, Y.pred3)
matriz3


# Crear un data frame con los valores reales y predichos para cada modelo
df_compare <- data.frame(
  Quality_Real = df.Wine.Test$quality,
  Quality_Predicted_Model1 = mdl.Predict,
  Quality_Predicted_Model2 = mdl.Predict2,
  Quality_Predicted_Model3 = mdl.Predict3
)

# Gráfico de dispersión para el primer modelo
p1 <- ggplot(df_compare, aes(x = Quality_Real, y = Quality_Predicted_Model1)) +
  geom_point(color = "darkblue") +
  geom_abline(intercept = 0, slope = 1, color = "darkred") +
  labs(title = "Modelo 1", x = "Calidad Real", y = "Calidad Predicha")

# Gráfico de dispersión para el segundo modelo
p2 <- ggplot(df_compare, aes(x = Quality_Real, y = Quality_Predicted_Model2)) +
  geom_point(color = "#008b15") +
  geom_abline(intercept = 0, slope = 1, color = "darkred") +
  labs(title = "Modelo 2", x = "Calidad Real", y = "Calidad Predicha")

# Gráfico de dispersión para el tercer modelo
p3 <- ggplot(df_compare, aes(x = Quality_Real, y = Quality_Predicted_Model3)) +
  geom_point(color = "#c800ff") +
  geom_abline(intercept = 0, slope = 1, color = "darkred") +
  labs(title = "Modelo 3", x = "Calidad Real", y = "Calidad Predicha")

# Mostrar los gráficos
p1
p2
p3

library(gridExtra)

# Combinar los tres gráficos en una cuadrícula
grid.arrange(p1, p2, p3, ncol = 3)

#Evaluar el rendimiento de los modelos
confusionMatrix(as.factor(Y.pred), as.factor(df.Wine.Test$quality), mode = "everything")
confusionMatrix(as.factor(Y.pred2), as.factor(df.Wine.Test$quality), mode = "everything")
confusionMatrix(as.factor(Y.pred3), as.factor(df.Wine.Test$quality), mode = "everything")


