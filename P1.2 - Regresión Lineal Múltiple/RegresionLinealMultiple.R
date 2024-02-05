# Gómez Plascencia Waldo.
# Vidrio Lizaola Alfonso Manuel. 
# Becerra Ramírez Alejandro.
# Ascencio Padilla Isaac Ulises. 
# Aceves Díaz César Alejandro.

source("LibrariesPreprocess.R")

#Carga del csv que contiene el conjunto de datos.
df.Startups <- read.csv("50_Startups.csv")
set.seed(12345)

#Variables dummies. 
df.Startups$State <- factor(df.Startups$State, levels = c("New York","California","Florida"),
                          labels = c(1,2,3))

#Calculo de medidas de tendencia central. 
summary(df.Startups)

#Diagrama de caja del conjunto de datos. 
boxplot(df.Startups)


#Separación de entrenamiento y prueba
Split <- sample.split(Y = df.Startups$Profit, SplitRatio = 0.3)
df.Startups.Train <- subset(df.Startups, Split == T)
df.Startups.Test <- subset(df.Startups, Split == F)


#Regresión lineal multiple  lm()
mdl.Regresor <- lm(formula = Profit ~ Marketing.Spend + Administration + R.D.Spend, data = df.Startups.Train)
summary(mdl.Regresor)
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

summary(mdl.Regresor)


#Gráfica
plt.Profit <- ggplot() + 
  theme_light() +
  ggtitle("Regresión: Profit vs. Spends") +
  xlab("Gastos de investigacion y desarrollo") + 
  ylab("Profit")

#Profit ~ R.D.Spend

plt.Profit.Data <- plt.Profit + 
  geom_point(aes(x = df.Startups.Train$R.D.Spend,
                 y = df.Startups.Train$Profit), 
             colour = "blue2") +
  geom_line(aes(x = df.Startups.Train$R.D.Spend,
                y = predict(mdl.Regresor, newdata = df.Startups.Train)),
            colour = "red3")


#Muestra la gráfica.
plt.Profit.Data

#Almacena los objetos a continuación para poder crear el gráfico en Rmarkdown.
save(plt.Profit.Data, df.Startups, df.Startups.Train, df.Startups.Test, mdl.Regresor,
     file = "Grafica")
