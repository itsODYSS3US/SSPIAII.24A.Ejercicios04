#Librerias

# libs <- c("ggplot","caTools","mltools","data.table","cowplot")

# if(!require(libs))
# {install.packages(libs, dependencies = T)}

list.of.packages <- c("ggplot2","caTools","mltools","data.table","cowplot","caret","rpart","rpart.plot","randomForest","gbm","corrplot","viridis")

new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)

#Habilitar
# library(ggplot2)
# library(caTools)
# library(mltools)
# library(data.table)
# library(cowplot)

