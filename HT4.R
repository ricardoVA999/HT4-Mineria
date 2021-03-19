setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT4-Mineria")
library(ModelMetrics)

houses = read.csv('train.csv')
houses[is.na(houses)]<-0

colnames(houses)

houses$clasification <- ifelse(houses$SalePrice > 290000, 1, ifelse(houses$SalePrice>170000, 2, 3))

corCuantHouses <- houses[,c(4,5,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81,82)]


#1. Caras 2. Intermedia 3. Economias
houses$clasification <- ifelse(houses$SalePrice > 290000, 1, ifelse(houses$SalePrice>170000, 2, 3))

#Conjuntos de entrenamiento y prueba
porciento <- 70/100
set.seed(5432)
trainRowsNumber<-sample(1:nrow(houses),porciento*nrow(houses))

training<-houses[trainRowsNumber,]
test<-houses[-trainRowsNumber,]
table(training$clasification)
table(test$clasification)

#Regresion Lineal Multiple
fitLM<-lm(clasification~., data = training)
summary(fitLM)
