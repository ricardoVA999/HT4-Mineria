setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT4-Mineria")
library(ModelMetrics)
library(corrplot)
library(nortest)

houses = read.csv('train.csv')
houses[is.na(houses)]<-0

corCuantHouses1 <- houses[,c(4,5,20,21,27,35,37,38,39,44,45,81)]
corCuantHouses2 <- houses[,c(46,47,48,49,50,51,52,53,55,57,81)]
corCuantHouses3 <- houses[,c(60,62,63,67,68,69,70,71,72,76,77,78,81)]

#Se separan las variables cuantitativas para mejor visualizacion
M1<-cor(corCuantHouses1)
corrplot.mixed(M1, upper = "square")
#Podemos observar que hay variables que tienen mucha correlacion entre ellas aparte de tener correlacion con salePrice
#Para evitar overfitting de las variables que tienen mucha correlaicion entre si solo dejaremos la que tenga mayor correlacion
#con saleprice
corCuantHouses1$YearRemodAdd<-NULL
corCuantHouses1$X1stFlrSF<-NULL

#Realizaremos el mismo procedidmiento para los demas grupos de variables cuantitativas, para evitar el overfitting

M2<-cor(corCuantHouses2)
corrplot.mixed(M2, upper = "square")
corCuantHouses2$TotRmsAbvGrd<-NULL
corCuantHouses2$FullBath<-NULL

M3<-cor(corCuantHouses3)
corrplot.mixed(M3, upper = "square")
corCuantHouses3$GarageArea<-NULL
corCuantHouses3$GarageYrBlt<-NULL

#Empezamos a juntar los grupos de las variables cuantitativas y ver si entre estos no hay variables muy correlacionadas
#En el caso de haber variables correlacionadas se hace el mismo proceso que antes.
corCuantHouses1$SalePrice<-NULL
corCuant12<-cbind(corCuantHouses1, corCuantHouses2)
M12<-cor(corCuant12)
corrplot.mixed(M12, upper = "square")
corCuant12$X2ndFlrSF<-NULL
corCuant12$BsmtFullBath<-NULL

corCuant12$SalePrice<-NULL
cuantHouses<-cbind(corCuant12,corCuantHouses3)
M<-cor(cuantHouses)
corrplot.mixed(M, upper = "square")
cuantHouses$TotalBsmtSF<-NULL
cuantHouses$GarageCars<-NULL
cuantHouses$GrLivArea<-NULL

#Conjuntos de entrenamiento y prueba
porciento <- 70/100
set.seed(1234)
trainRowsNumber<-sample(1:nrow(houses),porciento*nrow(houses))

training<-cuantHouses[trainRowsNumber,]
test<-cuantHouses[-trainRowsNumber,]

#Regresion Lineal Multiple prediciendo el precio de las casas
modeloreg<-lm(SalePrice~., data = training)
summary(modeloreg)

pred <- predict(modeloreg, newdata = test)
test$prediccion <- round(pred,0)
plot(test$SalePrice, test$prediccion)
plot(modeloreg)


#Errores
rmse(test$SalePrice, test$prediccion)
hist(test$prediccion, freq = F)
lines(density(test$prediccion))
lillie.test(test$prediccion)

#Prediciendo el tipo de casa
cuantHouses$clasification <- ifelse(houses$SalePrice > 290000, 3, ifelse(houses$SalePrice>170000, 2, 1))
porciento <- 70/100
set.seed(1234)
trainRowsNumber<-sample(1:nrow(houses),porciento*nrow(houses))
training<-cuantHouses[trainRowsNumber,]
test<-cuantHouses[-trainRowsNumber,]
fitLM<-lm(clasification~., data = training)
summary(fitLM)

pred <- predict(fitLM, newdata = test)
test$prediccion <- round(pred,0)
plot(test$SalePrice, test$prediccion)

rmse(test$clasification, test$prediccion)
mean(test$prediccion-test$clasification)

cfmCaret <- confusionMatrix(table(test$prediccion,test$clasification))
cfmCaret

table(test$clasification)
table(test$prediccion)

