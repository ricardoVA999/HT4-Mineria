#Ricardo Valenzuela 18762
#Amado Garcia 181469
#Sara Zavala 18893

library(ggplot2)
library(cluster)
library(e1071)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)

setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT3-Mineria")
houses = read.csv('train.csv')
houses[is.na(houses)]<-0

nrow(houses)
ncol(houses)

#Relacion del Area del terreno y el precio de venta
plot(main='Correlacion Area - Precio de Venta', xlab='Precio de Venta', ylab='Area del terreno', x=houses$SalePrice, y=houses$LotArea, col='blue')

#Cantidad de casas que se vendieron por tipo de casa
mshouses <- table(houses$MSSubClass)
barplot(main='Cantidad de Casas Vendidas por Tipo de casa', mshouses, col='violet')

#Zona de las casas con el precio de venta
ggplot(houses, aes(x = MSZoning, y = SalePrice)) + geom_jitter()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#Vecindarios y  el precio de venta
ggplot(houses, aes(x = Neighborhood, y = SalePrice)) + geom_point()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#Cantidad de casas que se vendieron por la condicion de la misma
overallCond <- table(houses$OverallCond)
barplot(main='Cantidad de Casas Vendidas condicion de la casa', overallCond, col='Yellow')

#Relacion entre la condicion de las casas y su precio de venta
lab <- c('Very Poor', 'Poor', 'Fair', 'Below Average', 'Average', 'Above Average', 'Good', 'Very Good', 'Excelent', 'Very Excelent')
p <- ggplot(houses, aes(x = OverallCond, y = SalePrice)) + geom_point()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
p + scale_x_discrete(limit=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'), labels= lab)

#Variables cuantitativas de interes
cuanti_data_houses<-cbind(houses$LotFrontage, houses$LotArea, houses$YearBuilt, houses$YearRemodAdd, houses$BsmtUnfSF, houses$TotalBsmtSF, houses$X1stFlrSF, houses$X2ndFlrSF, houses$TotRmsAbvGrd, houses$GarageCars, houses$GarageArea, houses$PoolArea, houses$MoSold, houses$YrSold, houses$SalePrice)

#Grafica de codo para determinar cuantos clusters se realizaran
wss <- (nrow(cuanti_data_houses-1)*sum(apply(cuanti_data_houses,2,var)))
for (i in 2:10) 
  wss[i] <- sum(kmeans(cuanti_data_houses, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Creacion de grupos
km<-kmeans(cuanti_data_houses,3,iter.max =100)
houses$grupokm<-km$cluster
fviz_cluster(km, data = cuanti_data_houses,geom = "point", ellipse.type = "norm")

#Calidad del agrupamiento
silkm<-silhouette(km$cluster,dist(cuanti_data_houses))
mean(silkm[,3])

#Analisis de los grupos
g1KM<-houses[houses$grupokm==1,]
g2KM<-houses[houses$grupokm==2,]
g3KM<-houses[houses$grupokm==3,]

#Grupo 1
barplot(main='Cantidad de Casas Vendidas por Tipo de casa en el Grupo 1', table(g1KM$MSSubClass), col='violet')
barplot(main='Cantidad de Casas Vendidas por zona en el Grupo 1', table(g1KM$MSZoning), col='violet')
barplot(main='Cantidad de Casas Vendidas por vecindario en el Grupo 1', table(g1KM$Neighborhood), col='violet')
barplot(main='Cantidad de Casas Vendidas por condicion en el Grupo 1', table(g1KM$OverallCond), col='violet')
summary(g1KM)

#Grupo 2
barplot(main='Cantidad de Casas Vendidas por Tipo de casa en el Grupo 2', table(g2KM$MSSubClass), col='green')
barplot(main='Cantidad de Casas Vendidas por zona en el Grupo 2', table(g2KM$MSZoning), col='green')
barplot(main='Cantidad de Casas Vendidas por vecindario en el Grupo 2', table(g2KM$Neighborhood), col='green')
barplot(main='Cantidad de Casas Vendidas por condicion en el Grupo 2', table(g2KM$OverallCond), col='green')
summary(g2KM)

#Grupo 3
barplot(main='Cantidad de Casas Vendidas por Tipo de casa en el Grupo 3', table(g3KM$MSSubClass), col='skyblue')
barplot(main='Cantidad de Casas Vendidas por zona en el Grupo 3', table(g3KM$MSZoning), col='skyblue')
barplot(main='Cantidad de Casas Vendidas por vecindario en el Grupo 3', table(g3KM$Neighborhood), col='skyblue')
barplot(main='Cantidad de Casas Vendidas por condicion en el Grupo 3', table(g3KM$OverallCond), col='skyblue')
summary(g3KM)

#Variable clasificadora de casas
houses$clasification <- ifelse(houses$SalePrice > 290000, "Caras", ifelse(houses$SalePrice>170000, "Intemedia", "Economicas"))
table(houses$clasification)

#Creacion de grupos
porciento <- 70/100
set.seed(1234)

houses$Condition2 <- NULL
houses$Exterior1st <- NULL
houses$RoofStyle <- NULL
houses$ExterCond <- NULL
houses$RoofMatl <- NULL
houses$Electrical <- NULL
houses$Heating <- NULL
houses$HeatingQC <- NULL
houses$MiscFeature <- NULL
houses$grupokm <- NULL
houses$Exterior2nd <- NULL
houses$SaleType <- NULL
houses$SalePrice <- NULL


trainRowsNumber<-sample(1:nrow(houses),porciento*nrow(houses))
training<-houses[trainRowsNumber,]
test<-houses[-trainRowsNumber,]

table(training$clasification)
table(test$clasification)

#Arbol de regresion
arbolModelo<-rpart(clasification~.,houses,method = "class")
rpart.plot(arbolModelo)


#Arbol de clasificacion
dt_model<-rpart(clasification~.,training,method = "class")
rpart.plot(dt_model)

prediccion <- predict(dt_model, newdata = test[1:69])

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta

cfm<-confusionMatrix(table(test$prediccion, test$clasification))
cfm

#Random forest
ct<-trainControl(method = "cv",training[,1:70],number=10, verboseIter=T)
modelorf<-train(clasification~.,data=training,method="rf",trControl = ct)
prediccionrfVC<-predict(modelorf,newdata = test[,1:70])
test$predrfVC<-prediccionrfVC

cfmCaret <- confusionMatrix(table(test$predrfVC,test$clasification))
cfmCaret

table(test$clasification)
table(test$prediccion)
table(test$predrfVC)
