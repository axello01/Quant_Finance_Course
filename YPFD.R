#### Se importan las librerias necesarias, para correr un Perceptron Multicapa y conectarse a un 
#### repositorio de valores en tiempo real para diferentes acciones:



rm(list = ls())

library(neuralnet)
library(quantmod)

#################################################################################################################
### Defino todos los valores parametrizables, para no tener que modificar luego el cuerpo principal##############
#################################################################################################################
start_day <- 250 ##### Defino una variable que luego utilizare como rango de tiempo.
start_date = as.Date(Sys.Date()- start_day)#### Fecha de Inicio. En este ejemplo seran 100 dias hacia atras tomando el dia de hoy.
end_date = (Sys.Date()) ####################### Fecha Final, en este ejemplo, el dia de hoy.
acc <- as.character("YPFD.BA") ################## Quote de la accion segun Yahoo. En este ejemplo APPLE.
nn <- 5 ###################### Numero de nueronas en la capa oculta, en este ejemplo, 5
stmax <- 1000000 ### Variable para el parametro step_max. Maxima cantidad de iteraciones
############### Fin de valores parametrizables ##################################################################
#################################################################################################################

## Obtengo los valores de la accion que seleccione en la variable "acc" en el paso anterior.
## Y nombro sus variables.

	symb <- getSymbols(acc, src = "yahoo", from = start_date,auto.assign = FALSE,header=FALSE)
	names(symb)[1]='Open'
	names(symb)[2]='High'
	names(symb)[3]='Low'
	names(symb)[4]='Close'
	names(symb)[5]='Volume'
	names(symb)[6]='Adjusted'

## De todas las variables anteriores, solo seleccion Volume y Adjusted
	data <- subset(symb,select=c(Volume, Adjusted))

## Calculo diferentes indicadores, sobre la variable Precio de Ajuste: "Adjusted" y los redondeo a dos decimales.
## Aqui podrian seleccionar tantos indicadores como deseen y mejores resultados obtengan.

	rsi_2 <- signif(RSI(data$Adjusted,2),2)
	rsi_7 <- signif(RSI(data$Adjusted,7),2)
	rsi_14 <- signif(RSI(data$Adjusted,14),2)
	rsi_21 <- signif(RSI(data$Adjusted,21),2)

## Uno con el comando cbind para formar el dataset con la variales que seleccione en "data" y los nuevos indicadores.


	data <- cbind(data,rsi_2,rsi_7,rsi_14,rsi_21)

## Hago un lag del dataset, creando una nueva variable con los precios "laggeados"

	lag_close_price=lag(data$Adjusted, - 1)

## Con cbind, ahora uno el dataset anterior "data" con la nueva columna de precios "laggeados" y le pongo nombre a cada variable.

	data_lag <- cbind(data,lag_close_price)

	names(data_lag)[3]='RSI_2'
	names(data_lag)[4]='RSI_7'
	names(data_lag)[5]='RSI_14'
	names(data_lag)[6]='RSI_21'
	names(data_lag)[7]='Price_Lagged'

## Resto el precio "laggeado" con el valor original de ajuste, para determinar si ese dia las acciones SUBIERON o BAJARON
## Para ello, se crea una nueva variable "data_diff" justamente con la diferencia.

	data_diff <- (data_lag$Price_Lagged - data_lag$Adjusted)

## Con este loop FOR, voy a proponer que si la diferencia es NEGATIVA, entoces las acciones BAJARON, y si es POSITIVA, SUBIERON

	Signal <- 0

	data_lag <- cbind(data_lag,Signal)
	names(data_lag)[8]='Signal'

			for(i in 1:(nrow(data_lag) - 1)) {
				if(data_diff[i] <= 0) {
					data_lag$Signal[i] <- 'BAJA'
					} else {
					data_lag$Signal[i] <- 'SUBE'
					}
					}

## Ahora voy a crear los sets de testeo y entrenamiento
## Como el indicador RSI_21 toma los primeros 21 valores para realizar calculos, entre los registros 1 y 21, van a aparecer varios "NA"
## Para evitar estos casos, el training lo voy a formar desde el registro 23 hasta el ultimo menos 1. El ultimo lo reservo para el testing.
## Las variables Adjustes y Price_Lagged las quito del set de datos, y solo me quedo con los indicadores que he creado. 
## Igualmente se puede trabajar con tantas variables como ustedes deseen. En el set de training, tambien se quita la variable "Signal", ya que esa es 
## la variable que quiero predecir, o sea, si la accion va a SUBIR o va a BAJAR.

	train_data <- subset((data_lag[23:nrow(data_lag)-1]), select=c(-Adjusted, -Price_Lagged))
	test_data <- subset((data_lag[nrow(data_lag)]), select=c(-Adjusted, -Price_Lagged, -Signal))

	nnet_train <- train_data

	nnet_train <- cbind(nnet_train, train_data$Signal == 'SUBE')
	nnet_train <- cbind(nnet_train, train_data$Signal == 'BAJA')

	names(nnet_train)[7] <- 'SUBE'
	names(nnet_train)[8] <- 'BAJA'

#### Se crea el modelo!! ###################################

	neural <- neuralnet(SUBE+BAJA ~ RSI_2+RSI_7+RSI_14+RSI_21,data=nnet_train,hidden=nn,stepmax=stmax)
	plot(neural)



	testing <- subset(test_data,select=c(-Volume))
	mypredict <- compute(neural, testing)$net.result

	print(mypredict)
