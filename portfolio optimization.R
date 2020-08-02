# Primera Sesión:
# Profesor: Siri, Julián Ricardo
# Programa: QUANt
# Módulo:   Optimización de Portafolios
# Fecha:    02-10-2019

#sp500 comps usar edgar sec bajar datos de fundamentals

# Paquetes a descargar ----------------------------------------------------
paquetes <- c("tidyverse","lubridate",
              "highcharter","tidyquant","tibbletime","quantmod","quadprog","Quandl",
              "PerformanceAnalytics","scales","riingo","plotly")

# Vamos a chequear primero si los paquetes están instalados y luego los cargamos
installed.packages()[,'Package'] # lista todos los paquetes instalados

#library(paquete específico) forma tradicional de cargar paquetes



# ahora vamos a aplicar una función que chequee si está instalado y luego los instala
paquetes[1]
(paquetes[1] %in% installed.packages()[,'Package'])

# entonces
check.installed <- function(package){
  new.package <- package[!(package %in% installed.packages()[,'Package'])]
  if(length(new.package)) install.packages(new.package,dependencies = TRUE)
  sapply(package, library,character.only = TRUE)
}

check.installed(paquetes)


# mi token 5f34e957ea5d67525f4dba05f65f6e8e3178fa24
# siri token c8874d9164df9b867e92331d8c1df44f3c865a11

# Tiingo Token
TOKEN <- "5f34e957ea5d67525f4dba05f65f6e8e3178fa24"
# riingo_set_token(TOKEN)
# riingo_get_token()


# trasury con duration menor a un año, etf quasi risk free SHV

# Descarga de instrumentos financieros ------------------------------------
# symbols <- c("SPY","EFA", "SHV", "USO","AGG")

symbols <- c("TSLA","GE", "NKE", "X","JPM")
e = new.env()

# Download de precios
getSymbols.tiingo(Symbols = symbols,
                            src = "tiingo",
                            api.key = TOKEN,
                            from = "2013-01-01",
                            to = "2019-06-30",env=e, auto.assign = TRUE,
                            adjust = TRUE)



precios <- do.call(merge, eapply(e,Cl))
head(precios)
colnames(precios) <- c("JPM", "X", "TSLA", "NKE", "GE")
head(precios)

e

class(precios)
precios.mensuales <- to.monthly(x = precios,indexAt = "lastof",OHLC = FALSE)
head(precios.mensuales)

retornos_mensuales <- CalculateReturns(precios.mensuales, method = "log")
head(retornos_mensuales)
retornos_mensuales <- na.omit(CalculateReturns(precios.mensuales,method = "log"))
head(retornos_mensuales)


# visualizando los retornos 
highchart(type = "stock") %>%
  hc_title(text = "Retornos mensuales") %>%
  hc_add_series(retornos_mensuales[, symbols[1]]*100,
                name = symbols[1]) %>%
  hc_add_series(retornos_mensuales[, symbols[2]]*100,
                name = symbols[2]) %>%
  hc_add_series(retornos_mensuales[, symbols[3]]*100,
                name = symbols[3]) %>%
  hc_add_series(retornos_mensuales[, symbols[4]]*100,
                name = symbols[4]) %>%
  hc_add_series(retornos_mensuales[, symbols[5]]*100,
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d}
                            {point.y:.4f}%') %>%
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}%"))




hc_hist <- hist(retornos_mensuales[, symbols[1]],
                breaks = 50,
                plot = FALSE)
hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Log Returns Distribution",
                   sep = " ")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)


# Construcción de métricas ------------------------------------------------

#### Importante....uso retornos logaritmicos o aritmecos?
#Utilizar la funci???n colMeans para obtener el mismo resultado
Vector_Medias <- colMeans(retornos_mensuales)
#O con la funci???n apply.
Vector_Medias <- apply(retornos_mensuales,2,mean) #Sus inputs son: la matriz con los retornos, indicar si trabajar por fila (1)
# o columnas(2) y finalmente la funci???n a aplicar (en este caso mean, para obtener la media).
#Calculo el desv???o, elemento a elemento (la heróica).
Vector_sd <- c(sd(retornos_mensuales$JPM),
               sd(retornos_mensuales$X))
           
names(Vector_sd) <- symbols

#O puedo tamb???en hacer uso de la funci???n apply, cambiando mean (antes usado) por sd (funci???n de R para calcular el desv???o).
Vector_sd <- apply(retornos_mensuales,2,sd)
Media.Varianza <- data.frame( Activo = names(Vector_Medias),
                              Media = as.numeric(Vector_Medias),
                              Desv = as.numeric(Vector_sd))
#Corr matrix
cor_matrix <- cor(retornos_mensuales)
#Covariance matrix
Covarianza_matrix <- (Vector_sd%*%t(Vector_sd)) * cor_matrix
#O bien, llego al mismo resultado utilizando la funci???n de R
Covarianza_matrix <- cov(retornos_mensuales)

plot_ly(data = Media.Varianza, x = ~Desv, y = ~Media, color = ~Activo)

#Grafico los activos en el espacio mean-variance
plot(x = Vector_sd, y = Vector_Medias, 
     xlim = c(0,max(Vector_sd)*1.25), 
     ylim = c(min(Vector_Medias)*1.25,max(Vector_Medias)*1.25), pch = 20, lwd = 3,
     col = "royalblue4",xlab = expression(sigma[p]),ylab = expression(mu[p]))
text(x = Vector_sd,y = Vector_Medias,labels = symbols,pos = 4, cex = 0.65)

data.descriptivos <- data.frame(ACtivos = names(Vector_Medias),
                                Media = Vector_Medias,
                                Volatilidad = Vector_sd)

# Construcción de portafolio Equally-Weighted (EW) ------------------------

#Generemos un portafolio equally-weighted
vector_x <- rep(1/length(Vector_Medias),length(Vector_Medias)) #Vector de ponderaciones
names(vector_x) <- symbols #Asigno nombres de cada componente del portafolio
sum(vector_x) # Controlo que la suma de las ponderaciones sea igual a 1

#Calcular retorno esperado del portafolio
mu.portfolio <- vector_x%*%Vector_Medias # Es posible calcularlo de la siguiente forma : crossprod(vector_x,Vector_Medias) 
sig2.portfolio <- t(vector_x)%*%Covarianza_matrix%*%vector_x
sig.portfolio <- sqrt(t(vector_x)%*%Covarianza_matrix%*%vector_x)

#Agrego el portafolio al plot de media-varianza
points(x = sig.portfolio,y = mu.portfolio, pch = 20, lwd = 3, col = "black" )
text(x = sig.portfolio,y = mu.portfolio,labels = "EW",pos = 2, cex = 0.65)



# Construcción del portafolio de Mínima Varianza Global (MVG) -------------

#Encontrar el portafolio de m???nima varianza global. 
#NOTA: Recordar que para esto debemos minimizar la varianza del portafolio sujeta
#a la condici???n de que la sumatoria de las ponderaciones sea igual a 1.
#Esto lo obtenemos formando un lagrangiano, calculando las FOCs. 
#Para el ejemplo hasta aqu??? tratado, tendremos 4 ecuaciones lineales y 4 incognitas :
#la ponderaci???n de cada activo (5 en nuestro ejemplo) y la varianza del portafolio.
#En R lo abordaremos el problema matricialmente.
top.mat <- cbind(Covarianza_matrix,rep(1,length(Vector_Medias)))
bot.mat <- c(rep(1,length(Vector_Medias)),0)
Am.mat <- rbind(top.mat,bot.mat)
b.vec <- c(rep(0,length(Vector_Medias)),1)
z.m.mat <- solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:length(Vector_Medias),1]
sum(m.vec) # Controlar que la sumatoria de las ponderaciones sumen 1 para asegurarnos que cumplimos la restricci???n.

#Con las ponderaciones del portafolio de m???nima varianza global, pasamos a calcular la media y varianza del mismo 
#(recordar que lo hicimos con un portafolio Equally-Wighted)
Media_minP <- m.vec%*%Vector_Medias
Var_minP <- t(m.vec)%*%Covarianza_matrix%*%m.vec
Sd_minP <- sqrt(Var_minP)

#Lo agregamos al gr???fico.
points(x = Sd_minP,y = Media_minP, pch = 20, lwd = 6, col = "green" )
text(x = Sd_minP,y = Media_minP,labels = "MVG",pos = 2, cex = 0.75)



# Construcción de la frontera eficiente -----------------------------------

#Primero encontremos un portafolio eficiente con los mismos retornos esperados que una de las empresas bajo an???lisis,
#por ejemplo, United States Oil ETF (USO). (La idea es lograr los mismos retornos esperados con menor volatilidad).
top.mat <- cbind(Covarianza_matrix,Vector_Medias,rep(1,length(Vector_Medias)))
mid.vec = c(Vector_Medias,0,0)
bot.vec = c(rep(1, length(Vector_Medias)), 0, 0)
A.mat = rbind(top.mat, mid.vec, bot.vec)
USO.vec = c(rep(0, length(Vector_Medias)), Vector_Medias["USO"], 1)
#Calculamos el vector de ponderaciones. (Recordar la forma matricial de esto y traducirlo a lenguaje R)
z.mat = solve(A.mat)%*%USO.vec
x.vec = z.mat[1:length(Vector_Medias),]
sum(x.vec) #Controlamos que la suma sea igual a 1.

Media_P_USO <- Vector_Medias%*%x.vec
Sd_P_USO <- sqrt(t(x.vec)%*%Covarianza_matrix%*%x.vec)

#Lo agregamos al gr???fico.
points(x = Sd_P_USO,y = Media_P_USO, pch = 20, lwd = 6, col = "red" )
text(x = Sd_P_USO,y = Media_P_USO,labels = "Port-eff",pos = 2, cex = 0.55)

# Construir un loop para guardar toda una frontera eficiente.

alpha.vec = seq(1,-1,by = -0.05) # seq() es una secuencia, que en este caso, comienza en 1, termina en cero, y va cayendo 
# de a 0.01 por paso

# Preasigno las matrices para las ponderaciones y para los resultados (retorno esperado y varianza del portafolio)
ponderaciones = matrix(rep(0,length(Vector_Medias)*length(alpha.vec)),
                       ncol= length(alpha.vec),
                       nrow=length(Vector_Medias))
metricas = matrix(rep(0,length(alpha.vec)*2), ncol = length(alpha.vec), nrow = 2)

for(i in 1:length(alpha.vec)){
  
  ponderaciones[,i] = alpha.vec[i]*m.vec + (1-alpha.vec[i])*x.vec
  
  # Caminos alternativos
  #1era forma
  metricas[1,i] = ponderaciones[,i]%*%Vector_Medias
  
  #Varianza del portafolio
  metricas[2,i] = sqrt((t(ponderaciones[,i])%*%Covarianza_matrix)%*%ponderaciones[,i])
}

points(x = metricas[2,],y = metricas[1,], pch = 20, lwd = 1, col = "green" )

#Notar que la frontera eficiente solo va desde el portafolio de m???nima varianza global hasta el portafolio eficiente
#que hemos calculado anteriormente (uno equivalente a USO en t???rminos de retornos, con menor volatilidad.)



# Utilizando el paquete quadprog ------------------------------------------

#Comenzamos a utilizar solve.QP, para resolver problemas de optimizaci???n.
#Comenzamos calculando nuevamente el portafolio MVG pero bajo esta nueva metodolog???a.
#Llamar la librar???a (instalarla en su defecto)
Dmat = Covarianza_matrix
dvec = array(data = 0,dim=c(length(Vector_Medias),1))
Amat = array(data = 1,dim=c(length(Vector_Medias),1))
bvec = 1

pond.mvg = solve.QP(Dmat,dvec, Amat, bvec,meq= 1)
pond.mvg = pond.mvg$solution #Comparar con los resultados antes logrados

mu.mvg = t(Vector_Medias)%*%pond.mvg
sd.mvg = sqrt(t(pond.mvg)%*%Covarianza_matrix%*%pond.mvg)
plot(x = sd.mvg,y = mu.mvg, main = "Media-Varianza",pch = 20, lwd = 1,
     col = "royalblue4", ylim = c(-0.05,0.10),xlim = c(0,0.25),
     xlab = expression(sigma[p]),ylab = expression(mu[p]))
text(x = sd.mvg,y = mu.mvg,labels = "MVG",pos = 2, cex = 0.65)

#Buscamos la frontera eficiente con solve.qp
covariance <- Covarianza_matrix
n <- ncol(covariance)
# Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
Amat <- matrix(1, nrow=n)
dvec = array(data = 0,dim=c(length(Vector_Medias),1))
bvec <- 1
meq <- 1
# Cuantas corridas quiero 
risk.premium.up <- 0.15
risk.increment <- 0.001
loops <- risk.premium.up / risk.increment + 1
counter <- 0
#Genero elemento para conservr la frontera
Frontera.eff <- matrix(nrow=loops, ncol=n+2)
# Nombre a los elementos del la matriz del portafolio
colnames(Frontera.eff) <- c(colnames(retornos_mensuales), "Volatilidad", "Retornos.Esperados")
#Corro la iteraci???n
for (i in seq(from=mu.mvg, to=risk.premium.up, by=risk.increment)){
  counter <- counter + 1
  Ret <- mu.mvg + i
  bvec.Eff <- c(bvec,Ret)
  Amat.Eff <- cbind(Amat,as.vector(Vector_Medias))
  # Voy moviendo la frontera (variando los retornos)
  sol <- solve.QP(covariance, dvec=dvec, Amat=Amat.Eff, bvec=bvec.Eff, meq=meq)
  Frontera.eff[counter,1:n] <- sol$solution
  
  Frontera.eff[counter,"Volatilidad"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
  Frontera.eff[counter,"Retornos.Esperados"] <- as.numeric(sol$solution %*% colMeans(retornos_mensuales))
}
#Convierto el elemento a data frame (recuerden que de esta forma podemos llamar a los objetos con $)
Frontera.eff <- as.data.frame(Frontera.eff)
points(x = Frontera.eff$Volatilidad,y = Frontera.eff$Retornos.Esperados,
       col = "darkorange",pch = 20,lwd = 0.75)

##Computar el portafolio de m???nima varianza global sin SHORT SELLING. 
Amat <- cbind(1, diag(n))
dvec = array(data = 0,dim=c(length(Vector_Medias),1))
bvec <- 1 #La suma de las ponderaciones debe ser igual a 1
bvec <- c(bvec, rep(0, n)) #Pero adem???s, cada una de ellas debe ser mayor a 0.
#Resolvemos el problema
pond.mvg.NO.SH = solve.QP(Dmat,dvec, Amat,bvec,meq=1)
pond.mvg.NO.SH <- pond.mvg.NO.SH$solution#Obtenemos las ponderaciones
#COmputo estad???sticas descriptivas
mu.mvg.NO.SH = t(Vector_Medias)%*%pond.mvg.NO.SH
sd.mvg.NO.SH = sqrt(t(pond.mvg.NO.SH)%*%Covarianza_matrix%*%pond.mvg.NO.SH)
#Agrego al plot antes generado
points(x =sd.mvg.NO.SH,y = mu.mvg.NO.SH,
       col = "red",pch = 20,lwd = 1)
text(x =sd.mvg.NO.SH,y = mu.mvg.NO.SH,
     label = "MVG.NO.SH",cex = 0.65,pos = 4)


# Agregaos la restricci???n de NO SHORT SELLING
Amat <- cbind(1, diag(n))
dvec = array(data = 0,dim=c(length(Vector_Medias),1))
bvec <- 1
bvec <- c(bvec, rep(0, n))
counter <- 0
# Cuantas corridas quiero 
risk.premium.up <- 0.10
risk.increment <- 0.001
loops <- risk.premium.up / risk.increment + 1
counter <- 0
#Genero elemento para conservr la frontera
Frontera.eff.NS <- matrix(nrow=loops, ncol=n+2)
# Nombre a los elementos del la matriz del portafolio
colnames(Frontera.eff.NS) <- c(colnames(retornos_mensuales), "Volatilidad", "Retornos.Esperados")
#Corro la iteraci???n
for (i in seq(from=mu.mvg.NO.SH, to=risk.premium.up, by=risk.increment)){
  counter <- counter+1
  Ret <- mu.mvg.NO.SH+i
  bvec.Eff <- c(Ret,bvec)
  
  Amat.Eff <- cbind(as.matrix(Vector_Medias),Amat)
  # Voy moviendo la frontera (variando los retornos)
  sol <- solve.QP(covariance, dvec=dvec, Amat=Amat.Eff, bvec=bvec.Eff, meq=meq)
  Frontera.eff.NS[counter,1:n] <- sol$solution
  
  Frontera.eff.NS[counter,"Volatilidad"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
  Frontera.eff.NS[counter,"Retornos.Esperados"] <- as.numeric(sol$solution %*% colMeans(retornos_mensuales))
}

Frontera.eff.NS <- as.data.frame(Frontera.eff.NS)
points(x = Frontera.eff.NS$Volatilidad,y = Frontera.eff.NS$Retornos.Esperados, col = "lightblue",pch = 20,lwd = 0.75)

#Agregamos todos los activos
points(x = Vector_sd,y = Vector_Medias, col = "deepskyblue4",
       pch = 20,lwd = 0.75)
text(x = Vector_sd,y = Vector_Medias, col = "deepskyblue4",
     labels = colnames(Retornos),cex = 0.55,pos = 4)
legend("topleft", legend = c("FE.Short Sales", "FE.NO.Short Sales"),
       bty  = "n",cex = 0.75,lty = c(6,6),lwd = c(3,3),
       col = c("darkorange","lightblue"),y.intersp = 0.2,inset=-0.05)


