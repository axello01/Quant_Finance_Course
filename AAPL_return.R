.libPaths("C:/Users/Axel/Desktop/escritorio_axel/Diplo Data Science/Library")

C:/Users/Axel/Desktop/quant/Clase 2

help.start()

getSymbols(Symbols = "AAPL", from = "2010-01-01")
print(AAPL)
View(AAPL)
class(AAPL)
str(AAPL)

AAPL.CierreAdj = Ad(AAPL)

#cl es close, etc. Ad es adjusted
# notar que la fecha no es una columna

dim(AAPL)
dim(AAPL.CierreAdj)
index(AAPL)
dates = index(AAPL.CierreAdj)
#es un vector. Entonces no usas dim, usas length

length(dates)
#xts significa serie de tiempo
#adjusted es precio adjustado por dividendos
#la funcion index te trae el indice

#ls() ves los objetos generados hasta el momento
#rm() remove los objetos
#rm(list = ls()) borras todos los objetos

ls()

plot(AAPL.CierreAdj)

#con kpm no trabajas con precios, trabajas con retornos

#para modelar un activo necesitas que los drivers esten iid


#retorno discreto, dif de precios. Se usa para generar un portafolio. Retorno continuo: log(p1/po) Son aditivos en terminos de serie de tiempos, los otros cross sectional

#exp(rc)-1 para el retorno discreto

library(TTR)
#por default te tira el retorno continuo
AAPL.CierreAdj.Ret <- ROC(x = AAPL.CierreAdj,
                          type = "discrete")
#PERDES UNA FILA, UN grado de libertad

plot(AAPL.CierreAdj.Ret)
#leverage y clustering indican que en los picos hay volatilidad
#alt 91 y 93 corchetes
AAPL ["2010::2011"]

hist(AAPL.CierreAdj.Ret["2010::2012"], breaks = 1000)
hist(AAPL.CierreAdj.Ret["2013::2015"], breaks = 1000)
#los precios estan correlationados con el del dia anterior, los retornos no.

symbols <- c("ORCL", "AAPL", "CVX")
head(symbols)
getSymbols(Symbols = symbols, from = "2010-01-01")

FN_suma <- function(x , y){
  print(x + y)
}
FN_suma(x = 10 , y = 5)

elemento_f <- merge(AAPL.CierreAdj, AAPL.CierreAdj.Ret)
get(symbols)
#do.call mergea las series de tiempo. 
#lapply aplica todo a una lista. x adopta cada uno de los simbolos, ad get extrae de 
#adentro el valor ajustado

activos <- do.call(what = merge , args = lapply(symbols, function(x) {Ad(get(x))}))
head(activos)
plot(activos)

activos.ret <- ROC(activos, type = "discrete")

activos.ret <- activos.ret[-1, ]
plot(activos.ret)

plot(cumprod(1 + activos.ret ))
getSymbols("SPY", from = "2010-01-01")
SPY.CierreAdj.Ret <- ROC(Ad(SPY) , type = "discrete") [-1]
activos.ret <- merge(activos.ret, SPY.CierreAdj.Ret)
plot(cumprod(1 + activos.ret) , legend.loc = 1)

#retorno medio
vector_media <- colMeans(activos.ret)*252
#MARGIN 1 ES FILA 2 ES COLUMNA
vector_sd <- apply(X = activos.ret, MARGIN = 2 , FUN = sd )*sqrt(252)
#anualizas con 252, en el desvio raiz cuadrada obvio

plot(x = vector_sd , y = vector_media)
options(scipen = 999) #te permite no ver los numeros en notacion cientifica
matrix.cov <- cov(activos.ret)
(4*(4-1))/2  #sacas los elementos de la diag ppal, (varianzas), para buscas las covarianzas
matrix.cor <- cor(activos.ret)

scale(activos.ret)
#calcula la media y el desvio y normaliza. restas media, dividis por desvio. t es la traspuesta

(t(scale(activos.ret))%*%scale(activos.ret))/(nrow(activos.ret)-1)

#solve te invierte la matriz
round(solve(matrix.cor)%*%matrix.cor, 3)
pc.dec <- eigen(matrix.cr) #vectores y valores propios
pc.dec <- eigen(matrix.cor)
pc.dec$values
det(matrix.cor)
