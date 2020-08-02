 install.packages("edgar")
 install.packages("edgarWebR")
 install.packages("openxlsx")
 install.packages("MASS")

getwd()
.libPaths("C:/Users/Axel/Desktop/Axel 2019/Diplo Data Science/Library")

.libPaths()

"C:/Users/Axel/Desktop/Quant/7.10.19.siri"


library(edgar)
library(edgarWebR)
library(openxlsx)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(MASS)
# Cargando los componentes del S&P 500 y sus CIK
Componentes <- read.xlsx("C:/Users/Axel/Desktop/Quant/7.10.19.siri/emisores.xlsx")

head(Componentes)

TOKEN <- "5f34e957ea5d67525f4dba05f65f6e8e3178fa24"

#TOKEN <- "c8874d9164df9b867e92331d8c1df44f3c865a11"

#TOKEN <- "c8874d9164df9b867e92331d8c1df44f3c865a11"


symbols <- c("SPY","FB", "GE")
e = new.env()

head(symbols)
e

# Single Factor Model: CAPM -----------------------------------------------
# Download de precios
getSymbols.tiingo(Symbols = symbols,
                  api.key = TOKEN,
                  from = "2010-01-01",
                  to = "2019-06-30",env=e,adjust = TRUE)

precios <- do.call(merge, eapply(e,Cl))
precios.mensuales <- to.monthly(x = precios,indexAt = "lastof",OHLC = FALSE)

retornos.mensuales <- na.omit(CalculateReturns(precios.mensuales, method = "log"))
colnames(retornos.mensuales) <- symbols


#Calculamos la beta
CAPM.GE <- lm(retornos.mensuales[,"GE"] ~ retornos.mensuales[,"SPY"])
summary(CAPM.GE)
Beta.GE <- CAPM.GE$coefficients[2]

CAPM.FB <- lm(retornos.mensuales[,"FB"] ~ retornos.mensuales[,"SPY"])
Beta.FB <- CAPM.FB$coefficients[2]

# Descomposición del riesgo: sistemático e idiosincrático
# Varianza total: riesgo sistemático + riesgo idiosincrático
var.GE <- var(retornos.mensuales[,"GE"])

var.FB <- var(retornos.mensuales[,"FB"])


# Riesgo sistemático: Beta^2 * var(Ret.Mercado)
var.SPY <- var(retornos.mensuales[,"SPY"])
riesgo.sis.GE <- (Beta.GE^2)*var.SPY
porc.sis.GE <- riesgo.sis.GE/var.GE

var.SPY <- var(retornos.mensuales[,"SPY"])
riesgo.sis.FB <- (Beta.GE^2)*var.SPY
porc.sis.FB <- riesgo.sis.FB/var.FB


# Riesgo idiosincrático: Var(error) -Residual standard error-
riesgo.id.GE <- var(CAPM.GE$residuals)
porc.id.GE <- riesgo.id.GE/var.GE

riesgo.id.FB <- var(CAPM.FB$residuals)
porc.id.FB <- riesgo.id.FB/var.FB

riesgo.id.FB
porc.id.FB


# Características del portafolio
# Beta del portafolio es la suma ponderada de las betas de los activos
# asumimos un vector de ponderaciones
pond <- c(0.7,0.3)
beta.port <- t(pond) %*% c(Beta.GE,Beta.FB)
riesgo.id.port <- sqrt(12)*sqrt(t(pond^2)%*% c(var(CAPM.GE$residuals),var(CAPM.FB$residuals)))
# implica que los retornos especificos están incorrelacionados, ¿será así?
# Debería darnos lo mismo que generar un vector sintético de retornos
# 70% GE + 30% FB y correr regresión contra mercado.

# Vamos a comprobarlo
ret.port <- Return.portfolio(retornos.mensuales[,c("GE","FB")],weights = pond)
CAPM.port <- lm(ret.port ~ retornos.mensuales[,"SPY"])
Beta.reg.port <- CAPM.port$coefficients[2]
riesgo.id.port.reg <- sqrt(12)*sd(CAPM.port$residuals)

Riesgo <- c(riesgo.id.port,riesgo.id.port.reg)
names(Riesgo) <- c("Ponderado (incorr)", "Estimado Reg")

print(Riesgo)


# Multifactor Models: Fama-French -----------------------------------------
# url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
# temporal <- tempfile()
# download.file(url, temporal,quiet = TRUE)
# prueba <- unz(temporal, "F-F_Research_Data_Factors.CSV")
rm(temporal, prueba, url)
FF_Factors <- read.csv(file = "C:/Users/Axel/Desktop/Quant/7.10.19.siri/F-F_Research_Data_Factors.CSV",
                       header = TRUE, na.strings = "NULL",stringsAsFactors = FALSE,
                       colClasses = c("character",rep("numeric",4)))

head(FF_Factors)
tail(FF_Factors)

library(tidyverse)

fechas <- parse_date(FF_Factors$Dates,format = "%Y%m")
fechas <- fechas + months(1)
tail(fechas)                          
fechas <- rollback(fechas, roll_to_first = FALSE)                          
tail(fechas)

FF_Factors$Dates <- fechas
#filter()

FF_Factors <- FF_Factors[FF_Factors$Dates>= index(retornos.mensuales)[1] &
                           FF_Factors$Dates <= index(retornos.mensuales)[nrow(retornos.mensuales)],]

View(FF_Factors)

# Algunas transformaciones
FF_Factors$Mkt.RF <- FF_Factors$Mkt.RF/100
FF_Factors$SMB <- FF_Factors$SMB/100
FF_Factors$HML <- FF_Factors$HML/100
FF_Factors$RF <- FF_Factors$RF/100


# Exceso de retorno para activos individuales
exc.ret.mens <- retornos.mensuales[,c("GE","FB")] - matrix(rep(FF_Factors$RF,2),ncol = 2)

# Regresion lineal multivariada
x <- exc.ret.mens
y <- as.xts(FF_Factors[,c(-1,-5)],order.by = FF_Factors[,1])

class(y)
class(x)

# armamos una lista para las regresiones
lista.reg = list()
# inicializamos los "holders" de betas, alfas así como R^2 de regresiones
betas = matrix(0, ncol = ncol(x), nrow = ncol(y))
dimnames(betas) = list(colnames(y),colnames(x))
alfas <- rep(0, ncol(x))
var.res = r2.activos <- alfas
names(alfas) = names(var.res) = names(r2.activos) <- names(x)

# loop para estimar todo las regresiones
lista.reg[["GE"]] <- lm(x[,1]~y)
lista.reg[["FB"]] <- lm(x[,2]~y)

alfas["GE"] <- coef(lista.reg[["GE"]])[1]
alfas["FB"] <- coef(lista.reg[["FB"]])[1]

betas[,"GE"] <- coef(lista.reg[["GE"]])[-1]
betas[,"FB"] <- coef(lista.reg[["FB"]])[-1]

cov.risk.fact <- cov(y)

cov.risk.fact

summary(lista.reg[["GE"]])


Componentes[1:15,]
tail(Componentes)

#edicion





