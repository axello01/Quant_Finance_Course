#install.packages("openxlsx")
library(openxlsx)
library(xts)
library(PerformanceAnalytics)
library(dplyr)




precios <- read.xlsx("C:/Users/Axel/Desktop/Quant/24.10.19/archivosclase5/Datos.xlsx",sheet ="Precios",detectDates = TRUE,na.strings = "NULL")
precios <- precios[-1,]
precios <- as.xts(precios[,c(2:ncol(precios))],order.by = precios[,1])

# EPS <- read.xlsx("C:/Users/Usuario/Desktop/UCEMA/QUANt/Datos.xlsx",sheet ="EPS",detectDates = TRUE,na.strings = "NULL")
# EPS <- as.xts(EPS[,c(2:ncol(EPS))], order.by = EPS[,1])
# 
# D_EV <- read.xlsx("C:/Users/Usuario/Desktop/UCEMA/QUANt/Datos.xlsx",sheet ="D to EV",detectDates = TRUE,na.strings = "NULL")
# D_EV <- as.xts(D_EV[,c(2:ncol(EPS))], order.by = D_EV[,1])

Sectors <- read.xlsx("C:/Users/Axel/Desktop/Quant/24.10.19/archivosclase5/Datos.xlsx",sheet ="Clasificacion",detectDates = TRUE,na.strings = "NULL")



# computos auxiliares
retornos <- CalculateReturns(precios,method = "discrete")

# Trabajando con los portafolios ------------------------------------------

# computos auxiliares
retornos <- CalculateReturns(precios,method = "discrete")

n.assets <- ncol(precios)

# Definimos un portafolio ¿Benchmark? El Equally-Weighted
eq_weights <- rep(1/n.assets,n.assets)

# calculamos sus retornos -caso "buy and hold"-
port.EW.BH <- Return.portfolio(retornos, weights = eq_weights)
head(port.EW.BH)

# Mismo portafolio, pero rebalanceamos mensualmente 
port.EW.Rebal <- Return.portfolio(retornos, weights = eq_weights, rebalance_on = "months")
head(port.EW.Rebal)

# Graficamos
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(port.EW.BH)
plot.zoo(port.EW.Rebal)

# Otros análisis útiles
dev.off()
par()
charts.RollingPerformance(merge(port.EW.BH,port.EW.Rebal),main = "B&H 'puro' vs. B&H con Rebalanceo")
# alternativamente
charts.RollingPerformance(port.EW.Rebal-port.EW.BH,main = "Excess del B&H con Reabalanceo")
chart.RelativePerformance(Ra = port.EW.Rebal,Rb = port.EW.BH)


# Herramientas asociadas a Return.portfolio
# Construcción de la Equity Curve
EC.EW.Rebal <- Return.portfolio(retornos, weights = eq_weights, rebalance_on = "months",wealth.index = TRUE)
plot.xts(EC.EW.Rebal)
charts.PerformanceSummary(EC.EW.Rebal) #ojo, funciona sobre los retornos
charts.PerformanceSummary(port.EW.Rebal)

# Tablas relevantes para un análisis rápido
table.Stats(port.EW.Rebal)
table.ProbOutPerformance(R = port.EW.Rebal,Rb = port.EW.BH,period_lengths = c(1,3,6,12))
table.CalendarReturns(R = port.EW.Rebal,digits = 3,as.perc = TRUE)
t(table.CalendarReturns(R = port.EW.Rebal,digits = 3,as.perc = TRUE))
t(table.CalendarReturns(R = merge(port.EW.Rebal,port.EW.BH), digits = 3, as.perc = TRUE))
table.Drawdowns(port.EW.Rebal,top = 10,digits = 3) # Drawdowns (desde, mínimo, hasta, tamaño, duración)
table.DownsideRisk(port.EW.Rebal)
chart.Drawdown(port.EW.Rebal)
table.AnnualizedReturns(R = merge(port.EW.Rebal,port.EW.BH),digits = 3)
table.AnnualizedReturns(R = merge(port.EW.Rebal,port.EW.BH),digits = 3,scale = 12)


# Agregarle profundidad
class(port.EW.Rebal)
port.EW.Rebal <- Return.portfolio(retornos, weights = eq_weights, rebalance_on = "months",verbose = TRUE)
class(port.EW.Rebal)
port.EW.BH <- Return.portfolio(retornos, weights = eq_weights, verbose = TRUE)

# Analicemos la diferencia de pesos
AMZN.w.BH <- port.EW.BH$EOP.Weight[,"AMZN"]
AMZN.w.Rebal <- port.EW.Rebal$EOP.Weight[,"AMZN"]
head(merge(AMZN.w.BH,AMZN.w.Rebal)) # No nos dice mucho, pero veamoslo gráficamente

par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
plot.zoo(AMZN.w.BH)
plot.zoo(AMZN.w.Rebal)
dev.off()

# más métricas
ret_anual = Return.annualized(R = port.EW.Rebal)
vol_anual = StdDev.annualized(port.EW.Rebal)
sharpe_anual = SharpeRatio.annualized(R = port.EW.Rebal,Rf = 0.005) # la risk free -Rf- puede ser tanto una constante como un vector
# además cuidado si se pasa como constante, dado que tiene la misma frecuencia que la serie de tiempo que utilizamos para los retornos


# Rolling data
chart.RollingPerformance(R = port.EW.Rebal,FUN = "Return.annualized",width = 12)
charts.RollingPerformance(R = port.EW.Rebal,width = 12,Rf = 0.005)
chart.RollingCorrelation(Ra = port.EW.Rebal,Rb = port.EW.BH,width = 12)



# Analisis de portafolios -------------------------------------------------
# install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)

# armamos las especificaciones de un portafolio
port.specs <- portfolio.spec(assets = Sectors$Ticker,category_labels = Sectors$Sector)

#veamos la estructura del objeto creado
str(port.specs)

# sobre este objeto es que vamos a ir armando el resto
port.specs <- add.constraint(portfolio = port.specs, type = "full_investment")
# alternativamente add.constraints(portfolio = port.specs, type = "weight_sum",min_sum = 1, max_sum = 1)
str(port.specs)

# las restricciones no se tienen que contradecir
port.specs <- add.constraint(portfolio = port.specs, type = "long_only")
port.specs <- add.constraint(portfolio = port.specs, type = "box", min = 0, max = 0.1)
# aqui podemos pasarlo como un valor o como un vector -como máximo activo por activo-

# también operan restricciones a nivel grupo -por eso es útil tener los sectores-
port.specs <- add.constraint(portfolio=port.specs,
                        type="group",
                        groups=list(port.specs$category_labels$`Health Care`, 
                                    port.specs$category_labels$Utilities),
                        group_min=c(0.12, 0.15),
                        group_max=c(0.25, 0.3),
                        group_labels= list(port.specs$category_labels$`Health Care`,port.specs$category_labels$Utilities))
print(port.specs)

# o restricciones a nivel turnover
port.specs <- add.constraint(portfolio = port.specs, type = "turnover", turnover_target = 0.1)

str(port.specs)


# finalmente podemos optimizar, pasando funciones objetivos
port.specs <- add.objective(portfolio = port.specs, type = "risk", name = "var")
port.specs <- add.objective(portfolio = port.specs, type = "return", name = "mean")

print(port.specs)


frontera <- create.EfficientFrontier(R = retornos,portfolio = port.specs,type = "mean-sd",n.portfolios = 25)

install.packages("ROI")
install.packages("DEoptim")
install.packages("pso")
install.packages("GenSA")
install.packages("doParallel")
library(ROI)
library(DEoptim)
library(pso)
library(doParallel)

optimizacion <- optimize.portfolio(R = retornos,portfolio = port.specs)
port.spec <- add.constraint(portfolio = port.specs, type = "full_investment",min_sum = 0.99, max_sum = 1.01)
optimizacion <- optimize.portfolio(R = retornos,portfolio = port.specs)



# Una optimización menos ambiciosa

specs <- portfolio.spec(assets = stocks.selection$symbol,
                        category_labels = stocks.selection$sector)
specs <- add.constraint(portfolio=specs,
                        type="full_investment")
specs <- add.constraint(portfolio=specs,
                    type="box",
                    min = -0.5,
                    max = +0.5)
# to create the efficient frontier 
specs <- add.objective(portfolio=specs, 
                       type="return",
                       name="mean") # mean
specs <- add.objective(portfolio=specs, 
                       type="risk",
                       name="var") # uses sd
frontera <- create.EfficientFrontier(
  R = retornos,
  portfolio = specs,
  type = "mean-sd",
  n.portfolios = 25)
summary(frontera, digits=2) 
# meansd.ef$frontier[1:2,] # shows the first two portfolios
chart.EfficientFrontier(frontera,
                        match.col="StdDev", # which column to use for risk
                        type="l", 
                        RAR.text="Sharpe Ratio",
                        rf=0.04/12,
                        tangent.line = TRUE,
                        chart.assets=TRUE,
                        labels.assets=TRUE,xlim=c(0.03,0.20),ylim=c(0,0.055))