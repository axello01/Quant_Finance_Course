
library(quantmod)
Strat.Symbols <- c("SPY" , "IEF")
getSymbols(Strat.Symbols, from = "2010-01-01")

Prices <- do.call(what = merge, args = lapply(Strat.Symbols, function(x) Ad(get(x)) ))
Prices_IS = Prices["2010::2013"]
Prices_OOS = Prices["2014::"]

Prices_strat <- Prices_IS
SMA_Short <- seq(5, 100, 3)
SMA_Long <- SMA_Short + 10
Strat.Performance = xts()
SMA_Long
SMA_Short
length(SMA_Long)
length(SMA_Short)

for(I in 1:length(SMA_Long)){
  print(I)
  Prices_strat$SMA_Long = SMA(Prices_strat$SPY.Adjusted, n = SMA_Long[I])
  Prices_strat$SMA_Short = SMA(Prices_strat$SPY.Adjusted, n = SMA_Short[I])
  Prices_tmp = Prices_strat[which(!is.na(Prices_strat$SMA_Long)), ]
  strat.returns = NULL
  count = 0
  for(i in 1:nrow(Prices_tmp)){
  if (Prices_tmp$SMA_Short[i] > Prices_tmp$SMA_Long[i]){
    strat.returns[i] = 1
  } else {
    strat.returns[i] = 0
    
  }
    }  
  
w_strat <- as.xts(cbind(strat.returns, (1 - strat.returns)), order.by = 
                    index(Prices_tmp))
w_strat <- lag(w_strat)[-1, ]  

Prices.R <- ROC(Prices_tmp[ ,1:2], tyoe = "discrete") [-1, ]

dim(Prices_strat)
class(Prices_strat)
