library(quantmod)
Strat.Symbols = c("SPY", "IEF")
getSymbols(Strat.Symbols, from = "2010-01-01")

Prices <- do.call(what = merge, args = lapply(Strat.Symbols, function(x) Ad(get(x)) ))
Prices_IS = Prices["2010::2013"]
Prices_OOS = Prices["2014::"]
Prices_strat = Prices_OOS
SMA_Short = seq(5, 100, 3) 
SMA_Long = SMA_Short + 10
Strat.Performance = xts()

for(I in 1:32){
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
  W_strat = lag(as.xts(cbind(strat.returns, 1 - strat.returns), order.by = index(Prices_tmp)))
  Prices.R = ROC(Prices_tmp[ ,1:2], type = "discrete")[-1, ]
  Strat.Performance_tmp = as.xts(rowSums(Prices.R*W_strat[-1, ]), order.by = index(Prices.R))
  Strat.Performance = merge(Strat.Performance, Strat.Performance_tmp)
}

stratStats(Strat.Performance)

colnames(Strat.Performance_tmp) = "Estrategia Ganadora"
stratStats(cbind(Strat.Performance_tmp, Prices.R$SPY.Adjusted))


plot(cumprod(1 + Strat.Performance_tmp))
lines(cumprod(1 + Prices.R$SPY.Adjusted))
