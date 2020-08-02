rm(list=ls())
suppressMessages(library(quantmod))
suppressMessages(library(RJSONIO))

####
#btc_last <- market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker')
url <- "https://www.bitstamp.net/api/transactions/"
btc_data <- fromJSON(url) # returns a list
btc_last <- do.call(rbind,lapply(btc_data,data.frame,stringsAsFactors=FALSE))[1,]$price
head(btc_last)

write.table(btc_last,file="C:/BITCOIN/data/btc_last.txt",sep=",", row.names=F, col.names=F, append=T)
####
btc_hist <- read.csv("C:/BITCOIN/data/btc_last.txt",sep=",", header=F)
names(btc_hist) <- c("Last")
last_price_btc <- tail(btc_hist$Last,1)

rsi_14_btc <- round(RSI(btc_hist$Last,14),1) #el , 1 es por 1 decimal hasta wirte table tuviste que escribirlo 14 vees para tenerlo
last_rsi_14_btc <- tail(rsi_14_btc,1)

##### DEFINO CAPITAL INICIAL
actual_capital <- tail(read.csv("C:/BITCOIN/data/actual_capital.txt",sep=",",header=F),1)
total_btc <- tail(read.csv("C:/BITCOIN/data/total_btc.txt",sep=",",header=F),1)
###############################

##################################
########## ACA COMIENZA EL ALGORITMO PROPIAMENTE DICHO
#######################################################

  
if((last_rsi_14_btc < 20) & (actual_capital > (0.1*last_price_btc))) {
  op_btc <- paste(Sys.time(),"BUY 0.1 BTC at:", last_price_btc)	
  write.table(op_btc,file="C:/BITCOIN/data/btc_trades.txt",sep=",", row.names=F, col.names=F, append=T)
  actual_capital <- actual_capital - (0.1 * last_price_btc)
  total_btc <- total_btc + 0.1
  write.table(actual_capital,file="C:/BITCOIN/data/actual_capital.txt",sep=",", row.names=F, col.names=F, append=T)
  write.table(total_btc,file="C:/BITCOIN/data/total_btc.txt",sep=",", row.names=F, col.names=F, append=T)
  
  print("BUYING 0.1 BTC")
  
} else {
  
  if((last_rsi_14_btc > 80) & (total_btc >= 0.1)) {
    
    op_btc <- paste(Sys.time(),"SELL 0.1 BTC at:", last_price_btc)	
    #ENVIO AL MERCADO op_btc necesitas que se llamen iguales q la anterior pq son mutuamente excluyentes
    #RECIBO CONFIRMACION
    write.table(op_btc,file="C:/BITCOIN/data/btc_trades.txt",sep=",", row.names=F, col.names=F, append=T)
    actual_capital <- actual_capital + (0.1 * last_price_btc)
    total_btc <- (total_btc - 0.1)
    
    write.table(actual_capital,file="C:/BITCOIN/data/actual_capital.txt",sep=",", row.names=F, col.names=F, append=T)
    write.table(total_btc,file="C:/BITCOIN/data/total_btc.txt",sep=",", row.names=F, col.names=F, append=T)
    
    print("SELLING 0.1 BTC")
    
  } else {
    print("DO NOTHING !!! ")
  }
}

paste("Estamos Trabajando!!!",Sys.time())
paste("Actual Capital: ",actual_capital)
paste("BTC Amount: ", total_btc)
paste("Last RSI: ", last_rsi_14_btc)
paste("INIT CAPITAL:  77050")
paste("TOTAL CAPITAL:",actual_capital+(total_btc*last_price_btc))