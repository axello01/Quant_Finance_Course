call_europea_BIN <- function(S, K, r, T, sigma, pasos) {

  
#Parametros internos
R <- exp(r*(T/pasos))          
Rinv <- 1.0/R                   
u <- exp(sigma*sqrt(T/pasos))
uu <- u*u
d <- 1.0/u

#Probabilidad de riesgo neutral
q <- (R-d)/(u-d)

#Precios finales
precios <- numeric(pasos+1)

precios[1] <- S*(d^pasos) 
for (i in seq(2,pasos+1,1)) {
  precios[i] <- uu*precios[i-1]
} 

#Lo doy vuelta (ver mejor metodo)  
#preciostemp <- precios
#for (i in seq(1,pasos+1,1)) {
#  precios[i] <- preciostemp[pasos+1-i+1]
#} 

precios <- rev(precios)

#Matriz de Calls
call_values <- matrix(0,pasos+1,pasos+1) 

#Payoff
for (i in seq(1,pasos+1,1)) {
call_values[i,pasos+1] <- max(0, (precios[i]-K))
}

#Recurrencia
for (step in seq(pasos,1,-1)){ 
    for (i in seq(1,step,1)) {
        call_values[i,step] <- ( q * call_values[i,step+1] + (1.0-q) * call_values[i+1,step+1] )*Rinv
        
    }
  
}

price_BIN <- call_values[1,1]

return(price_BIN)

}