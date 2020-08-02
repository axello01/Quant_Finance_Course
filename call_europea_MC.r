call_europea_MC <- function(S,K,r,T,sigma,caminos){

z <- rnorm(caminos,0,1)  
call <- numeric(caminos)
for (i in seq(1,caminos,1)){  
  call[i] <- exp(-r * T) * max( 0 , S * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T)  * z[i]) - K);
  }

price_MC = mean(call)
var= var(call)

#[price_MC, var, CI] = normfit(payoff_desc);
#[price_MC, var, CI] 


return(price_MC)
}