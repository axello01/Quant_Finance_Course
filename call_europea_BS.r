call_europea_BS <- function(S, K, r, T, sigma){
# -------------------------------------------------------------------------
# Summary: To calculate vanilla option price by B/S
# Example: Price = OptionPrice_BS(100, 100, 0.05, 0.01, 0.25, 1, 1)
#          Price = 11.7193
# -------------------------------------------------------------------------

d1 <- (log(S/K) + (r + 0.5 * sigma * sigma) * T) / sigma / sqrt(T)
d2 <- (log(S/K) + (r - 0.5 * sigma * sigma) * T) / sigma / sqrt(T)
price_BS <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
	
return(price_BS)
}