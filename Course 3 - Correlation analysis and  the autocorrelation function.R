# Referencias: DataCamp.com
# Course: Introduction to time series analysis
# Lesson 3: Correlation analysis and the autocorrelation function

# Scatterplots ......

# Lets consider two hypothetically stocks A and B.
# You can use ts.plot(cbind(stock_A, stock_B)) to make a ts plot 
# You can also looked at the behavior of stock prices paired by time
# To do so, you make a scatterplot plot(stock_A, stock_B)
# You can detect if there is a positive relationship between the pair
# Instead of stock prices, you can also compare the log returns
# You first need to calc the log return

# Plot EuStockMarkets
plot(EuStockMarkets)

# Use this code to convert daily prices to daily returns
returns <- EuStockMarkets[-1,] / EuStockMarkets[-1860,] - 1

# Convert returns to a ts object
returns <- ts(returns, start = c(1991, 130), frequency = 260)

# Plot daily returns
plot(returns)


# Use this code to convert prices to log returns
logreturns <- diff(log(EuStockMarkets))

# Plot daily logreturns
plot(logreturns)
# Daily net returns and daily log returns are two valuable metrics 
# for financial data.

# We transform returns into percentage returns
eu_percentreturns <- returns * 100

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)

# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)

# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)

# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, 
      main = "", xlab = "Percentage Return")
# Note that the vast majority of returns are near zero, but some
# daily returns are greater than 5 percentage points in magnitude.



# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)
# Note the clear departure from normality, especially in the tails
# of the distributions, as evident in the normal quantile plots.


# We get DAX and FTSE from EuStockMArkets
DAX <- EuStockMarkets[,"DAX"]
FTSE <- EuStockMarkets [,"FTSE"]


# Make a scatterplot of DAX and FTSE
plot(DAX,FTSE)

# Make a scatterplot matrix of eu_stocks
pairs(EuStockMarkets)

# Make a scatterplot matrix of logreturns
pairs(logreturns)

# As you can see, pairs() is a useful way to check for 
# relaionships between the indices.
# DAX = Germany, SMI = Switzerland, CAC = France, FTSE = UK.


# Lets clean up the log returns for DAX and FTSE
DAX_logreturns <- logreturns [ , "DAX"]
FTSE_logreturns <- logreturns [ , "FTSE"]

# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)

# Use cov() with logreturns
cov(logreturns)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)

# Use cor() with logreturns
cor(logreturns)

# These commands provide a simple and intuitive output for 
# comparing the relationships between your indices, especially
# when a scatterplot matrix is difficult to interpret.

# Let´s create a vector
y <- c(2.06554379,  1.29963803,  0.03357800, -0.34258065,  0.23256126,  0.46812008,
       4.34111562,  2.82007636,  2.90799984,  2.33495061,  1.15989954, 0.82008659,
       -0.2433892, -0.03355907, -1.53548216, -0.69363797, -1.41731648, -0.76623179,
       0.83536060,  0.04395345,  1.07447506,  1.50200360, -0.21238609,  0.32996521,
       -0.75033470, -0.10522038,  0.20471918 ,-0.17170595,  0.87181378,  1.47213721,
       0.84112591,  0.96430157,  0.66829027, -0.25752691,  0.08193916, -1.46057000,
       -1.26726830, -2.19329186, -2.21008902,  0.42338945, -1.01513893, -1.54446229,
       -0.72524036,  0.70352378, -0.36108456, -0.77422092, -0.50023603,  1.31369378,
       1.15621723,  0.68782375, -0.79475183,  0.32563325,  2.00955556,  1.70614293,
       0.99910640,  0.68932712,  0.65764259,  1.51403467,  0.85806413,  1.96951273,
       2.98268339,  3.01781322,  1.30009671,  0.71140225,  0.40782908, -0.53429804,
       -0.21147251,  1.72814428, -0.75541665, -1.34178777, -1.72317007, -2.78147841,
       -1.72572507, -3.49466071, -2.41789449, -0.13744248, -0.15805310, -0.27865357,
       -0.97493500, -1.52666608, -1.04093146, -1.26059748, -1.44067012, -1.23902633,
       -0.44668174,  1.12562870 , 3.25518488,  1.13570549,  0.98992411,  0.38244269,
       2.71124649,  2.42216865,  1.78509981, -1.03092109, -1.06607323, -2.63465306,
       -2.66808169, -1.30411399, -1.04269885,  0.40215260, -0.48928251, -0.49381470,
       -1.08457733, -0.27456945, -1.84390881, -2.09907629, -1.88923578, -1.84534263,
       -0.33812159, -1.20911695, -0.50157701, -0.58298734,-1.66575871, -1.41327839,
       -2.55380296, -0.86895290, -2.16915012, -2.60202618, -2.05678159, -0.87654110,
       1.32919650,  1.07620974, -0.96432698, -1.81480027, -2.05757608, -2.34353353,
       -0.01467163,  0.77321454,  0.03106214,  1.16999559,  2.67732293,  4.57761736,
       4.90582958,  4.13300371,  4.04398099, 1.35081333,  0.61429043,  1.42969023,
       0.79231154, 1.34178061,  2.22016551,  2.82502290,  2.43279283,  1.89023418,
       0.46877402, -1.30680558, -1.45910588,  0.21169330,  1.10203354, 1.42360646)
x <- ts (y, start=1, end=150, frequency = 1)


# Define x_t0 as x[-1]
x_t0 <- x [-1]

# Define x_t1 as x[-n]
n <- length(x)
x_t1 <- x[-n]

# Confirm that x_t0 and x_t1 are (x[t], x[t-1]) pairs  
head(cbind(y, x_t0, x_t1))
tail(cbind(y, x_t0, x_t1))

# Plot x_t0 and x_t1
plot(x_t0, x_t1)

# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)

# Use acf with x
acf(x, lag.max = 1, plot = FALSE)

# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n

# Autocorrelations or lagged correlations are used to assess 
# whether a time series is dependent on its past. 
# For a time series x of length n we consider the n-1 pairs
# of observations one time unit apart.
# The first such pair is (x[2],x[1]), and the next is (x[3],x[2]).
# The lag-1 autocorrelation of x can be estimated as the sample 
# correlation of these (x[t], x[t-1]) pairs.
# Finally, note that the two estimates differ slightly as they use 
# slightly different scalings in their calculation of sample 
# covariance, 1/(n-1) versus 1/n. 
# Although the latter would provide a biased estimate, it is preferred
# in time series analysis, and the resulting autocorrelation estimates
# only differ by a factor of (n-1)/n.
# The acf() command is a helpful shortcut for calculating autocorrel.

# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)

# Type the ACF estimate at lag-10 
0.100

# Type the ACF estimate at lag-5
0.198

# View the ACF of x
acf(x)

# Plotting the estimated ACF of x shows large positive correlations 
# for several lags which quickly decay towards zero.

# ---------End of lesson 3 Correlation analysis and the ACF----