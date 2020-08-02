# Referencias: DataCamp.com
# Course: Introduction to time series analysis
# Lesson 1: Predicting the future


# Trend spotting --------------

# Sometimes ts do not exhibit any clear trend over time 
# Some other times they exhibit a pattern, for example linear
# They could be upward trends or downward trends
# There are also upward trends increasing more rapidly than linear
# Rapid decay is also possible, but not as common.
# Some series may exhibit periodic or sinusoidal trends over time
# Here, the cycle lenghts determines the periodicity of the cycle
# Also you may see increasing variance over time

# There are data transformations that can be used to filter
# the common trends just discussed.

# A rapid growth trend can be linearized with the log()
# This transformation also applies when you see increasing variance
# But remember that log is only defined for positive values

# The diff() is a first difference transformation
# it can remove a linear trend
# The resulting values represent the increment, or changes, between
# consecutive observations in the original series
# Of course, the diff series will have smaller observations than
# the original series

# Finally, for a periodic series, the diff(...,s) is applied
# to calculate a difference transformation.
# It can remove periodic trends.
# It is called the seasonal difference transformation.


# Random or not random?.............
# Some time series exhibit predictability when strong periodic
# or seasonal patterns are present. 
# Other time series exhibit predictability when positive 
# autocorrelation - or correlation among neighboring observations -
# induces what appear as short-term trends.


# Removing trends in variability...........
rapid_growth <- c(505.9547,  447.3556,  542.5831,  516.0634,
  506.9599,  535.0162, 496.9291, 497.5626, 577.2483,  536.8560,  
  541.2459,  473.4978, 550.9890,  569.4106,  522.9152, 487.2002,
  594.6108,  591.1740,  615.9868,  621.3175, 607.1250,  587.0367,
  554.1554,  644.1172, 509.7000,  607.0943,  603.5512,  613.6216,
  544.9143,  670.8118,  687.1316,  615.5817,  711.1873,  694.2979,
  681.9293,  659.1403,  642.7021,  601.5301,  666.7623,  650.9657,
  606.0913,  696.6788, 641.6025,  855.7719,  667.3291, 573.4914,
  791.7333,  751.5914,  610.7948, 624.6503,  833.2990,  639.8867,
  736.8283,  772.2923,  686.8865,  667.7631, 712.9415,  918.1838,
  656.1089,  700.4972,  683.4933,  781.7380,  715.6843, 808.2875,
  820.7795,  656.8856,  733.3400,  773.5387,  641.2027,  932.2119,
  680.6766,  988.2828,  664.8986,  813.5283,  883.4088,  924.2749,
  969.4321,  777.3293,  880.9984,  971.3583,  902.9584, 1020.7457,
  1075.1483,  886.1707, 889.6322,  950.3908,  878.0395, 1043.7676,
  901.1090, 1079.6584,  933.9054,  921.9433,  870.8071,  811.1398,
  1004.2677, 1008.1758, 1189.4893,  751.9706, 947.4753,  886.5153,
  1074.8943, 1101.1307, 1130.1855,  975.8495,  948.1610,
  1177.8227, 1227.1271,  976.9957,  836.7089, 1323.6047,  852.3532,
  1200.8262, 1274.4788, 1349.2614, 1102.6334, 1324.8566, 1268.7187,
  1058.2289, 1204.0872, 1084.6503, 1284.4305, 1195.2843, 1058.4262,
  1188.0577, 1166.5934, 1064.6946, 1429.0685, 1070.8528, 1539.3305,
  1467.1571, 1127.7058, 1296.0717, 1555.2741, 1332.9037, 1315.4236,
  1189.2462, 1482.4339, 1240.9287, 1237.7720, 1468.6083, 1328.5457,
  1589.5078, 1373.1630, 1503.5563, 1659.9376, 1704.6137, 1550.4638,
  1625.8026, 1873.8582, 1370.6209, 1439.7114, 1447.4369, 1579.9158,
  1681.2571, 1661.6059, 1311.8468, 1326.0308, 1323.0995, 1550.4863,
  1606.2042, 1768.5401, 1509.8368, 1592.1086, 1627.6188, 1544.6329,
  1439.5234, 1682.3518, 1850.7097, 1673.3801, 1832.4272, 1672.2672,
  1781.5768, 1659.2899, 1970.0389, 2044.7124, 1929.0902, 1891.7042, 
  1487.1577, 2013.8722, 1796.7886, 1977.0183, 1516.9552,
  1650.6039, 1523.2834, 1696.6181, 1627.2609, 1787.2968, 1567.2874,
  1881.9963, 2318.9833, 1941.9879, 1820.2797, 2154.8123, 2261.5471,
  2052.2214, 2079.1710, 2010.0609, 2145.2606, 1775.3008, 2013.4070)

ts.plot(rapid_growth)

# Log rapid_growth
linear_growth <- log(rapid_growth)

# Plot linear_growth using ts.plot()
ts.plot(linear_growth)
# Notice that the data stabilized by inducing a linear growth
# over time


# Differencing a time series can remove a time trend. 
# The first difference transformation of a time series z[t] 
# consists of the differences (changes) between successive 
# observations over time, that is z[t]???z[t???1].


z <- c(6.226447, 6.103354, 6.296341, 6.246230, 6.228432, 6.282297, 6.208447, 6.209721,
      6.358273, 6.285730, 6.293874, 6.160147, 6.311715, 6.344602, 6.259419, 6.188675,
       6.387907, 6.382110, 6.423225, 6.431842, 6.408735, 6.375087, 6.317445, 6.467881,
       6.233822, 6.408684, 6.402831, 6.419378, 6.300629, 6.508489, 6.532526, 6.422568,
       6.566936, 6.542901, 6.524926, 6.490936, 6.465681, 6.399477, 6.502434, 6.478457,
      6.407031, 6.546325, 6.463969, 6.752004, 6.503283, 6.351743, 6.674225, 6.622193,
      6.414761, 6.437192, 6.725393, 6.461291, 6.602355, 6.649363, 6.532169, 6.503933,
   6.569399, 6.822398, 6.486327, 6.551790, 6.527217, 6.661520, 6.573239, 6.694918,
   6.710255, 6.487510, 6.597609, 6.650976, 6.463346, 6.837560, 6.523087, 6.895969,
    6.499635, 6.701381, 6.783788, 6.829010, 6.876710, 6.655864, 6.781056, 6.878695,
   6.805677, 6.928289, 6.980214, 6.786910, 6.790808, 6.856873, 6.777692, 6.950592,
 6.803626, 6.984400, 6.839375, 6.826484, 6.769420, 6.698440, 6.912014, 6.915898,
7.081279, 6.622697, 6.853801,6.787298, 6.979978, 7.004093, 7.030137,6.883308,
    6.854524, 7.071423, 7.112431, 6.884482, 6.729476, 7.188114, 6.748001, 7.090765,
   7.150293, 7.207313, 7.005457, 7.189060, 7.145763, 6.964352, 7.093477, 6.989013,
    7.158071, 7.086139, 6.964538, 7.080075, 7.061843, 6.970443, 7.264778, 6.976211,
   7.339103, 7.291082, 7.027941, 7.167093, 7.349407, 7.195115, 7.181914, 7.081075,
   7.301441, 7.123615, 7.121068, 7.292071, 7.191840, 7.371180, 7.224872, 7.315588,
    7.414535, 7.441094, 7.346309, 7.393757, 7.535755, 7.223019, 7.272198, 7.277550,
7.365127, 7.427297, 7.415540, 7.179191, 7.189945, 7.187732, 7.346324, 7.381629,
   7.477910, 7.319757, 7.372815, 7.394873, 7.342542, 7.272067, 7.427948, 7.523324,
  7.422601, 7.513397, 7.421936, 7.485254, 7.414145, 7.585809, 7.623012, 7.564804,
   7.545233, 7.304622, 7.607815, 7.493756, 7.589345, 7.324460, 7.408897, 7.328623,
    7.436392, 7.394653, 7.488460, 7.357102, 7.540088, 7.748884, 7.571467, 7.506745,
   7.675459, 7.723804, 7.626678, 7.639725, 
7.605920, 7.671016, 7.481725, 7.607584)

# Generate the first difference of z
dz <- diff(z)

# Plot dz
ts.plot(dz)

# View the length of z and dz, respectively
length(z)
length (dz)
# By removing the long-term time trend, you can view the amount
# of change from one observation to the next.



# For time series exhibiting seasonal trends, seasonal 
# differencing can be applied to remove these periodic patterns. 
# For example, monthly data may exhibit a strong 12 month pattern.
# Then, changes in behavior from year to year may be of more 
# interest than changes from month to month
# The function diff(..., lag = s) will calculate the lag s 
# difference or length s seasonal change series.
# For monthly or quarterly data, 
# an appropriate value of s would be 12 or 4,


x <- c(-4.198033,   9.569009,   5.175143,  -9.691646,  -3.215294,  10.843669,
       6.452159, -10.833559,  -2.235351,  10.119833,   6.579646,  -8.656565,
      -2.515001,   9.837434,   7.386194,  -8.243504,  -4.264033,   8.898861,
       8.544336,  -8.066913, -4.023025,   9.822679,   7.772852,  -6.587777,
     -3.459171,  10.613851,   7.374450,  -5.798715,  -1.204711,  11.429236,
     7.570047,  -4.968384,  -2.003787,  11.941348,   9.406672,  -4.396585,
   -1.555579,  12.599877 ,  8.502916,  -3.728968, -2.827000,  13.375981,
    8.128941,  -3.149249,  -2.799473,  13.710570,   6.755217,  -3.779744,
  -3.768274,  13.625336,   6.537931,  -3.249098,  -5.024191,  13.355373,
   6.931161,  -3.527354,  -5.197329,  11.579791,   7.162449, -1.894607,
   -5.777797,  12.482695,   6.208088,  -3.434038,  -7.080721,  11.413656,
     6.741990,  -3.532376,  -8.393542,  12.507261,   6.473175,  -3.745246,
   -9.426209,  12.380817,   8.048243,  -2.831528,  -7.301893,  12.765838,
       8.223699,  -4.448131,  -6.963558,  12.034005,   7.574925,  -5.402218,
    -6.568198,  10.896482,   7.276571,  -4.037873,  -6.723013,  12.180815,
     8.285162,  -4.159342,  -6.360670,  12.753018,   8.665912,  -5.440538,
  -4.874932,  12.600197,   8.162589,  -6.539572)


# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x, lag =4)

ts.plot(dx)

# View the length of x and dx, respectively 
length(x)
length (dx)


# The White Noise model ------------

# It is the simplest example of a stationary process
# A weak white noise process has a fixed constant mean
# It has a fixed constant variance
# No correlation over time

# An upper trend figures is not white noise, It has increasing mean
# If there is greater variability in a plot in the right than in the 
# left, there are changes in variance. It is not WN.
# A seasonal pattern translates into correlations or patterns over
# time. It is not WN.

# Simulate a WN model.....

# We will focus on the simplest form of WN,
# independent and identically distributed data.

# The arima.sim() can be used to estimate a WN model
# ARIMA = Autorregressive integrated moving average model
# WN is a special case of it.
# ARIMA has 3 orders: p, d, q. Thus, we name it ARIMA(p,d,q) model
# p = order for autoregression
# d = order of integration (or differencing)
# q = order for moving average.
# ARIMA (0,0,0) is the simplest model. This is a WN model.

# Simulate a WN model with list(order = c(0, 0, 0))
white_noise <- arima.sim(model = list(order=c(0,0,0)), n = 100)

# Plot your white_noise data
ts.plot(white_noise)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order=c(0,0,0)), n = 100, mean = 100, sd =10)

# Plot your white_noise_2 data
ts.plot(white_noise_2)


# Now, for a given series y we can fit a WN model
# Suppose, you have:
y <- c(109.76134,  98.31610, 100.63295,  88.74340, 101.87238, 104.62836,  96.64462,
      102.86194, 112.76247,  82.23219,  88.94434,  94.60318, 105.61113, 113.82776,
    104.82319, 101.29629,  82.98459,  88.96058, 100.60046,  91.03525, 100.19286,
    95.32537,  94.58643, 121.35830,  87.37874,  96.89007,  90.51310,  99.90843,
   79.15428,  98.18847,  99.90006,  98.91084, 101.64422, 102.79526,  84.65112,
      96.47870, 105.81547,  98.51869, 105.24366, 109.61264,  85.14201,  82.81442,
       103.03629,  93.56967,  98.08922,  81.25461, 109.18554,  80.43181, 103.55953,
       80.22269,  84.97477, 107.78363,  92.61288,  99.80293, 107.25085,  98.66378,
     91.92275,  98.32642, 112.73402,  96.02811,  92.64909,  83.08484,  97.22196,
    106.61361,  97.36943, 108.78465, 104.91858,  84.44343,  85.60786 , 96.51529,
       94.18105,  85.02851,  63.26622,  87.22137, 103.52295, 105.60216, 103.26039,
  101.11519, 108.45697,  97.67631, 103.01081, 100.61756, 105.58108,  98.72722,
   98.43996,  90.78219, 92.74599, 102.93762, 83.95306, 110.15937, 104.17578,
  99.27876, 103.25115)

# Fit the WN model to y using the arima command
arima(y, order = c(0,0,0))

# Calculate the sample mean and sample variance of y
mean(y)
var(y)
# you can see that the arima() function estimates are very close 
# to the sample mean and variance estimates


# Let´s consider the Random Walk model now (RW)
# It is a simple example of a non stationary process
# It has no specified mean or variance
# It shows very strong dependence over time with each observation
# closely related to its immediate neighbors
# However, its changes or increments follow a WN process, which is 
# stable and stationary

# Now, let´s look at the RW model
# Today = Yesterday + Noise. 
# It is defined recursively
# Yt = Yt-1 + Et
# The error is specifically mean zero WN.

# In practice, simulating a RW requires an initial point Y0.
# Giver Y0 and the first WN, you generate a new observation

# The RW model has only one parameter: the variance of the WN

# The terms of a RW can be re-arranged as a first difference series
# Yt - Yt-1 = Et --> diff(Y) is WN
# That is, the difference, or change series is WN.

# The RW model can be extended to include an intercept
# Today = Constant + Yesterday + Noise
# Yt = c + Yt-1 + Et
# where Et is a zero mean WN.
# The intercept is also called a drift coefficient
# The randow walk with drift has 2 parameters: the variance of the WN
# and the constant c

# What is the first difference of a RW w/drift?
# It is simply a constant plus noise, which is a WN with mean c


# Simulation of a RW model....

# It is a basic ts model. It comes from the cumulative sum (or 
# integration) of a mean zero WN, such that the first diff of a RW
# is a WN series.
# For reference, a RW is an ARIMA (0,1,0)

# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100)

# Plot random_walk
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk)

# Plot random_walk_diff
ts.plot(random_walk_diff)
# Clearly, this plot is WN. This is because RW is simply recursive WN
# By removing the long term trend you end up with simply WN


# Simulate the RW with drift...........

# It will not wander around zero, it can have an upward or downward
# trayectory, i.e. drift, or time trend
# This is done by including an intercept in the RW model, which
# corresponds to the slope of the RW time trend.

# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 1)

# Plot rw_drift
ts.plot(rw_drift)

# Calculate the first difference series
rw_drift_diff <- diff(rw_drift)

# Plot rw_drift_diff
ts.plot(rw_drift_diff)
# Notice that taking the difference of the RW transform it back to
# WN, regardless of the long term drift.



# Estimate the RW model for a give series Y....

random_walk <- ts(c(0.0000000,  0.8407997,  0.6374594,  0.7579217,  0.5118742,
0.1137669, 0.2789728,  0.2775946,  0.6030589,  2.9120963,  3.1819914,
2.5515731, 3.2675620,  4.9997993,  7.1091580,  8.0107086,  8.2377246,
8.7015723, 9.0512610, 13.3200714, 12.1675754, 11.5324303, 12.2601005,
12.5767819, 12.6632633, 12.7256680, 13.1994837, 12.8396495, 13.5061941,
15.4816848, 16.1771077, 16.8365295, 16.6163362, 16.1808337, 16.9662660,16.7270308,
17.6561798, 20.8901637, 20.0567603, 22.4168557, 23.1628256, 22.9168362,
21.9326982, 22.6547467, 23.6325743, 24.8637728, 25.5358779, 27.2225699,
27.3721605, 29.0197579, 30.8698419, 32.4859913, 33.1883373, 33.7998279,
32.7718376, 33.3293449, 36.0168050, 36.4458501, 38.2388467, 39.1157217,
38.7992108, 39.2275168, 37.5690113, 40.0882063, 40.5380769, 40.7578637,
41.5757014, 41.1067984, 41.1064560, 40.6455320, 39.5554851, 40.4312198,
41.9604182, 43.6866496, 45.2350442, 45.9024582, 47.5404571, 49.0971826,
48.9170239, 48.0796343, 46.7765324, 47.9775098, 46.4334193, 45.9453639,
47.0727461, 47.1890042, 51.2925271, 52.1247377, 51.3796798, 52.1270458,
51.2230171, 52.6301981, 52.4262449, 51.3564739, 52.2017866, 53.3817250,
55.0244438, 55.9303590, 55.5117188, 58.3080319, 58.1183345), 
  frequency = 1, start = 0)

# Difference your random_walk data
rw_diff <- diff(random_walk) 

# Plot rw_diff
ts.plot(rw_diff)

# Now fit the WN model to the differenced data
model_wn <-arima(x = rw_diff, order = c(0,0,0))

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Plot the original random_walk data
ts.plot(random_walk)

# Use abline(0, ...) to add time trend to the figure
abline(0, int_wn)
# This function adds one or more straight lines
# through the current plot.

# The arima() correctly identified the time trend in the 
# original RW data.


# Stationary models...............
# Stationary models are parsimonious
# They have distributional stability over time

# For an observed ts, fluctuations occur randomly
# However, the same type of random behavior often holds from one 
# time period to the next.

# For expample,
# Returns on stocks or changes in interest rates
# Each of them have very different behavior from the previous year
# But their mean, standard deviation, and other statistical properties
# stay the same, or are often similar from one year to the next one.

# A process is weak stationary if its mean, variance, and covariance
# are constant over time.
# Y1, Y2,.. is a weakly stationary process if...
# ...there is a common, constant mean, u which is the same for all t.
#....and the variance, sigma squared, is also constant for all time t.
# ...and the covariance between Yt and Ys is same (constant) for all
#...|t-s|=h, for all h.
# In other words, only depends on how close t and s are.
# They do not depend on the time indeces t and s themselves.
# For example, if Y is stationary process...
# Cov(Y2, Y5) = Cov (Y7, Y10) since they are both 3 time units apart.

# Why do we focus on stationary models?
# Stationary processes can be modeled with few parameters
# We do not need a different mean for each observation as they all
# have common expectation u.
# You can estimate u accurately by Ybar...the sample mean.

# When a process is observed, we ask if it is stationary.
# Specifically, we ask if is a stationary model appropiate for this
# data.
# Although many financial ts do not exhibit stationarity, they can
# be converted to stationary processes by considering the change in
# the series (approximately stationary, perhaps after applying a log)

# A stationary series should show random oscillation around some
# fixed level (we call this, mean reversion)
# For example, take inflation rates. They do not naturally return
# to a specific fixed level as they are heavily influenced by the 
# prevailing FED monetary policy.
# If you instead consider the changes in the rate series, you now see
# a very quick reversion to the common mean of zero.
# And there are no clear patterns over time in the change series.

# Remember that there are many commonly encountered departures from
# stationarity, including time trends, periodicity, and a
# lack of mean reversion.


# Are the WN and RW models stationary?

# Both are closely relaed but only the WN is estationary
# Recall that if we start with a mean zero WN process and compute 
# its running or cumulative sum, the result is a RW process. 
# Similarly, if we create a WN process, but change its mean from zero,
# and then compute its cumulative sum, the result is a RW process
# with a drift.

# Use arima.sim() to generate WN data
white_noise <-arima.sim(model = list(order = c(0,0,0)), n=100)

# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)

# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model = list(order = c(0,0,0)), n=100, mean=0.4)

# Use cumsum() to convert your WN drift data to RW

rw_drift <- cumsum(wn_drift)

# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))

# You can see that it is easy to reverse-engineer the RW data by 
# simply generating a cumulative sum of white noise data


#----- End of lesson 2: Predicting the future --------------









