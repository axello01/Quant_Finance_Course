# Referencias: DataCamp.com
# Course: Introduction to time series analysis
# Lesson 1: Exploratory TS data analysis

# Idea here is to introduce you to some time series analysis concepts & techniques

# This lesson shows you how to organize and visualize TS in R.

# TS is a sequence of data in chronological order.
# Data is recorded sequentially, over time.
# TS data is everywhere. 
# Examples: daily log return on a stock / monthly CPI.
# TS data is dated or time stamped in R. 

# Exploring raw time series ----------

# Let´s assume you are given the following data:
t <- c(1120, 1160,  963, 1210, 1160, 1160,  813, 1230, 1370, 1140,  995,  935, 1110,  994, 1020,
960, 1180,  799,  958, 1140, 1100, 1210, 1150, 1250, 1260, 1220, 1030, 1100,  774, 840,
874,  694,  940,  833,  701,  916,  692, 1020, 1050,  969,  831,  726,  456,  824,  702,
1120, 1100,  832,  764,  821,  768,  845,  864,  862,  698,  845,  744,  796, 1040,  759,
781,  865,  845,  944,  984,  897,  822, 1010,  771,  676,  649,  846,  812,  742,  801,
1040,  860,  874,  848,  890,  744,  749,  838, 1050,  918,  986,  797,  923,  975,  815,
1020,  906,  901, 1170,  912,  746,  919,  718,  714,  740)
# The vector t, above, is the River Nile annual streamflow data.

# To create a time series object, you use the funciton ts()
# Want to know more?
? ts()

# Now, we create the ts object
Nile <- ts(t, frequency = 1, start = 1871)

# The most basic starting point is to display it in an intuitive format
# To view a raw TS data we use print()
# Another useful command is length()
# If the data set is very long, you can use head() or tail()

# Print the Nile dataset
print(Nile)

# List the number of observations in the Nile dataset
length(Nile)

# Display the first 10 elements of the Nile dataset
head(Nile, n=10)

# Display the last 12 elements of the Nile dataset
tail(Nile, n=12)


# Basic time series plots ---------------

# We use the plot() function. 
# Notice that a Time index for the horizontal axis is now included

# Plot the Nile data
plot(Nile)

# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")

# Plot the Nile data with xlab, ylab, main, and type arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})", 
     main= "Annual River Nile Volume at Aswan, 1871-1970", type = "b")
# You can alsy try type = "l"


# Descriptive info about structure and patterns in a TS ------------

# Let´s assume that you are given an AirPassengers data set which you don´t
# know anything about it.

# Plot AirPassengers
plot(AirPassengers)

# View the start and end dates of AirPassengers
start(AirPassengers) # returns time index of first observation
end(AirPassengers) # returns time index of last observation

# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers) # calcs a vector of time indices, with one element for
# each time index on which the series was observed
deltat(AirPassengers) # returns the fixed time interval between observations
frequency(AirPassengers) # number of observations per unit time
cycle(AirPassengers) # returns the position in the cycle of each observation


# Missing values ----------------------

# They are denoted NA in R. It is useful to know their locations
# It is also important to know how they are handled by R functions
# Sometimes we may want to ignore them, some other times to estimate them

print(AirPassengers)

# Let´s explore this issue transforming our AirPassengers
AirPassengers[85:96] <- NA
plot(AirPassengers)

# Compute the mean of AirPassengers
mean(AirPassengers, na.rm = TRUE)

# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = 275.9394)

# Generate another plot of AirPassengers
plot(AirPassengers)

# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3) #overlies a
# graph on top of the other
# it seems that mean input is not good as a substitute for NAs

# We make sure our data has not been changed
plot(AirPassengers)
print(AirPassengers)


# A time series is more than a vector of numbers
# It also includes an index for each observation
# Given a vector, you can apply the ts() to create a time series
# object.
# These objects will be ts class.
# They represent data that is, approximately, evenly spaced over time
# Why would you want to create a time series class object?
# Because you can use different enhancements...
# 1. improved plotting
# 2. access to time index information
# 3. model estimation and forecasting

# Creating a time series object with ts() and exploring it--------

data_vector <- c(2.0521941073,  4.2928852797,  3.3294132944,                 3.5085950670,  0.0009576938,
1.9217186345,  0.7978134128,  0.2999543435,  0.9435687536,
0.5748283388, -0.0034005903,  0.3448649176,  2.2229761136,
0.1763144576,  2.7097622770, 1.2501948965, -0.4007164754,
0.8852732121, -1.5852420266, -2.2829278891, -2.5609531290,
-3.1259963754, -2.8660295895, -1.7847009207, -1.8894912908,
-2.7255351194, -2.1033141800, -0.0174256893, -0.3613204151,
-2.9008403327, -3.2847440927, -2.8684594718, -1.9505074437,
-4.8801892525, -3.2634605353, -1.6396062522, -3.3012575840,
-2.6331245433, -1.7058354022, -2.2119825061, -0.5170595186,
0.0752508095, -0.8406994716, -1.4022683487, -0.1382114230,
-1.4065954703, -2.3046941055,  1.5073891432,  0.7118679477,
-1.1300519022)

# Use print() and plot() to view data_vector
print(data_vector)
plot(data_vector)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <- ts(data_vector, start = 2004, frequency =4)

# Use print() and plot() to view time_series
print(time_series)
plot(time_series)
# ts objects are treated differently by commands such as print()
# and plot()

# Check whether data_vector and time_series are ts objects
is.ts(data_vector)
is.ts(time_series)


# Check whether Nile is a ts object
is.ts(Nile)

# Check whether AirPassengers is a ts object
is.ts(AirPassengers)




# Check whether EuStockMarkets is a ts object
is.ts(EuStockMarkets)

# View the start, end, and frequency of EuStockMarkets
start(EuStockMarkets)
end (EuStockMarkets)
frequency (EuStockMarkets)


# Generate a simple plot of EuStockMarkets
plot(EuStockMarkets)

# Use ts.plot with EuStockMarkets
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", 
        ylab = "Index Value", 
        main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(EuStockMarkets), 
       lty = 1, col = 1:4, bty = "n")


# ---------- End of Lesson 1: Exploratory TS data analysis-----





