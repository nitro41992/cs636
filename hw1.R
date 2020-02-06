
# 1.20

a <- sort(islands, decreasing = TRUE)[1:7]
print(a);

# 1.21

library(UsingR)
PrimeData <- primes
print(length(PrimeData))

print(length(PrimeData[PrimeData <= 100]))

print(length(PrimeData[PrimeData >= 100 & PrimeData <= 1000]))

# 1.22

# PrimeData[-1] returns all the values of PrimeData without the first element. 
print(head(PrimeData[-1]))

# The PrimeData[-n] returns all the values of PrimeData without the last element. 

n <- length(primes)
print(primes[-n])

# The result is the subtraction of the two datasets and results in the distance between respective primes elements.
# Since the distances can sometimes be equal, the resuslt of two differences can be the same resulting in twin primes where
# differences equals 2.

print(primes[-1] - primes[-n])

# Finding the count of twin primes by summing the count of 2's in the resulting subtraction of primes
print(sum(primes[-1] - primes[-n]==2))

# 1.23

data <- treering
print(length(data))

print(min(data))

print(max(data))

print(length(data[data > 1.5]))

# 1.24 


data <- mandms
print(data)
x  <- names(which(rowSums(data==0)==1))
print(x)


mean <- apply(data,1,mean)
print(data - mean)
print(names(which(rowSums(data - mean)==0)))


maxColor <- max(data)
print(names(which(rowSums(data == maxColor)==1)))

print(names(which(colSums(data == maxColor)==1)))

# 1.25

data <- nym.2002
times <- data$time
print(length(times))

convert_sec_to_time <- function(time) {
    whole = floor(time)
    fraction = round((time - whole) * 60)
    sprintf("%1$d:%2$d", whole, fraction)
}

fastTime <- min(times)
print(fastTime)


minFastTime <- fastTime / 60
print(convert_sec_to_time(minFastTime))


slowTime <- max(times)
print(slowTime)

minSlowTime <- slowTime / 60
print(convert_sec_to_time(minSlowTime))

# 1.27

data <- uspop
names(data) <- seq(1790,1970,10)
print(data)

intDec <- diff(data)
print(intDec)


x <- data[(which.max(intDec))]
y <- data[which.max(diff(intDec))+1]
print(x)
print(y)

# It can be expected that the difference will always be increasing, as shown in the data from 1790 to 1970 because of increaing global econamy, 
# and technological advancements aimed at prolonging the global population over time.