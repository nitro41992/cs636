# Question 1

f_x <- function(x) {
  if (x < 1){
    return(5/((x-1)^2))
  } else if (x > 1) {
    return(5/((x-1)^3))
  } else {
    return(2)
  }
}

print(f_x(1))
print(f_x(10))
print(f_x(0.3))

# Question 2

fib <- function(n) {
  if (n == 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  } else if (n > 1) {
    current <- 0
    old <- 1
    for (i in seq (0, n)) {
      out <- current
      current <- current + old
      old <- out
    }
  }
  return(out)
}

print(fib(1))
print(fib(2))
# print(fib(3))
# print(fib(4))
# print(fib(5))
print(fib(100))


# Question 3

Merge <- function (S1, S2) {
  out <- c()
  final <- c()
  for (a in S1) {
    out[length(out) + 1] = a
  }
  for (b in S2) {
    out[length(out) + 1] = b
  }
  for (c in 1:(length(out) - 1)) {
    for (d in (c + 1):length(out)){
      if (out[c] > out[d]){
        a <- out[c]
        b <- out[d]
        out[c] = b
        out[d] = a
      } 
    }
  }
return(out)
}
print(Merge(seq(1, 50, by=3), seq(2, 30, by=2)))

# Question 4

Partition <- function(Pivot, Vect) {
  out_1 <- c()
  out_2 <- c()
  i <- 1
  j <- 1
  for (a in 1:length(Vect)){
    if (Vect[a] < Pivot){
      out_1[i] <- Vect[a]
      i <- i + 1
    } else {
      out_2[j] <- Vect[a]
      j <- j + 1
    }
  }
  return(list(out_1, out_2))
}

print(Partition(50, sample(1:100, 100, replace=F)))

