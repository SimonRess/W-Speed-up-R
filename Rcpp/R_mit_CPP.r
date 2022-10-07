

fibR <- function(n) {
  if (n == 0) return(0)
  if (n == 1) return(1)
  return (fibR(n - 1) + fibR(n - 2))
}

for(i in 1:10) {
  print(fibR(i)) 
}


library(Rcpp)




#!/usr/bin/r

## this short example was provided in response to this StackOverflow questions:
## http://stackoverflow.com/questions/6807068/why-is-my-recursive-function-so-slow-in-r
## and illustrates that recursive function calls are a) really expensive in R and b) not
## all expensive in C++ (my machine sees a 700-fold speed increase) and c) the byte
## compiler in R does not help here.

## inline to compile, load and link the C++ code
if(!require("inline")) install.packages("inline")
require(inline)

## byte compiler
require(compiler)

## Rtools
if(!require("Rtools")) {
  -> https://cran.r-project.org/bin/windows/Rtools/
  install.packages("installr")
  require(installr)
  install.Rtools()
}




## we need a pure C/C++ function as the generated function
## will have a random identifier at the C++ level preventing
## us from direct recursive calls
incltxt <- '
int fibonacci(const int x) {
   if (x == 0) return(0);
   if (x == 1) return(1);
   return (fibonacci(x - 1)) + fibonacci(x - 2);
}'

## now use the snipped above as well as one argument conversion
## in as well as out to provide Fibonacci numbers via C++
fibRcpp <- cxxfunction(signature(xs="int"),
                       plugin="Rcpp",
                       incl=incltxt,
                       body='
   int x = Rcpp::as<int>(xs);
   return Rcpp::wrap( fibonacci(x) );
')

## for comparison, the original (but repaired with 0/1 offsets)
fibR <- function(seq) {
  if (seq == 0) return(0);
  if (seq == 1) return(1);
  return (fibR(seq - 1) + fibR(seq - 2));
}

## also use byte-compiled R function
fibRC <- cmpfun(fibR)

## load rbenchmark to compare
install.packages("rbenchmark")
library(rbenchmark)

N <- 35     ## same parameter as original post
res <- benchmark(fibR(N),
                 fibRC(N),
                 fibRcpp(N),
                 columns=c("test", "replications", "elapsed",
                           "relative", "user.self", "sys.self"),
                 order="relative",
                 replications=1)
print(res)  ## show result





incltxt <- '
  int mean(int[] arr, int n) { // n is size of array 
    int sum = 0; 
    for (int i = 0; i < n; i++) 
        sum += arr[i]; 
 
    return sum / n; 
} 
   return(mean);
}'

## now use the snipped above as well as one argument conversion
## in as well as out to provide Fibonacci numbers via C++
meanRcpp <- cxxfunction(signature(xs="int"),
                       plugin="Rcpp",
                       incl=incltxt,
                       body='
   int x = Rcpp::as<int>(xs);
   return Rcpp::wrap( mean(x) );
')

## for comparison, the original (but repaired with 0/1 offsets)
fibR <- function(seq) {
  if (seq == 0) return(0);
  if (seq == 1) return(1);
  return (fibR(seq - 1) + fibR(seq - 2));
}


library(Rcpp)

Rcpp.mean <- cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add(1,2,3)


cppFunction('double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}')

v <- runif(1e5)
v <- iris[,1]

meanC(v)
mean(v)

library(microbenchmark)
microbenchmark(
  mean(x),
  meanC(x)
)


double res = x[0];

cppFunction(depends = "Rcpp.h",
            'double sqrtC(double x) {
  double res = std::sqrt(x);
  return res;
}')


cppFunction('double sqrtC(double x) {
  double res = std::sqrt(x);
  return res;
}')


sqrtC(c(1,2,3))


setwd("C:/Users/simon/Desktop/Rcpp")
sourceCpp("c.cpp")

meanC(v)

sqrtC(4)

microbenchmark(
  sqrt(4456456),
  sqrtC(4456456)
)


