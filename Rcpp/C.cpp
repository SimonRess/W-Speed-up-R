#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// [[Rcpp::export]]
double sqrtC(double x) {
  return sqrt(x);
}


/*** R
library(microbenchmark)
x <- runif(50)
microbenchmark(
  mean(runif(50)),
  meanC(runif(50)),
  mean(runif(200)),
  meanC(runif(200)),
  mean(runif(500)),
  meanC(runif(500))
)
*/