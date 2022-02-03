#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
List paircountxx(NumericMatrix x, double dr, double rmax) {

  // initialize variables
  int nx = x(_,1).size();
  int nr = round(rmax/dr)+1;
  IntegerVector count(nr);
  count[0] = nx; // auto-pairs i-i

  // count pairs
  for (int i = 0; i<(nx-1); i++) {
    for (int j = i+1; j<nx; j++) {
      double r = sqrt(sum(pow(x(i,_)-x(j,_),2)));
      if (r<=rmax) {
        int index = round(r/dr);
        count[index] = count[index]+1;
      }
    }
  }

  // output
  List ret;
  ret["count"] = count;
  return ret;
}

// [[Rcpp::export]]
List paircountxy(NumericMatrix x, NumericMatrix y, double dr, double rmax) {

  // initialize variables
  int nx = x(_,1).size();
  int ny = y(_,1).size();
  int nr = round(rmax/dr)+1;
  IntegerVector count(nr);

  // count pairs
  for (int i = 0; i<nx; i++) {
    for (int j = 0; j<ny; j++) {
      double r = sqrt(sum(pow(x(i,_)-y(j,_),2)));
      if (r<=rmax) {
        int index = round(r/dr);
        count[index] = count[index]+1;
      }
    }
  }

  // output
  List ret;
  ret["count"] = count;
  return ret;
}

// [[Rcpp::export]]
List paircountxx1d(NumericVector x, double dr, double rmax) {

  // initialize variables
  int nx = x.size();
  int nr = round(rmax/dr)+1;
  IntegerVector count(nr);
  count[0] = nx; // auto-pairs i-i

  // count pairs
  for (int i = 0; i<(nx-1); i++) {
    for (int j = i+1; j<nx; j++) {
      double r = abs(x(i)-x(j));
      if (r<=rmax) {
        int index = round(r/dr);
        count[index] = count[index]+1;
      }
    }
  }

  // output
  List ret;
  ret["count"] = count;
  return ret;
}

// [[Rcpp::export]]
List paircountxy1d(NumericVector x, NumericVector y, double dr, double rmax) {

  // initialize variables
  int nx = x.size();
  int ny = y.size();
  int nr = round(rmax/dr)+1;
  IntegerVector count(nr);

  // count pairs
  for (int i = 0; i<nx; i++) {
    for (int j = 0; j<ny; j++) {
      double r = abs(x(i)-y(j));
      if (r<=rmax) {
        int index = round(r/dr);
        count[index] = count[index]+1;
      }
    }
  }

  // output
  List ret;
  ret["count"] = count;
  return ret;
}
