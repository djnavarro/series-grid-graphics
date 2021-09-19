#include<Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix skip_stone(IntegerMatrix x, int times) {
  
  // size of the matrix
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  // indexing and weighting variables
  double wt;
  int rind;
  int cind;
  int rstp;
  int cstp;
  
  for(int i = 0; i < times; i++) {
    for(int r = 0; r < nrow; r++) {
      for(int c = 0; c < ncol; c++) {
        
//        wt = R::runif(0, 1); // scalar random weight using the R namespace
//        if(wt < .5) {
          
          // sample from binomial distributions
          rstp = R::rbinom(4, 0.5);
          cstp = R::rbinom(4, 0.5);
          
          // find corresponding indices
          rind = r + rstp - 2;
          cind = c + cstp - 2;
          
          // truncate at the boundaries
          if(rind < 0) {rind = 0;}
          if(cind < 0) {cind = 0;}
          if(rind >= nrow) {rind = nrow - 1;}
          if(cind >= ncol) {cind = ncol - 1;}
          
          // replace
          x(r,c) = x(rind, cind);
//        }
      }
    }
  }
  
  return x;
}