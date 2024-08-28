#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat arma_cosine(const arma::mat &mat){
  arma::mat cosines = mat * mat.t();
  arma::mat square = mat % mat;
  arma::colvec b = sum(square,1);
  arma::mat denum = sqrt(b) * sqrt(b.t());
  return cosines / denum;
}
