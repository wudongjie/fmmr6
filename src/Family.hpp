//
//  Family.hpp
//  test_rcpp
//
//  Created by Dongjie Wu on 18/11/2021.
//

#ifndef Family_hpp
#define Family_hpp

#include <stdio.h>
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


// !! need to decide provide the option of not log
template <class T>
class Family {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false) {
    return(static_cast<T*>(this) -> logLik(theta, Y, X, lg));
  }
  
};

class FamilyNormal : public Family<FamilyNormal> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_mat = (X * theta.subvec(1,(theta.n_elem-1)));
    double sd = sqrt(theta[0] * theta[0]);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;i++) {
      for (int j=0; j<Y.n_cols; j++) {
        //normal_distribution nd(mean_mat.at(i,j),sd);
        l.at(i,j) = R::dnorm4(Y.at(i,j), mean_mat.at(i,j), sd, lg);
      }
    }
    return l;
  }
};

class FamilyPoisson : public Family<FamilyPoisson> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_mat = arma::exp(X * theta); // the precision problem
    //Rcpp::Rcout << mean_mat << std::endl;
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;i++) {
      for (int j=0; j<Y.n_cols; j++) {
        l.at(i,j) = R::dpois(Y.at(i,j), mean_mat.at(i,j), lg);
      }
    }
    return l;
  }
};

class FamilyLogit : public Family<FamilyLogit> {
public:
  arma::mat sigmoid(arma::mat x) {
    return (1/(1+arma::exp(-x)));
  };
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_t = (X * theta);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    l = (Y % mean_t) - arma::log1p(arma::exp(mean_t));
    return l;
  }
};

class FamilyMultiNomial : public Family<FamilyMultiNomial> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat theta_t = arma::mat(theta);
    theta_t.reshape(X.n_cols, Y.n_cols);
    arma::mat mean_mat = (X * theta_t);
    arma::vec r = arma::sum((Y % mean_mat), 1) - log(1 + arma::sum(arma::exp(mean_mat), 1));
    arma::mat l = arma::mat(r);
    return l;
  }
};

#endif /* Family_hpp */