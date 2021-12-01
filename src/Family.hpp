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
//#include <boost/math/distributions/normal.hpp>
//#include <boost/multiprecision/cpp_bin_float.hpp>
#include "distribution.hpp"
//using namespace boost::math;
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

arma::mat sigmoid(arma::mat x) {
    return (arma::log(1/(1+arma::exp(-x))));
};

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
          normal_distribution nd(mean_mat.at(i,j),sd);
          if (lg) {
            l.at(i,j) = nd.lpdf(Y.at(i,j));
        } else {
            l.at(i,j) = nd.pdf(Y.at(i,j));
        }
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
    arma::mat mean_mat = (X * theta);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;i++) {
      for (int j=0; j<Y.n_cols; j++) {
          poisson_distribution nd(mean_mat.at(i,j));
          if (lg) {
            l.at(i,j) = nd.lpdf(Y.at(i,j));
        } else {
            l.at(i,j) = nd.pdf(Y.at(i,j));
        }
      }
    }
    return l;
  }
};

class FamilyLogit : public Family<FamilyLogit> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_t = (X * theta);
    arma::mat mean_mat = sigmoid(mean_t);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;i++) {
      for (int j=0; j<Y.n_cols; j++) {
          bernoulli_distribution nd(mean_mat.at(i,j));
          if (lg) {
            l.at(i,j) = nd.lpdf(Y.at(i,j));
        } else {
            l.at(i,j) = nd.pdf(Y.at(i,j));
        }
      }
    }
    return l;
  }
};

class FamilyMultiNomial : public Family<FamilyMultiNomial> {
public:
  arma::mat logLik(const arma::vec& theta, const arma::mat& Y,
                   const arma::mat& X, const bool& lg = false)
  {
    arma::mat mean_t = (X * theta);
    arma::mat mean_mat = sigmoid(mean_t);
    arma::mat l = arma::zeros(Y.n_rows, Y.n_cols);
    for (int i=0; i<Y.n_rows;i++) {
      for (int j=0; j<Y.n_cols; j++) {
          bernoulli_distribution nd(mean_mat.at(i,j));
          if (lg) {
            l.at(i,j) = nd.lpdf(Y.at(i,j));
        } else {
            l.at(i,j) = nd.pdf(Y.at(i,j));
        }
      }
    }
    return l;
  }
};

#endif /* Family_hpp */
