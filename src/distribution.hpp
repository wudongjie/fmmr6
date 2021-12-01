//
//  distribution.hpp
//  test_rcpp
//
//  Created by Dongjie Wu on 24/11/2021.
//
#define _USE_MATH_DEFINES

#ifndef distribution_hpp
#define distribution_hpp

#include <iostream>
#include <math.h>
#include <boost/multiprecision/cpp_bin_float.hpp>


class normal_distribution{
public:
    normal_distribution(const double& mean, const double& sd)
    {
        m_mean = mean;
        m_sd = sd;
    }
    
    // Distribution functions
    double pdf(const double& x){
        using boost::math::constants::pi;
        return (1.0/(m_sd*sqrt((2.0 * pi<double>())))) * exp(-pow(x-m_mean,2)/(2*pow(m_sd,2)));
    };
    
    double lpdf(const double& x){
        using boost::math::constants::pi;
        return (-log(sqrt(2.0 * pi<double>())*m_sd)-pow(x-m_mean,2)/(2*pow(m_sd,2)));
    };
    
    
    double cdf(const double& x) {return(0);}

    // Inverse cumulative distribution functions (aka the quantile function)
    double inv_cdf(const double& quantile) {return(0);}
    
    // Descriptive stats
    double mean() {return(0);}
    double var() {return(0);}
    double stdev() {return(0);}

private:
    double m_mean;
    double m_sd;
};


class poisson_distribution {
public:
    poisson_distribution(const double& lambda)
    {
        m_lambda = lambda;
    }
    
    // Distribution functions
    double pdf(const double& x){
        return (log((exp(-m_lambda)*pow(m_lambda,x))/ boost::math::factorial<double>(x)));
    };
    
    double lpdf(const double& x){
      if (std::isinf(m_lambda)) {
        return -std::numeric_limits<double>::infinity(); }
      else {
        return (-m_lambda+x*log(m_lambda)-log(boost::math::factorial<double>(x)));
      }
    };
    
    double cdf(const double& x) {return(0);}

    // Inverse cumulative distribution functions (aka the quantile function)
    double inv_cdf(const double& quantile) {return(0);}
    
    // Descriptive stats
    double mean() {return(0);}
    double var() {return(0);}
    double stdev() {return(0);}

private:
    double m_lambda;
};

class bernoulli_distribution {
public:
    bernoulli_distribution(const double& p)
    {
        assert(p>=0);
        assert(p<=1);
        m_p = p;
    }
    
    // Distribution functions
    double pdf(const double& x){
        assert((x==1.0)||(x==0.0));
        return (pow(m_p,x)*pow(1-m_p,1-x));
    };
    
    double lpdf(const double& x){
        assert((x==1.0)||(x==0.0));
        return (x*log(m_p)+(1-x)*log(1-m_p));
    };
    
    double cdf(const double& x) {return(0);}

    // Inverse cumulative distribution functions (aka the quantile function)
    double inv_cdf(const double& quantile) {return(0);}
    
    // Descriptive stats
    double mean() {return(0);}
    double var() {return(0);}
    double stdev() {return(0);}

private:
    double m_p;
};

class categorical_distribution {
public:
    categorical_distribution(const std::vector<double>& pV)
    {
        m_pV = pV;
    }
    
    // Distribution functions
    double pdf(const std::vector<double>& x){
        assert(x.size() == m_pV.size());
        double result = 1;
        for (int i=0;i<x.size();i++) {
            result = result*pow(m_pV[i],x[i]);
        }
        return result;
    };
    
    double lpdf(const std::vector<double>& x){
        assert(x.size() == m_pV.size());
        double result = 0;
        for (int i=0;i<x.size();i++) {
            result = result+x[i]*log(m_pV[i]);
        }
        return result;
    };
    
    double cdf(const double& x) {return(0);}

    // Inverse cumulative distribution functions (aka the quantile function)
    double inv_cdf(const double& quantile) {return(0);}
    
    // Descriptive stats
    double mean() {return(0);}
    double var() {return(0);}
    double stdev() {return(0);}

private:
    std::vector<double> m_pV;
};


#endif /* distribution_hpp */
