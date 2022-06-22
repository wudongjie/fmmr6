// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// mix_ll
NumericVector mix_ll(const arma::vec& theta, const arma::mat& Y, const arma::mat& X, const arma::mat& d, int latent, Rcpp::CharacterVector family, bool isLog, const arma::mat& constraint);
RcppExport SEXP _fmmr6_mix_ll(SEXP thetaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP dSEXP, SEXP latentSEXP, SEXP familySEXP, SEXP isLogSEXP, SEXP constraintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type latent(latentSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type family(familySEXP);
    Rcpp::traits::input_parameter< bool >::type isLog(isLogSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type constraint(constraintSEXP);
    rcpp_result_gen = Rcpp::wrap(mix_ll(theta, Y, X, d, latent, family, isLog, constraint));
    return rcpp_result_gen;
END_RCPP
}
// pi_ll
Rcpp::NumericVector pi_ll(const arma::vec& alpha, const arma::mat& X, const arma::mat& d, int latent);
RcppExport SEXP _fmmr6_pi_ll(SEXP alphaSEXP, SEXP XSEXP, SEXP dSEXP, SEXP latentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type latent(latentSEXP);
    rcpp_result_gen = Rcpp::wrap(pi_ll(alpha, X, d, latent));
    return rcpp_result_gen;
END_RCPP
}
// post_pr
arma::mat post_pr(const arma::vec& theta, const arma::mat& pi_m, const arma::mat& Y, const arma::mat& X, const int& latent, Rcpp::CharacterVector family, const arma::mat& constraint);
RcppExport SEXP _fmmr6_post_pr(SEXP thetaSEXP, SEXP pi_mSEXP, SEXP YSEXP, SEXP XSEXP, SEXP latentSEXP, SEXP familySEXP, SEXP constraintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type pi_m(pi_mSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const int& >::type latent(latentSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type family(familySEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type constraint(constraintSEXP);
    rcpp_result_gen = Rcpp::wrap(post_pr(theta, pi_m, Y, X, latent, family, constraint));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fmmr6_mix_ll", (DL_FUNC) &_fmmr6_mix_ll, 8},
    {"_fmmr6_pi_ll", (DL_FUNC) &_fmmr6_pi_ll, 4},
    {"_fmmr6_post_pr", (DL_FUNC) &_fmmr6_post_pr, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_fmmr6(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
