arma::mat post_pr(const arma::vec& theta, const arma::vec& pi_v,
                      const arma::mat& Y, const arma::mat& X, const int& latent,
                  Rcpp::CharacterVector family, const arma::mat& constraint);
