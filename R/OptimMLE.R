#' @title A Class for the MLE Optimization Method.
#' 
#' @import R6
#' 
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´bbmle::mle2´.
#'
#' @name OptimMLE
#'
#' @return No return.
#' 
#' @export


OptimMLE <- R6Class("OptimMLE",
                     inherit = OptimMethod,
                     public = list(
                       
                       
                       fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                      npar, latent, family) {
                         browser()
                         result <- bbmle::mle2(ll, list(theta=theta), method="BFGS")
                         # result <- suppressWarnings(optim(theta,
                         #                                  ll,
                         #                                  gr,
                         #                                  method="BFGS",
                         #                                  hessian=T))
                         result$pi_vector <- pi_vector
                         result$AIC <- 2 * npar + 2 * result$value
                         result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                         return(result)
                       }
                     )
)