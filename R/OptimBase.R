#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' 
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´optim()´ in the R base.
#'
#' @name OptimBase
#'
#' @return No return.
#' 
#' @export


OptimBase <- R6Class("OptimBase",
                    inherit = OptimMethod,
                    public = list(
                      #' @description 
                      #' Run the optimization for the model.
                      #' @param data_model (`DataModel()`) \cr
                      #' The DataModel object contains data used in the fmmr6.
                      #' @param theta (`numeric()`) \cr
                      #' The coefficients to estimates.
                      #' @param ll (`function()`) \cr
                      #' The loglikelihood function.
                      #' @param gr (`function()`) \cr
                      #' The gradient function.
                      #' @param hidden (`matrix()`) \cr
                      #' The matrix of the posterior probability.
                      #' @param pi_vector (`numeric()`) \cr
                      #' A vector of the prior probability `pi`.
                      #' @param npar (`integer()`) \cr
                      #' Number of the parameters.
                      #' @param latent (`integer(1)`) \cr
                      #' The number of latent classes
                      #' @param family (`character(1)|character()`) \cr
                      #' The distribution family which can be either a string like "gaussian"
                      #' or a vector like `c("gaussian", "gaussian")`.
                      #' @return
                      #' Return the optimization result with the estimates, 
                      #' the Loglikelihood value and the information criteria.
                      fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                     npar, latent, family) {
                        result <- suppressWarnings(optim(theta,
                                                         ll,
                                                         gr,
                                                         method="BFGS",
                                                         hessian=T))
  
                        # if ("multinom" %in% family) {
                        #   ny <- ncol(data_model$Y)
                        #   result$df <- result$df*ny
                        # }
                        # kpar <- length(result$par) / ny
                        # result$sddev <- sqrt(diag(solve(result$hessian)))
                        # result$tvalue <- result$par/result$sddev
                        # result$pvalue <- pt(result$tvalue, result$df, lower.tail=FALSE)
                        # result$p_ast <- add_ast(result$pvalue)
                        result$pi_vector <- pi_vector
                        result$AIC <- 2 * npar + 2 * result$value
                        result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                        return(result)
                      }
                    )
)