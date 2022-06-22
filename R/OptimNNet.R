#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' 
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´nnet::nnet()´.
#'
#' @name OptimNNet
#'
#' @return No return.
#' 
#' @export


OptimNNet <- R6Class("OptimNNet",
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
                        result <- list()
                        result$par <- c()
                        for (k in 1:latent) {
                          train <- suppressWarnings(nnet::nnet(data_model$X[,-1], data_model$Y, 
                                                               weights=hidden[,k], size=0,
                                                               skip = T, entropy = T, softmax=F, 
                                                               trace=F, Hess = T))
                          coefs <- matrix(coef(train), ncol=ncol(data_model$Y))
                          if (k == 1) {
                            result$par <- coefs
                          } else {
                            result$par <- rbind(result$par, coefs)
                          }
                        }
                        #browser()
                        result$hessian <- train$Hessian
                        result$pi_vector <- pi_vector
                        result$value <- ll(result$par)
                        result$AIC <- 2 * npar + 2 * result$value
                        result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                        return(result)
                      }
                    )
)