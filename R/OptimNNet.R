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