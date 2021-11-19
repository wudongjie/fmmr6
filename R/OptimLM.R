#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´lm.wfit´.
#'
#' @name OptimLM
#'
#' @return No return.
#' 
#' @export


OptimLM <- R6Class("OptimLM",
                    inherit = OptimMethod,
                    public = list(
                      fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                     npar, latent, family) {
                        result <- list()
                        result$par <- c()
                        for (k in 1:latent) {
                          train <- lm.wfit(data_model$X, data_model$Y, w=hidden[,k])
                          coefs <- train$coefficients
                          df <- sum(coefs != 0)  + 1
                          sigma <- sqrt(sum(hidden[,k] * (data_model$Y - data_model$X %*% coefs)^2/mean(hidden[,k]))/(nrow(data_model$X) - df))
                          result$par <- c(result$par, sigma, coefs)
                        }
                        result$pi_vector <- pi_vector
                        result$value <- ll(result$par)
                        result$AIC <- 2 * npar + 2 * result$value
                        result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                        result$par <- matrix(result$par, ncol=1)
                        return(result)
                      }
                    )
)