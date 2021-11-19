#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' 
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´gnm´.
#'
#' @name OptimGNM
#'
#' @return No return.
#' 
#' @export


OptimGNM <- R6Class("OptimGNM",
                    inherit= OptimMethod,
                    public = list(
                      fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                     npar, latent, family) {
                        result <- list()
                        result$par <- c()
                        browser()
                        for (k in 1:latent) {
                          data_model$data$w <- hidden[,k]
                          train <- nls(formula=data_model$formula, data=data_model$data,
                                       start=theta[(40*(k-1)+1):(40*(k-1)+40),], 
                                       weights=w)
                          coefs <- train$coefficients
                          if (family[[k]] == "gaussian") {
                            df <- sum(coefs != 0)  + 1
                            sigma <- sqrt(sum(train$weights * train$residuals^2/mean(train$weights))/(nrow(data_model$X) - train$rank))
                            result$par <- c(result$par, sigma, coefs)
                          } else {
                            result$par <- c(result$par, coefs)
                          }
                        }
                        result$pi_vector <- pi_vector
                        result$value <- ll(result$par)
                        #result$value <- sum(pi_vector * logLik(train))
                        result$AIC <- 2 * npar + 2 * result$value
                        result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                        result$par <- matrix(result$par, ncol=1)
                        return(result)
                      },
                      fam_distrs = list(
                        "gaussian" = gaussian(),
                        "poisson" = poisson(),
                        "logit" = quasibinomial()
                      )
                    )
)