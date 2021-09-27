#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description A class for the optimization methods to use ´glm.fit´.
#'
#' @name OptimGLM
#'
#' @return No return.
#' 
#' @export


OptimGLM <- R6Class("OptimGLM",
                       inherit = OptimMethod,
                       public = list(
                         initialize = function() {
                           
                         },
                         fit = function(X, Y, latent, family, weights) {
                           result <- list()
                           fam <- private$mixer$get_family_init()
                           result$par <- c()
                           for (k in 1:latent) {
                             train <- glm.fit(X, Y, family=family, weights=weights)
                             coefs <- train$coefficients
                             result$par <- c(result$par, coefs)
                           }
                           result$pi_vector <- pi_vector
                           result$value <- ll(result$par)
                           result$AIC <- 2 * npar + 2 * result$value
                           result$BIC <- npar * log(nrow(private$X)) + 2 * result$value
                           return(result)
                         }
                       )
)