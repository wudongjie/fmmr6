#' @title A Class for the GLM Optimization Method.
#' 
#' @import R6
#' 
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
                           # result$sddev <- c()
                           # result$tvalue <- c()
                           # result$pvalue <- c()
                           for (k in 1:latent) {
                             #data_model$data$h<- hidden[,k]
                             train <- glm.fit(data_model$X, data_model$Y, family=self$fam_distrs[[family]], weights=hidden[,k])
                             #train <- glm(data_model$formula, family=self$fam_distrs[[family[[k]]]],data=data_model$data, weights=h)
                             coefs <- train$coefficients
                             # result$sddev <- c(result$sddev, sqrt(diag(vcov(train))))
                             # result$tvalue <- c(result$tvalue, coefs/result$sddev)
                             # result$pvalue <-c(result$pvalue, pt(result$tvalue, train$rank, lower.tail=FALSE))
                             if (family == "gaussian") {
                               df <- sum(coefs != 0) - 1
                               sigma <- sqrt(sum(train$weights * train$residuals^2/mean(train$weights))/(nrow(data_model$X) - train$rank))
                               #sigma <- 3
                               result$par <- c(result$par, sigma, coefs)
                             } else {
                               result$par <- c(result$par, coefs)
                             }
                           }
                           result$pi_vector <- pi_vector
                           # result$p_ast <- add_ast(result$pvalue)
                           result$value <- ll(result$par)
                           #result$value <- sum(pi_vector * logLik(train))
                           result$AIC <- 2 * npar + 2 * result$value
                           result$BIC <- npar * log(nrow(data_model$X)) + 2 * result$value
                           result$par <- matrix(result$par, ncol=1)
                           return(result)
                         },
                         
                         #' @field fam_distrs (`list()`) \cr
                         #' The list of the family distributions that can be used in `glm.fit()`.
                         fam_distrs = list(
                           "gaussian" = gaussian(),
                           "poisson" = poisson(),
                           "logit" = quasibinomial()
                         )
                       )
)