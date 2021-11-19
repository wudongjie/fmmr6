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
                         fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                        npar, latent, family) {
                           result <- list()
                           result$par <- c()
                           # result$sddev <- c()
                           # result$tvalue <- c()
                           # result$pvalue <- c()
                           for (k in 1:latent) {
                             #data_model$data$h<- hidden[,k]
                             train <- glm.fit(data_model$X, data_model$Y, family=self$fam_distrs[[family[[k]]]], weights=hidden[,k])
                             #train <- glm(data_model$formula, family=self$fam_distrs[[family[[k]]]],data=data_model$data, weights=h)
                             coefs <- train$coefficients
                             # result$sddev <- c(result$sddev, sqrt(diag(vcov(train))))
                             # result$tvalue <- c(result$tvalue, coefs/result$sddev)
                             # result$pvalue <-c(result$pvalue, pt(result$tvalue, train$rank, lower.tail=FALSE))
                             if (family[[k]] == "gaussian") {
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
                         fam_distrs = list(
                           "gaussian" = gaussian(),
                           "poisson" = poisson(),
                           "logit" = quasibinomial()
                         )
                       )
)