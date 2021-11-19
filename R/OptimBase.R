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
                      fit = function(data_model, theta, ll, gr, hidden, pi_vector,
                                     npar, latent, family) {
                        result <- suppressWarnings(optim(theta,
                                                         ll,
                                                         gr,
                                                         method="CG",
                                                         hessian=T))
     
                        # result <- suppressWarnings(lbfgs(ll,
                        #                                  gr,
                        #                                  theta,
                        #                                  invisible=1))
                        # browser()
                        # result <- suppressWarnings(optimx(theta,
                        #                                  ll,
                        #                                  gr,
                        #                                  method="L-BFGS-B",
                        #                                  hessian=T))
                        
                        #result$coef <- coef(result)
                        #browser()
                        # result$df <- nrow(data_model$X) - (length(result$par) + latent - 1)
                        # ny <- 1
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