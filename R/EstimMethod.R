#' @title An Abstract Class for Estimation Methods
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description An abstract class for estimation methods which cannot be initialized.
#'
#' @name EstimMethod
#'
#' @return No return.


EstimMethod <- R6Class("EstimMethod",
                          inherit = AbstractMethod,
                          public = list(
                            data_model = NULL,
                            latent = 1,
                            optim_method = NULL,
                            #' @description
                            #' Create a new instance of this [R6] [R6::R6Class] class.
                            #' @param mixer (`Mixer(1)`) \cr
                            #' A mixer object contains the mixed density function of the model.
                            #' @param Y (`matrix()`) \cr
                            #' A matrix with 1 or k columns contains the dependent variable/variables.
                            #' @param X (`matrix()`) \cr
                            #' A matrix with m column contains m independent variables.
                            #' @param start (`matrix()`) \cr
                            #' The matrix of start values for the EM algorithm.
                            #' @param constraint (`matrix()`) \cr
                            #' The matrix of constraint values for the EM algorithm
                            #' @param glm_fit (`boolean(1)`) \cr
                            #' Whether use the `glm.fit()`, `ols.wfit()` or `nnet()` to fit the model.
                            #' @return Return a R6 object of class em.
                            initialize = function(mixer, data_model, start=NULL, constraint=NULL, optim_method="base"){
                              self$data_model <- data_model
                              self$latent <- mixer$latent
                              self$optim_method <- optim_method
                              private$.mixer <- mixer
                              private$.likelihood_func <- mixer$ll
                              if (!is.null(start)) {
                                private$.start <- start
                              } else {
                                private$.start <- private$.mixer$gen_start()(self$data_model$Y, self$data_model$X)
                              }
                              if (!is.null(constraint)) {
                                private$.constraint <- constraint
                              } else {
                                private$.constraint <- private$.mixer$gen_constraint()(self$data_model$Y, self$data_model$X)
                              }
                            },
                            partial = function(f, ...) {
                              l <- list(...)
                              function(...) {
                                do.call(f, c(l, list(...)))
                              }
                            }),
                            private = list(
                              .constraint = NULL,
                              .likelihood_func = NULL,
                              .start = NULL,
                              .mixer = NULL,
                              dist_list = list(
                                "glm" = quote(OptimGLM),
                                "lm" = quote(OptimLM),
                                "nnet" = quote(OptimNNet),
                                "base" = quote(OptimBase),
                                "gnm" = quote(OptimGNM),
                                "mle" = quote(OptimMLE)
                              )
                            )
                          )
