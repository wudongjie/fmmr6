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
                            initialize = function(mixer, Y, X, start=NULL, constraint=NULL, glm_fit=F){
                              private$X <- X
                              private$Y <- Y
                              private$latent <- mixer$get_latent()
                              private$mixer <- mixer
                              private$glm_fit <- glm_fit
                              if (!is.null(start)) {
                                private$start <- start
                              } else {
                                private$start <- private$mixer$gen_start()(private$Y, private$X)
                              }
                              if (!is.null(constraint)) {
                                private$constraint <- constraint
                              } else {
                                private$constraint <- private$mixer$gen_constraint()(private$Y, private$X)
                              }
                            }),
                            private = list(
                              Y = NULL,
                              X = NULL,
                              latent = 1,
                              constraint = NULL,
                              likelihood_func = NULL,
                              start = NULL,
                              mixer = NULL,
                              glm_fit = FALSE
                              # dist_list = list(
                              #   "glm" = OptimGLM$new(),
                              #   "lm" = OptimLM$new(),
                              #   "nnet" = OptimNNet$new(),
                              #   "bfgs" = OptimBFGS$new()
                              # )
                            )
                          )