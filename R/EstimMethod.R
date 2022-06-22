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
                            #' @field data_model (`DataModel()`) \cr
                            #' The DataModel Object that stores the data using in the fmmr6.
                            data_model = NULL,
                            
                            #' @field constraint (`matrix()`) \cr
                            #' The constraint matrix.
                            constraint = NULL,
                            
                            #' @field latent (`integer(1)`) \cr
                            #' The number of latent classes
                            latent = 1,
                            
                            #' @field optim_method (`character(1)`) \cr
                            #' The optimization method to use to fit the model.
                            optim_method = NULL,
                            
                            #' @description
                            #' Create a new instance of this [R6] [R6::R6Class] class.
                            #' @param latent (`numeric(1)`) \cr
                            #' The number of latent classes.
                            #' @param data_model (`DataModel()`) \cr
                            #' The DataModel object contains data used in the fmmr6.
                            #' @param start (`matrix()`) \cr
                            #' The matrix of start values for the EM algorithm.
                            #' @param optim_method (`character(1)`) \cr
                            #' The optimization method to use to fit the model.
                            #' The default is `base`.
                            #' @param use_llc (`boolean(1)`) \cr
                            #' Whether to use the complete log-likelihood or the normal log-likelihood.
                            #' The default is `TRUE`.
                            #' @param constraint (`matrix()`) \cr
                            #' The matrix of constraint values for the EM algorithm
                            #' @param concomitant (`formula(1)`) \cr
                            #' The formula for the concomitant model. E.g. `~ z1 + z2 + z3`.
                            #' @return Return a R6 object of class em.
                            initialize = function(latent, data_model, start=NULL, 
                                                  optim_method="base",use_llc=T,
                                                  constraint=matrix(1),concomitant=NULL){
                              self$constraint <- constraint
                              self$data_model <- data_model
                              self$latent <- latent
                              self$optim_method <- optim_method
                              private$.use_llc <- use_llc
                              private$.likelihood_func <- mix_ll
                              if (!is.null(concomitant)){
                                num_para <- ncol(self$data_model$Z)*self$latent
                                private$.pi_ll <- pi_ll
                                private$.start_z <- runif(num_para,-2,2)
                              }
                              if (!is.null(start)) {
                                private$.start <- start
                              } else {
                                if (length(c(self$constraint)) == 1) {
                                  private$.start <- self$gen_start()(self$data_model$Y, self$data_model$X)
                                } else {
                                  private$.start <- runif(unique(c(constraint)))
                                }
                              }
                            },
                            
                            #' @description 
                            #' Generate the partial function by filling in some parameters.
                            #' @param f (`function()`) \cr
                            #' The function to be filled in.
                            #' @param ... (`list()`) \cr
                            #' The list of the parameters to filled in to the functions.
                            partial = function(f, ...) {
                              l <- list(...)
                              function(...) {
                                do.call(f, c(l, list(...)))
                              }
                            },
                            
                            #' @description 
                            #' Generate the start values of parameters.
                            gen_start = function() {
                              start = function(Y, X){
                                constr = self$gen_constraint()(Y, X)
                                start_v = rep(1, length(constr$lower))
                                
                                gen_start_value <- function(vl,vh) {
                                  if (is.infinite(vl)) {
                                    vl <- -2
                                  }
                                  if (is.infinite(vh)) {
                                    vh <- 2
                                  }
                                  start_v <- runif(1, min=vl, max=vh)
                                  return(start_v)
                                }
                                start = matrix(mapply(gen_start_value, 
                                                      constr$lower, 
                                                      constr$upper), 
                                               ncol=ncol(constr$lower))
                                return(start)
                              }
                            },
                            #' @description 
                            #' Generate the constraint of parameters.
                            gen_constraint = function(){
                              latent = self$latent
                              constr = function(Y, X){
                                mix_l = c()
                                mix_h = c()
                                for (i in 1:latent) {
                                  if (self$data_model$family=="unidiff") {
                                    uY = length(unique(self$data_model$Y)) - 1
                                    uX = length(unique(self$data_model$X[,1])) - 1
                                    uZ = length(unique(self$data_model$X[,2])) - 1
                                    uSize = (uY*(uZ+1)+uY*uX+uZ)
                                    mix_l = rbind(mix_l, matrix(rep(-Inf, uSize)))
                                    mix_h = rbind(mix_h, matrix(rep(+Inf, uSize)))
                                  } else if (self$data_model$family=="gaussian") {
                                    mix_l = rbind(mix_l, matrix(c(0, rep(-Inf, ncol(self$data_model$X)))))
                                    mix_h = rbind(mix_h, matrix(c(+Inf, rep(+Inf, ncol(self$data_model$X)))))
                                  } else {
                                  mix_l = rbind(mix_l,matrix(-Inf, ncol(X), ncol(Y)))
                                  mix_h = rbind(mix_h,matrix(+Inf, ncol(X), ncol(Y)))
                                  }
                                }
                                return(
                                  list(
                                    lower = mix_l,
                                    upper = mix_h
                                  )
                                )
                              }
                              return(constr)
                            }
                            ),
                            private = list(
                              .likelihood_func = NULL,
                              .pi_ll = NULL,
                              .start = NULL,
                              .start_z = NULL,
                              .use_llc = NULL,
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
