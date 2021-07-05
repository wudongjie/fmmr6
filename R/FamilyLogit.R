#' @title Family Object for a Logistic Distribution
#' 
#' @author Dongjie Wu
#'
#' @description An family object for a 
#' logistic distribution.
#'
#' @name FamilyLogit
#'
#' @return A FamilyLogit Object
#'
#' @export

FamilyLogit <- R6Class("FamilyLogit",
                        inherit = Family,
                        public = list(
                          initialize = function() {
                          },
                          
                          #' @description 
                          #' Generate the density function
                          gen_density = function() {
                            sigmoid <- function(x) { 
                              return(1 / (1 + exp(x)))
                              }
                            return(function(theta, Y, X) {
                              den <- Y* log(sigmoid(X %*% theta)) + 
                                (1-Y) * log(1-sigmoid(X %*% theta))
                              return(den)
                              })
                          },
                          
                          #' @description 
                          #' Generate the start value
                          gen_start = function(X) {
                            if (is.vector(X)) {return(c(1))}
                            else {return(rep(1, (ncol(X))))}
                          },
                          
                          #' @description 
                          #' Generate the constraint
                          gen_constraint = function(X) {
                            if (is.vector(X)) {
                              l <- list(
                                lower = c(-Inf),
                                upper = c(+Inf)
                              )
                              return(l)
                            }
                            else {
                              l <- list(
                                lower = rep(-Inf, ncol(X)),
                                upper = rep(+Inf, ncol(X))
                              )
                              return(l)
                            }
                          } 
                        ),
                        active = list(
                        ),
                        private = list(
                          latent = NULL,
                          ll= NULL,
                          gr= NULL
                        )
)