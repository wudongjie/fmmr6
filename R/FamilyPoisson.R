#' @title Family Object for a Poisson Distribution
#' 
#' @author Dongjie Wu
#'
#' @description An family object for a 
#' Poisson distribution.
#'
#' @name FamilyPoisson
#'
#' @return A FamilyPoisson Object
#'
#' @export

FamilyPoisson <- R6Class("FamilyPoisson",
                       inherit = Family,
                       public = list(
                         #' @description
                         #' Creates a new instance of this [R6][R6::R6Class] class.
                         initialize = function() {
                         },
                         
                         #' @description 
                         #' Generate the density function
                         gen_density = function() {
                           return(function(theta, Y, X) {
                             den <- dpois(Y, lambda=exp(X %*% theta), log=TRUE)
                             return(den)
                           })
                         },
                         
                         #' @description 
                         #' Generate the start value
                         #' @param Y (`matrix()`) \cr
                         #' A matrix with 1 column contains the dependent variable.
                         #' @param X (`matrix()`) \cr
                         #' A matrix with m column contains m independent variables.
                         gen_start = function(Y, X) {
                           if (is.vector(X)) {return(matrix(c(1)))}
                           else {return(matrix(rep(1, (ncol(X)))))}
                         },
                         
                         #' @description 
                         #' Generate the constraint
                         #' @param Y (`matrix()`) \cr
                         #' A matrix with 1 column contains the dependent variable.
                         #' @param X (`matrix()`) \cr
                         #' A matrix with m column contains m independent variables.
                         gen_constraint = function(Y, X) {
                           if (is.vector(X)) {
                             l <- list(
                               lower = matrix(c(-Inf)),
                               upper = matrix(c(+Inf))
                             )
                             return(l)
                           }
                           else {
                             l <- list(
                               lower = matrix(rep(-Inf, ncol(X))),
                               upper = matrix(rep(+Inf, ncol(X)))
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