#' @title Family Object for a MultiNomial Distribution
#' 
#' @author Dongjie Wu
#'
#' @description An family object for a 
#' MultiNomial distribution.
#'
#' @name FamilyMultiNomial
#'
#' @return A FamilyMultiNomial Object
#'
#' @export

FamilyMultiNomial <- R6Class("FamilyMultiNomial",
                       inherit = Family,
                       public = list(
                         #' @description
                         #' Creates a new instance of this [R6][R6::R6Class] class.
                         initialize = function() {
                         },
                         
                         #' @description 
                         #' Generate the density function: \cr
                         #' Assume that there are \eqn{N} observations, 
                         #' \eqn{M} variables and \eqn{P} choices.
                         #' \eqn{X} is \eqn{N \times M}, 
                         #' \eqn{Y} is \eqn{N \times P} and
                         #' \eqn{\theta} is \eqn{M \times P}
                         #' Then the density function is:
                         #' \deqn{R_k = \sum_{i \in N}(y_{ik} \times 
                         #' \sum_{j \in M}(x_{ij} \times \theta_{jk})) - 
                         #' \log(1+\sum_{i \in N}(\exp{\sum_{j \in M}(x_{ij} \times \theta_{jk})})} 
                         gen_density = function() {
                           return(function(theta, Y, X) {
                             R = suppressWarnings(rowSums(Y*(X%*%theta))-log(1+rowSums(exp(X%*%theta))))
                             return(R)
                           })
                         },
                         
                         #' @description 
                         #' Generate the start value
                         #' @param Y (`matrix()`) \cr
                         #' A matrix with 1 column contains the dependent variable.
                         #' @param X (`matrix()`) \cr
                         #' A matrix with m column contains m independent variables.
                         gen_start = function(Y,X) {
                           if (is.vector(Y)) {stop("Y must be a matrix")}
                           if (is.vector(X)) {return(matrix(1,1,ncol(Y)))}
                           else {return(matrix(1, ncol(X), (ncol(Y))))}
                         },
                         
                         #' @description 
                         #' Generate the constraint
                         #' @param Y (`matrix()`) \cr
                         #' A matrix with 1 column contains the dependent variable.
                         #' @param X (`matrix()`) \cr
                         #' A matrix with m column contains m independent variables.
                         gen_constraint = function(Y, X) {
                           if (is.vector(Y)) {stop("Y must be a matrix")}
                           if (is.vector(X)) {
                             l <- list(
                               lower = matrix(-Inf, 1, ncol(Y)),
                               upper = matrix(+Inf, 1, ncol(Y))
                             )
                             return(l)
                           }
                           else {
                             l <- list(
                               lower = matrix(-Inf, ncol(X), ncol(Y)),
                               upper = matrix(+Inf, ncol(X), ncol(Y))
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