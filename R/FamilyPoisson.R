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