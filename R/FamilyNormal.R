#' @title Family Object for a Normal Distribution
#' 
#' @author Dongjie Wu
#'
#' @description An family object for a 
#' normal distribution.
#'
#' @name FamilyNormal
#'
#' @return A FamilyNormal Object
#'
#' @export

FamilyNormal <- R6Class("FamilyNormal",
                                     inherit = Family,
                                     public = list(
                                       #' @description
                                       #' Creates a new instance of this [R6][R6::R6Class] class.
                                       initialize = function() {
                                       },
                                       
                                       #' @description 
                                       #' Generate the density function
                                       gen_density = function() {
                                         return(function(theta, Y, X) {dnorm(Y, 
                                                                      mean = X %*% theta[-1], 
                                                                      sd = sqrt(theta[1]^2), log = T)})
                                       },
                                       
                                       #' @description 
                                       #' Generate the start value                    
                                       #' @param Y (`matrix()`) \cr
                                       #' A matrix with 1 column contains the dependent variable.
                                       #' @param X (`matrix()`) \cr
                                       #' A matrix with m column contains m independent variables.
                                       gen_start = function() {
                                         start = function(Y, X){
                                           constr = self$gen_constraint(Y, X)
                                           start_v = matrix(rep(1, length(constr$lower)))
                                           for (i in 1:length(constr$lower))
                                           {
                                             cmin <- constr$lower[i]
                                             cmax <- constr$upper[i]
                                             if (is.infinite(cmin)) {
                                               cmin <- -100
                                             }
                                             if (is.infinite(cmax)) {
                                               cmax <- 100
                                             }
                                             start_v[i] <- runif(1, min=cmin, max=cmax)
                                           }
                                           return(start_v)
                                         }
                                         return(start)
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
                                             lower = matrix(c(0, -Inf)),
                                             upper = matrix(c(+Inf, +Inf))
                                           )
                                           return(l)
                                         }
                                         else {
                                           l <- list(
                                             lower = matrix(c(0, rep(-Inf, ncol(X)))),
                                             upper = matrix(c(+Inf, rep(+Inf, ncol(X))))
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