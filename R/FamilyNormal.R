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
                                       initialize = function() {
                                       },
                                       
                                       #' @description 
                                       #' Generate the density function
                                       gen_density = function() {
                                         return(function(theta, Y, X) {dnorm(Y, 
                                                                      mean = X %*% theta[-1], 
                                                                      sd = sqrt(theta[1]^2), log = TRUE)})
                                       },
                                       
                                       #' @description 
                                       #' Generate the start value
                                       gen_start = function() {
                                         start = function(X){
                                           constr = self$gen_constraint(X)
                                           start_v = rep(1, length(constr$lower))
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
                                       predict = function() {
                                         
                                       },
                                       
                                       #' @description 
                                       #' Generate the constraint
                                       gen_constraint = function(X) {
                                         if (is.vector(X)) {
                                           l <- list(
                                             lower = c(0, -Inf),
                                             upper = c(+Inf, +Inf)
                                           )
                                           return(l)
                                         }
                                         else {
                                           l <- list(
                                             lower = c(0, rep(-Inf, ncol(X))),
                                             upper = c(+Inf, rep(+Inf, ncol(X)))
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