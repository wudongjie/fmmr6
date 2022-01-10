#' @title A Class for Optimization Methods
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description A class for optimization methods which cannot be initialized.
#'
#' @name OptimMethod
#'
#' @return No return.


OptimMethod <- R6Class("OptimMethod",
                       inherit = AbstractMethod,
                       public = list(
                         #' @description 
                         #' The initialization of the optimization methods.
                         initialize = function() {
                         }
                       ),
                       private = list(
                       )
)