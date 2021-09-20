#' @title An Abstract Class for Estimation Methods
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description An abstract class for estimation methods which cannot be initialized.
#'
#' @name AbstractMethod
#'
#' @return No return.


AbstractMethod <- R6Class("AbstractMethod",
    public = list(
        #'  @description
        #' The initialize function for the AbstractMethod class:
        #' This abstract class cannot be initialized
        #' @return The function will return an error.
        initialize = function() {
            stop("This is an abstract class that can't be initialized.")
        },
        #'  @description
        #' The fit function for the AbstractMethod class:
        #' This abstract class cannot be initialized
        #' @return The function will return an error.
        fit = function() {
            stop("This is an abstract class that can't be initialized.")
        }
    )
)