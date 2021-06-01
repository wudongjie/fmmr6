#' @title An Abstract Class for Estimation Methods
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description An abstract class for estimation methods which cannot be initialized.
#'
#' @name abstract_method
#'
#' @return No return.
#'
#' @export

abstract_method <- R6Class("abstract_method",
    public = list(
        #' @description 
        #' This is the abstract class for estimation methods.
        #' @param fit `function(1)` \cr
        #' Fit the model using data with the method.
        initialize = function() {
            stop("This is an abstract class that can't be initialized.")
        },
        fit = function() {
            stop("This is an abstract class that can't be initialized.")
        }
    )
)