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
#'
#' @export

AbstractMethod <- R6Class("AbstractMethod",
    public = list(
        initialize = function() {
            stop("This is an abstract class that can't be initialized.")
        },
        fit = function() {
            stop("This is an abstract class that can't be initialized.")
        }
    )
)