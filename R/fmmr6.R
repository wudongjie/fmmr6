#' @title The Abstract Class of Finite Mixture Models on R6 (fmmr6) 
#'
#' @author Dongjie Wu
#'
#' @description An abstract R6Class for the Finite Mixture Models.
#'
#' @name fmmr6
#'
#' @return
#'
#' @export

fmmr6 <- R6Class("fmmr6",
                 public = list(
                   initialize = function(){stop("This is an abstract class that can't be initialized.")},
                   fit = function(){stop()},
                   summarize = function(){stop("This is the summary")},
                   predict = function(){stop()}
                 ),
                 private = list(
                  )
                 )
