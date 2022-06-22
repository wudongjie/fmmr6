#' @useDynLib fmmr6
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"

#' @title The Abstract Class of Finite Mixture Models on R6 (fmmr6) 
#'
#' @author Dongjie Wu
#'
#' @description An abstract R6Class for the Finite Mixture Models.
#'
#' @template section_fmmr6_intro
#' @name fmmr6
#'
#' @return No return

fmmr6 <- R6Class("fmmr6",
                 public = list(
                   #' @description
                   #' Create a new instance of this [R6] [R6::R6Class] class.
                   initialize = function(){stop("This is an abstract class that can't be initialized.")},
                   #' @description 
                   #' Fit the fmmr6 model
                   fit = function(){stop()},
                   #' @description 
                   #' Generate a summary for the result.
                   summarize = function(){stop("This is the summary")},
                   #' @description 
                   #' Predict the outcome given the fitted model.
                   predict = function(){stop()}
                 ),
                 private = list(
                  )
                 )
