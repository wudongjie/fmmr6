#' @title An Abstract Class for Distribution Families
#' 
#' @import R6
#' @author Dongjie Wu
#'
#' @description An abstract class for Distribution Families 
#' which cannot be initialized.
#'
#' @name Family
#'
#' @return No return.


Family <- R6Class("Family",
                          public = list(
                            #'  @description
                            #' The initialize function for the Family class:
                            #' This class cannot be initialized
                            #' @return The function will return an error.
                            initialize = function() {
                              stop("This is an abstract class that can't be initialized.")
                            },
                            
                            #' @description 
                            #' Generate the density function
                            gen_density = function() {},
                            
                            #' @description 
                            #' Generate the start value
                            gen_start = function() {},
                            
                            #' @description 
                            #' Generate the constraint
                            gen_constraint = function() {} 
                          ),
                          private = list()
                          )