#' @title Fmmr6 DataModel Object
#'
#' @author Dongjie Wu
#'
#' @description A concrete R6 object to generate data based on the formula 
#' to use in the fmmr6.
#'
#' @name DataModel
#'
#' @return Returns R6 object of class DataModel.
#'
#' @export

DataModel <- R6Class("DataModel", 
                  public = list(
                    #' @field formula (`formula(1)`)\cr
                    #' The formula used in the model.
                    formula = NULL,
                    
                    #' @field data_format (`character(1)`)\cr
                    #' The data format: default is "data.frame".
                    data_format = "data.frame",
                    
                    #' @field data (`data.frame()|data.table()|table()`)\cr
                    #' Data used in the model.
                    data = NULL,
                    
                    #' @field X (`matrix()`)\cr
                    #' The matrix of the independent variables.
                    X = NULL,
                    
                    #' @field Y (`matrix()`)\cr
                    #' The matrix of the dependent variables.
                    Y = NULL,
                    
                    #' @field Y_coln (`character()`)\cr
                    #' The column value labels.
                    Y_coln = NULL,
                    
                    #' @field family (`character(1)`)\cr
                    #' The distribution family of the model.
                    family = NULL,
                    
                    #' @description Initialize and generate the `DataModel` object.
                    #' @param data (`data.frame()|data.table()|table()`)\cr
                    #' Data used in the model.
                    #' @param formula (`formula(1)`)\cr
                    #' The formula used in the model.
                    #' @param family (`character(1)`)\cr
                    #' The distribution family of the model.
                    #' @param mn_base (`integer(1)`) \cr
                    #' Determine which column of the multinomial variable is set to be the base group.
                    initialize = function(data, formula, family, mn_base=1) {
                      if (!inherits(formula,"formula")) {
                        stop("fm_formula is not a formula!")
                      }
                      if (data.table::is.data.table(data)) {
                        self$data_format <- "data.table"
                      } else if (is.data.frame(data)) {
                        self$data_format <-  "data.frame"
                      } else if (is.table(data)) {
                        self$data_format <- "table"
                      } 
                      else {
                        stop("Wrong data format!")
                      }
                      self$formula <- formula
                      self$data <- data
                      self$family <- family
                      if (self$family=="multinom") {
                        y_col <- unique(model.frame(formula, data)[,1])
                        self$Y <- fastDummies::dummy_columns(model.frame(formula, data)[,1],
                                                        remove_selected_columns = T)
                        self$Y <- as.matrix(self$Y)
                        self$Y_coln <- y_col
                        if (is.numeric(mn_base)) {
                          self$Y <- self$Y[,-mn_base]
                        } else if (is.character(mn_base)) {
                          if (mn_base %in% y_col) {
                            d_ind <- which(mn_base %in% y_col)[1]
                            self$Y <- self$Y[,-d_ind]
                          } else {
                            stop("Wrong baseline value!")
                          }
                        } else {
                          stop("Wrong baseline value!")
                        }
                      } else {
                        self$Y <-  matrix(model.frame(formula, data)[,1], ncol=1)
                      }
                      self$X <- model.matrix(formula, data)
                    }
                  ), 
                  private = list(
                  )
                  )