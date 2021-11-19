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
                    formula = NULL,
                    data_format = "data.table",
                    data = NULL,
                    X = NULL,
                    Y = NULL,
                    family = NULL,
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
                        colnames(self$Y) <- y_col
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