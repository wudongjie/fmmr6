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
                    
                    #' @template param_data_str
                    data_str = "default",
                    
                    #' @field data (`data.frame()|data.table()|table()`)\cr
                    #' Data used in the model.
                    data = NULL,
                    
                    #' @field X (`matrix()`)\cr
                    #' The matrix of the independent variables.
                    X = NULL,
                    
                    #' @field Y (`matrix()`)\cr
                    #' The matrix of the dependent variables.
                    Y = NULL,
                    
                    #' @field Z (`matrix()`)\cr
                    #' The matrix of the concomitant variables.
                    Z = NULL,
                    
                    #' @field Y_coln (`character()`)\cr
                    #' The column value labels.
                    Y_coln = NULL,
                    
                    #' @field data_var (`list()`)\cr
                    #' A list for identifying variables in the data.
                    data_var = NULL,
                    
                    #' @field family (`character(1)`)\cr
                    #' The distribution family of the model.
                    family = NULL,
                    
                    #' @field concomitant (`formula(1)`)\cr
                    #' The formula to model the concomitant model.
                    #' The default value is NULL.
                    concomitant = NULL,
                    
                    #' @field X_ex (`matrix()`)\cr
                    #' The extended data for X.
                    X_ex = NULL,
                    #' @description Initialize and generate the `DataModel` object.
                    #' @param data (`data.frame()|data.table()|table()`)\cr
                    #' Data used in the model.
                    #' @param formula (`formula(1)`)\cr
                    #' The formula used in the model.
                    #' @param family (`character(1)`)\cr
                    #' The distribution family of the model.
                    #' @template param_data_str
                    #' @param mn_base (`integer(1)`) \cr
                    #' Determine which column of the multinomial variable is set to be the base group.
                    #' @param concomitant (`formula(1)`)\cr
                    #' The formula to model the concomitant model.
                    #' The default value is NULL.
                    
                    initialize = function(data, formula, family, data_str="default",
                                          data_var=NULL, 
                                          mn_base=1, concomitant=NULL, group=NULL) {
                      # param check
                      if (!inherits(formula,"formula")) {
                        stop("fm_formula is not a formula!")
                      } else {
                        self$formula <- formula
                      }
                      self$data_var <- list(freq="freq",obs="obs",
                                            t="t",alt="alt",chosen="chosen")
                      if (!is.null(data_var)) {
                        if (is.list(data_var)) {
                        for (n in names(data_var)) {
                          self$data_var[[n]] <- data_var[[n]]
                        } }
                        else {
                          stop("data_var should be either NULL or a list.")
                        }
                      }
                      # Data Validation
                      if (data_str=="default") {
                      } else if (data_str=="freq_tab") {
                        if (!(self$data_var$freq %in% colnames(data))) {
                          stop("Please specify the correct frequency variable!")
                        }
                      } else if (data_str=="long") {
                        if (!(self$data_var$obs %in% colnames(data))) {
                          stop("Please specify the `obs` variable!")
                        }
                        if (!(self$data_var$t %in% colnames(data))) {
                          stop("Please specify the `t` variable!")
                        }
                      } else if (data_str=="wide") {
                        
                      } else if (data_str=="longc") {
                        if (!(self$data_var$chosen %in% colnames(data))) {
                          stop("Please specify the `chosen` variable!")
                        }
                        if (!(self$data_var$alt %in% colnames(data))) {
                          stop("Please specify the `alt` variable!")
                        }
                        if (!(self$data_var$obs %in% colnames(data))) {
                          stop("Please specify the `obs` variable!")
                        }
                      } else if (data_str=="widec") {
                        
                      } else {
                        stop("Wrong data_str!")
                      }

                      self$data <- data
                      self$family <- family
                      self$concomitant <- concomitant
                      if (!is.null(self$concomitant)) {
                        if (!inherits(self$concomitant,"formula")) {
                          stop("concomitant is not a formula!")
                        }
                        self$Z <- model.matrix(self$concomitant, data)
                      } 
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
                      if (self$family=="clogit") {
                        formula_ex <- update(formula, reformulate(c(".", self$data_var$obs)))
                        self$X_ex <- model.matrix(formula_ex, data)
                      }
                    },
                    data_to_table = function() {
                      self$data <- self$ind_to_table('chosen', c('x','g'), self$data, 'alt')
                    },
                    data_to_ind = function() {
                      self$data <- self$table_to_ind('y', c('x','g'), self$data, 'obs')
                    },
                    table_to_ind = function(y, x, data, obs){
                      data_ex <- data[rep(1:nrow(data), data[[obs]]),]
                      
                      ## Dummy variables
                      data_ex <- data_ex[c(y, x)]
                      data_ex <- fastDummies::dummy_cols(data_ex, select_columns=x)
                      l <- unique(data[[y]])
                      data_ex$obs <- 1:nrow(data_ex)
                      data_ex <- data_ex[rep(1:nrow(data_ex), l[length(l)]),]
                      data_ex <- data_ex[order(data_ex$obs),]
                      data_ex$alt <- rep(l, nrow(data_ex)/length(l))
                      data_ex$chosen <- data_ex$alt == data_ex[[y]]
                      data_ex <- fastDummies::dummy_cols(data_ex, select_columns="alt")
                      data_ex$chosen <- as.numeric(data_ex$chosen)
                      return(data_ex)
                    },
                    ind_to_table = function(chosen, x, data, alt){
                      browser()
                      data_ex <- data[data[[chosen]]==1,]
                      data_ex <- as.data.frame(table(data[,c(alt,x)]))
                      colnames(data_ex) <- c("y", x, "obs") 
                      return(data_ex)
                    }
                  ), 
                  private = list(
                  )
                  )