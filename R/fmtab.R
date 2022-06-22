#' @title Finite Mixture of Contingency Table Analysis (fmtab) Object
#'
#' @author Dongjie Wu
#'
#' @description A finite mixture of models related to contingency table analysis
#'
#' @name fmtab
#'
#' @return Returns R6 object of class fmtab.
#'
#' 
#' @export

fmtab <- R6Class("fmtab",
                 inherit = fmmr6,
                 public = list(
                   #' @field data_model (`DataModel()`) \cr
                   #' The DataModel Object that stores the data using in the fmmr6.
                   data_model = NULL,
                   
                   #' @field family (`character(1)|character()`) \cr
                   #' The distribution family which can be either a string like "gaussian"
                   #' or a vector like `c("gaussian", "gaussian")`.
                   family = NULL,
                   
                   #' @field latent (`integer(1)`) \cr
                   #' The number of latent classes.
                   latent = NULL,
                   
                   #' @field method (`character(1)`) \cr
                   #' The estimation method to fit the fmglm.
                   method = NULL,
                   
                   #' @field start (`matrix()`) \cr
                   #' The starting values for the fmglm.
                   start = NULL,
                   
                   #' @field constraint (`matrix()`) \cr
                   #' The constraint matrix.
                   constraint = NULL,
                   
                   #' @field concomitant (`formula(1)`)\cr
                   #' The formula to model the concomitant model.
                   #' The default value is NULL.
                   concomitant = NULL,
                   
                   #' @field optim_method (`character(1)`) \cr
                   #' The optimization method to use to fit the model.
                   optim_method = NULL,
                   #' @description
                   #' Create a new instance of this [R6] [R6::R6Class] class.
                   #' @param formula (`formula(1)`) \cr
                   #' The formula/expression of the model to fit in the fmglm.
                   #' @param data (`data.frame()`) \cr
                   #' The Data used in the fmglm.
                   #' @template param_data_str
                   #' @param latent (`integer(1)`) \cr
                   #' The number of latent classes.
                   #' @param method (`character(1)`) \cr
                   #' The estimation method to fit the fmglm.
                   #' @param start (`matrix()`) \cr
                   #' The starting values for the fmglm.
                   #' @param optim_method (`character(1)`) \cr
                   #' The optimization method to use to fit the model.
                   #' The default is `base`.
                   #' @param concomitant (`formula(1)`) \cr
                   #' The formula for the concomitant model. E.g. `~ z1 + z2 + z3`.
                   #' @param use_llc (`boolean(1)`) \cr
                   #' Whether to use the complete log-likelihood or the normal log-likelihood.
                   #' The default is `TRUE`.
                   #' @param constraint (`matrix()`) \cr
                   #' The constraint matrix.
                   #' @return Return a R6 object of class fmglm
                   
                   initialize = function(formula, data, data_str="longc", data_var=NULL,
                                         model="loglinear", latent=2,
                                         method="em", start=NULL, optim_method="base", 
                                         concomitant=NULL, constraint=matrix(1)) {
                     self$start <- start
                     self$optim_method <- optim_method
                     self$latent <- latent
                     self$family <- family

                     self$constraint <- constraint
                     self$concomitant <- concomitant
                     
                     self$data_model <- DataModel$new(data, formula, 
                                                      data_str=data_str,
                                                      data_var=data_var,
                                                      family=family, 
                                                      mn_base=mn_base,
                                                      concomitant=concomitant)
                     # check if data structure matches with model
                     if (!is.null(self$concomitant)) {
                       if (data_str == 'freq_tab') {
                         print("Data format is frequency table. Changed to longc.")
                         self$data_model <- self$data_model$data_to_ind()
                       }
                     }
                     if (model=='loglinear') {
                       if (data_str == 'longc') {
                         print("Changed data format to table for log linear model.")
                         self$data_model <- self$data_model$data_to_tab()
                       }
                     }
                     if (model=='clogit') {
                       if (data_format == 'tab') {
                         print("Data format is table. Changed to longc.")
                         self$data_model <- self$data_model$data_to_ind()        
                       }
                     }
                     
                     
                     if (method == "mle") {
                       self$method <- mle$new(self$latent, self$data_model,
                                              start=self$start, optim_method=self$optim_method, 
                                              use_llc=use_llc, constraint=self$constraint,
                                              concomitant=self$concomitant)
                     } 
                     if (method == "em") {
                       self$method <- em$new(self$latent, self$data_model,
                                             start=self$start, optim_method=self$optim_method, 
                                             use_llc=use_llc, constraint=self$constraint,
                                             concomitant=self$concomitant)
                     }
                   },
                   
                   #' @description 
                   #' Fit the fmglm model
                   #' @param algo (`character(1)`) \cr
                   #' The algorithm used in fitting the fmglm model. 
                   #' The default algorithm is `em` standing for the normal EM algorithm.
                   #' One can choose from `c("em", "cem", "sem")`.
                   #' `cem` is the classification EM algorithm.
                   #' `sem` is the stochastic EM algorithm.
                   #' @param max_iter (`integer(1)`) \cr
                   #' Specify the maximum number of iterations for the E-step-M-step loop.
                   #' The default number is 500.
                   #' @param start (`character(1)`) \cr
                   #' Specify the starting method of the EM algorithm.
                   #' Can either start from `kmeans` or `random`.
                   #' `kmeans` use the K-mean methods to put samples into latent classes.
                   #' `random` randomly assigns samples into latent classes.
                   #' The default method is `kmeans`.
                   #' @param rep (`integer(1)`) \cr
                   #' Specify the number of reps EM-algorithm runs.
                   #' This parameter is designed for preventing the local maximum.
                   #' Each rep, the EM_algorithm generates a start.
                   #' It is only useful when `start` is `random`. 
                   #' After all reps, the algorithm will pick the rep with maximum log likelihood.
                   #' The default value is 1       
                   fit = function(algo="em", max_iter=500, start="random", rep=1) {
                     private$.result <- self$method$fit(algo=algo, max_iter=max_iter, start=start, rep=rep)
                     return(private$.result)
                     #return(self$summary(result))
                   },
                   
                   #' @description 
                   #' Generate a summary for the result.
                   #' @param digits (`integer(1)`) \cr
                   #' Determine how many digits presented in the output.
                   summarize = function(digits=3){
                     return(0)
                   }
                 ),
                 private = list(
                   .result = NULL
                 )
)
