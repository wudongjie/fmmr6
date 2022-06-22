#' @title Finite Mixture of Generalized Linear Models (fmglm) Object
#'
#' @author Dongjie Wu
#'
#' @description A finite mixture of generalized linear model (GLM)
#'
#' @template section_fmglm_intro
#' 
#' @name fmglm
#'
#' @return Returns R6 object of class fmglm.
#'
#' 
#' @export

fmglm <- R6Class("fmglm",
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
        #' @param family (`character(1)|character()`) \cr
        #' The distribution family which can be either a string like "gaussian".
        #' or a vector like `c("gaussian", "gaussian")`.
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
        #' @param mn_base (`integer(1)`) \cr
        #' Determine which column of the multinomial variable is set to be the base group.
        #' @param constraint (`matrix()`) \cr
        #' The constraint matrix.
        #' @return Return a R6 object of class fmglm

        initialize = function(formula, data, data_str="default", 
                              data_var=NULL,
                              family="gaussian", latent=2,
                              method="em", start=NULL, optim_method="base", 
                              concomitant=NULL, use_llc=TRUE, 
                              mn_base=1,constraint=matrix(1)) {
            self$start <- start
            self$optim_method <- optim_method
            self$latent <- latent
            self$family <- family
            #browser()
            self$data_model <- DataModel$new(data, formula, 
                                             family=family, 
                                             mn_base=mn_base, 
                                             data_str=data_str,
                                             data_var=data_var,
                                             concomitant=concomitant)
            self$constraint <- constraint
            self$concomitant <- concomitant
            # check the start value
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
        #' @param verbose (`boolean(1)`) \cr
        #' Print the converging log-likelihood for all steps.   
        fit = function(algo="em", max_iter=500, start="random", rep=1, verbose=F) {
            private$.result <- self$method$fit(algo=algo, max_iter=max_iter, start=start, rep=rep, verbose=verbose)
            return(private$.result)
            #return(self$summary(result))
        },
        
        #' @description 
        #' Generate a summary for the result.
        #' @param digits (`integer(1)`) \cr
        #' Determine how many digits presented in the output.
        summarize = function(digits=3){
          result <- private$.result
          if (is.null(result)) {
            stop("Please fit the model first.")
          }
          latent <- self$latent
          family_group <- self$family

          pi <-  result$pi_vector
          pi_list <- list()
          npar <- nrow(result$par) / latent
          ny <- ncol(self$data_model$Y)
          result$df <- ny*nrow(self$data_model$Y) - sum(result$par != 0)

          kpar <- length(result$par) / ny
          result$sddev <- sqrt(diag(solve(result$hessian)))
          result$tvalue <- result$par/result$sddev
          #result$pvalue <- pt(result$tvalue, result$df, lower.tail=FALSE)
          result$pvalue <- 2*pt(-abs(result$tvalue), result$df)
          result$p_ast <- add_ast(result$pvalue)
          total_list <- list()

          for (k in 1:ny) {
            sum_df <-  data.frame()
            est <- result$par[,k]
            kstart <- (k-1)*kpar+1
            kend <- (k-1)*kpar+kpar
            sdv <- result$sddev[kstart:kend]
            tval <- result$tvalue[kstart:kend]
            pval <- result$pvalue[kstart:kend]
            past <- result$p_ast[kstart:kend]
            # group everything by components
            label_col <- c("Estimates", "Sd.Dev", "t-value", "P-value", "sig")
            for (l in 1:latent) {
              if (family_group=='gaussian') {
                  label_row <- c("sigma",colnames(self$data_model$X))
              } else {
                  label_row <- c(colnames(self$data_model$X))
              }
                
              start_v <- ((l-1)*npar+1)
              end_v <- ((l-1)*npar+npar)
              c_label <- paste("Comp", l, sep=".")
              pi_list[[c_label]]=pi[l]
              content_l <- list(
                "Estimates" = round(est[start_v:end_v],digits),
                "Std.Error" = round(sdv[start_v:end_v],digits),
                "t-value" = round(tval[start_v:end_v],digits),
                "P-value" = round(pval[start_v:end_v],digits),
                "sig " = past[start_v:end_v]
              )
              label <- sapply(label_row, 
                              function(x){
                                return(paste(l,x,sep="."))
                              })
              content_l <- data.frame(content_l, row.names=label)
              sum_df <- rbind(sum_df, content_l)
            }
            logLik <- - result$value
            total<- list(coefficients=sum_df)
            if (family_group=='multinom') {
              total_list[[colnames(self$data_model$Y)[k]]] <- total
            } else {
              total_list[['coefficients']] <- sum_df
            }
          }
          total_list[['pi']] <- pi_list
          total_list[['logLik']] <- logLik
          total_list[['AIC']] <- result$AIC
          total_list[['BIC']] <- result$BIC
          return(total_list)
        }
    ),
    private = list(
        .result = NULL
    )
)
