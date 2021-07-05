#' @title Finite Mixture of Generalized Linear Models (fmglm) Object
#'
#' @author Dongjie Wu
#'
#' @description A finite mixture of generalized linear model (GLM)
#'
#' @name fmglm
#'
#' @return Returns R6 object of class fmglm.
#'
#' @example fmglm$new(formula, data, family="gaussian", method="em",
#'                    latent=2)
#' @export

fmglm <- R6Class("fmglm",
    inherit = fmmr6,
    public = list(
        #' @description
        #' Create a new instance of this [R6] [R6::R6Class] class.
        #' @param formula `formula(1)` \cr
        #' The formula/expression of the model to fit in the fmglm
        #' @param data `data(1)` \cr
        #' The Data used in the fmglm
        #' @param family `character(1)` \cr
        #' The distribution family which can be either a string like "gaussian"
        #' or a vector like c("gaussian", "poisson")
        #' @param latent `numeric(1)` \cr
        #' The number of latent classes
        #' @param method `character(1)` \cr
        #' The estimation method to fit the fmglm
        #' @param start `matrix(1)` \cr
        #' The starting values for the fmglm

        #' @return Return a R6 object of class fmglm
        initialize = function(formula, data, family="gaussian", latent=2,
                              method="em", start=NULL) {
            # Given family, latent, model and data set a Mix object that
            # can return start, LogLikelihood, Gradient and Constraint.
            
            
            # given a Mix object, set a method variable that stores
            # a concrete Method object such as em or mle. 
            private$start <- start
            private$Y <-  model.frame(formula, data)[,1]
            private$X <- model.matrix(formula, data)
            private$mixer <- Mixer$new(family=family, latent=latent)
            #private$latent <- private$mixer$get_latent()
            # check the start value
            
            
            if (method == "mle") {
                #private$method <- mle$new(private$Y, private$X)
                private$method <- mle$new(private$mixer, 
                                          start=private$start)
            } 
            if (method == "em") {
              private$method <- em$new(private$mixer, 
                                       Y=private$Y,
                                       X=private$X,
                                       start=private$start)
            }
        },
        
        #' @description Fit the fmglm model
        fit = function() {
            result <- private$method$fit()
            return(self$summary(result))
        }, 
        summary = function(result, digits=3){
          #' Call:
          #' 
          #' Coefficients:
          #'             Estimate    Std.Error   t value   Pr(>t)
          #' Comp.1
          #' Intercept
          #' x1
          #' x2    
          #' Comp.2
          #' Intercept
          #' x1
          #' x2              
          #'       Comp. 1        Comp. 2
          #' pi     0.300          0.700
          #' sigma  1.002          1.010
          #' ---
          #'  AIC
          #'  BIC
          #'  Log-Likelihood
          #' ---
          #'
          if (is.null(result)) {
            stop("Please fit the model first.")
          }
          latent <- private$mixer$get_latent()
          estimates <- c(coef(result))
          npar <- (length(estimates) - latent + 1) / latent 
          hessian <- attr(result,"details")[,"nhatend"][[1]]
          sddev <- diag(solve(hessian))
          tvalue <- estimates/sddev
          pvalue <- pt(tvalue, (nrow(private$X)-1), lower.tail=FALSE)
          p_ast <- add_ast(pvalue)  
          pi <-  estimates[1:(latent-1)]
          pi <- c(pi, 1-sum(pi))
          pi_list <- list()
          # group everything by components
          sum_df <-  data.frame()
          label_col <- c("Estimates", "Sd.Dev", "t-value", "P-value", "sig")
          family_group <- private$mixer$get_family_init()
          for (l in 1:latent) {
            if (family_group[l]=='gaussian') {
                label_row <- c("sigma",colnames(private$X))
            } else {
                label_row <- c(colnames(private$X))
            }
              
            start_v <- ((l-1)*npar+latent)
            end_v <- ((l-1)*npar+latent+npar-1)
            c_label <- paste("Comp", l, sep=".")
            pi_list[[c_label]]=pi[l]
            content_l <- list(
              "Estimates" = round(estimates[start_v:end_v],digits),
              "Sd.Dev" = round(sddev[start_v:end_v],digits),
              "t-value" = round(tvalue[start_v:end_v],digits),
              "P-value" = round(pvalue[start_v:end_v],digits),
              "sig " = p_ast[start_v:end_v]
            )
            label <- sapply(label_row, 
                            function(x){
                              return(paste(l,x,sep="."))
                            })
            content_l <- data.frame(content_l, row.names=label)
            sum_df <- rbind(sum_df, content_l)
          }
          #colnames(sum_df) <- label_col
          total <- list(Coefficients=sum_df, pi=pi_list)
          return(total)
        }
    ),
    private = list(
        X = NULL,
        Y = NULL,
        method = NULL,
        mixer = NULL,
        start = NULL,
        result = NULL
    )
)
