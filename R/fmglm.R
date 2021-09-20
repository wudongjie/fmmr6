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
#' 
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
        #' or a vector like `c("gaussian", "poisson")`
        #' @param latent `numeric(1)` \cr
        #' The number of latent classes
        #' @param method `character(1)` \cr
        #' The estimation method to fit the fmglm
        #' @param start `matrix(1)` \cr
        #' The starting values for the fmglm
        #' @param glm_fit `Boolean(1)` \cr
        #' Whether use the `glm.fit()`, `ols.wfit()` or `nnet()` to fit the model.
        #' The default is `FALSE`.
        #' @param use_llc `Boolean(1)` \cr
        #' Whether to use the complete log-likelihood to fit the model.
        #' The default is `TRUE`.
        #' @param mn_base `integer(1)` \cr
        #' Determine which column of the multinomial variable is set to be the base group.
        #' @return Return a R6 object of class fmglm
        initialize = function(formula, data, family="gaussian", latent=2,
                              method="em", start=NULL, glm_fit=F, use_llc=T, mn_base=1) {
            # Given family, latent, model and data set a Mix object that
            # can return start, LogLikelihood, Gradient and Constraint.
            # given a Mix object, set a method variable that stores
            # a concrete Method object such as em or mle. 
            private$start <- start
            private$glm_fit <- glm_fit
            private$latent <- latent
            private$family <- family
            if (family=="multinom") {
              y_col <- unique(model.frame(formula, data)[,1])
              private$Y <- fastDummies::dummy_columns(model.frame(formula, data)[,1],
                                              remove_selected_columns = T)
              colnames(private$Y) <- y_col
              if (is.numeric(mn_base)) {
                private$Y <- private$Y[,-mn_base]
              } else if (is.character(mn_base)) {
                if (mn_base %in% y_col) {
                  d_ind <- which(mn_base %in% y_col)[1]
                  private$Y <- private$Y[,-d_ind]
                } else {
                  stop("Wrong baseline value!")
                }
              } else {
                stop("Wrong baseline value!")
              }
            } else {
              private$Y <-  model.frame(formula, data)[,1]
            }
            private$X <- model.matrix(formula, data)
            if (latent>1) {
              private$mixer <- Mixer$new(family=family, latent=latent, use_llc=use_llc)
            }

            # check the start value
            if (method == "mle") {
                private$method <- mle$new(private$Y, private$X, family, 
                                          start=private$start)
            } 
            if (method == "em") {
              private$method <- em$new(private$mixer, 
                                       Y=private$Y,
                                       X=private$X,
                                       start=private$start, glm_fit=private$glm_fit)
            }
        },
        
        #' @description 
        #' Fit the fmglm model
        #' @param algo `character(1)` \cr
        #' The algorithm used in fitting the fmglm model. 
        #' The default algorithm is `em` standing for the normal EM algorithm.
        #' One can choose from `c("em", "cem", "sem")`.
        #' `cem` is the classification EM algorithm.
        #' `sem` is the stochastic EM algorithm.
        #' @param max_iter `numeric(1)` \cr
        #' Specify the maximum number of iterations for the E-step-M-step loop.
        #' The default number is 500.
        #' @param start `character(1)` \cr
        #' Specify the starting method of the EM algorithm.
        #' Can either start from `kmeans` or `random`.
        #' `kmeans` use the K-mean methods to put samples into latent classes.
        #' `random` randomly assigns samples into latent classes.
        #' The default method is `kmeans`.
        #' @param rep `numeric(1)` \cr
        #' Specify the number of reps EM-algorithm runs.
        #' This parameter is designed for preventing the local maximum.
        #' Each rep, the EM_algorithm generates a start.
        #' It is only useful when `start` is `random`. 
        #' After all reps, the algorithm will pick the rep with maximum log likelihood.
        #' The default value is 1       
        fit = function(algo="em", max_iter=500, start="kmeans", rep=1) {
            result <- private$method$fit(algo=algo, max_iter=max_iter, start=start, rep=rep)
            private$result <- result
            return(result)
            #return(self$summary(result))
        },
        #' @description 
        #' Generate a summary for the result.
        #' @param digits `integer(1)` \cr
        #' Determine how many digits presented in the output.
        summarize = function(digits=3){
          result <- private$result
          if (is.null(result)) {
            stop("Please fit the model first.")
          }
          latent <- private$latent
          
          if (latent == 1) {
            family_group <- c(private$family)
          } else {
            family_group <- private$mixer$get_family_init()
          }
          pi <-  result$pi_vector
          pi_list <- list()
          estimates <- c(result$par)
          df <- nrow(private$X) - (length(estimates) + latent - 1)
          npar <- nrow(result$par) / latent
          ny <- 1
          if ("multinom" %in% family_group) {
              ny <- ncol(private$Y)
              df <- df*ny
          }
          kpar <- length(estimates) / ny
          hessian <- result$hessian
          sddev <- sqrt(diag(solve(hessian)))
          tvalue <- estimates/sddev
          pvalue <- pt(tvalue, df, lower.tail=FALSE)
          p_ast <- add_ast(pvalue)
          total_list <- list()

          for (k in 1:ny) {
            sum_df <-  data.frame()
            est <- result$par[,k]
            kstart <- (k-1)*kpar+1
            kend <- (k-1)*kpar+kpar
            sdv <- sddev[kstart:kend]
            tval <- tvalue[kstart:kend]
            pval <- pvalue[kstart:kend]
            past <- p_ast[kstart:kend]
            # group everything by components
            label_col <- c("Estimates", "Sd.Dev", "t-value", "P-value", "sig")
            for (l in 1:latent) {
              if (family_group[l]=='gaussian') {
                  label_row <- c("sigma",colnames(private$X))
              } else {
                  label_row <- c(colnames(private$X))
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
            if (family_group[l]=='multinom') {
              total_list[[colnames(private$Y)[k]]] <- total
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
        X = NULL,
        Y = NULL,
        family = NULL,
        latent = NULL,
        method = NULL,
        mixer = NULL,
        start = NULL,
        glm_fit = F,
        result = NULL
    )
)
