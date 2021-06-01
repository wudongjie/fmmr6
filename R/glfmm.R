#' @title Generalized Linear Finite Mixture Models (GLFMMs) Object
#'
#' @author Dongjie Wu
#'
#' @description A finite mixture of generalized linear model (GLM)
#'
#' @name glfmm
#'
#' @return Returns R6 object of class glfmm.
#'
#' @export

glfmm <- R6Class("glfmm",
    public = list(
        #' @description
        #' Create a new instance of this [R6] [R6::R6Class] class.
        #' @param formula `formula(1)` \cr
        #' The formula of the model to fit in the glfmm
        #' @param data `data(1)` \cr
        #' The Data used in the glfmm
        #' @param link `character(1)` \cr
        #' The link function in the glfmm
        #' @param latent `numeric(1)` \cr
        #' The number of latent classes
        #' @param method `character(1)` \cr
        #' The estimation method to fit the glfmm
        #' @param start `matrix(1)` \cr
        #' The starting values for the glfmm
        #' @param family `character(1)` \cr
        #' The distribution family
        #' @param summary `function(1)` \cr
        #' Print the detailed model-fitting results
        #' @param fit `function(1)` \cr
        #' Fit the model
        #' @param predict `function(1)` \cr
        #' Use the model to predict the dependent variable
        initialize = function(formula, data, link="identity", latent=1,
                              method="mle", start=NULL,
                              family="gaussian") {
            private$formula <- formula
            private$Y <- as.matrix(data[all.vars(formula)[1]])
            print(private$Y)
            private$X <- as.matrix(data[all.vars(formula)[-1]])
            private$data <- data
            private$link <- link
            private$latent <- latent
            if (method == "mle") {
                private$method <- mle$new(private$Y, private$X)
            }
            private$start <- start
            private$family <- family

        },
        summary = function() {
            print("This is the summary")
        },
        fit = function() {
            return(private$method$fit())
        },
        predict = function() {
            print("This is the predict")
        }
    ),
    private = list(
        formula = "~",
        X = NULL,
        Y = NULL,
        data = NULL,
        link = "identity",
        latent = 1,
        method = NULL,
        start = NULL,
        family = "gaussian"
    )
)
