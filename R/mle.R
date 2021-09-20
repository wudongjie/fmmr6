#' @title Maximum Likelihood Estimation (MLE) Object
#'
#' @author Dongjie Wu
#'
#' @description A concrete R6 object for Maximum Likelihood Estimation.
#'
#' @name mle
#'
#' @return Returns R6 object of class mle.
#'
#' @export

mle <- R6Class("mle",
    inherit = AbstractMethod,
    public = list(
        #' @description
        #' Create a new instance of this [R6] [R6::R6Class] class.
        #' @param Y `matrix(1)` \cr
        #' The dependent variable.
        #' @param X `matrix(1)` \cr
        #' The independent variables.
        #' @param family `formula(1)` \cr
        #' The distribution family to be used in the maximum likehood estimation.
        #' @param method `character(1)` \cr
        #' The optimization method used in the MLE. The default is `L-BFGS-B`.
        #' @param start `matrix(1)` \cr
        #' Specify the start value for the MLE. The default is `NULL`.
        #' @return Return a R6 object of class mle
        initialize = function(Y, X, family, method="L-BFGS-B", start=NULL){
            private$family <- self$gen_family(family)
            if (is.null(start)) {
              private$start <- private$family$gen_start(Y, X)
            } else {
              private$start <- start
            }
            private$Y <- Y
            private$X <- X
            private$method <- method
        },
        #' @description 
        #' Given the `family` variable and number of classes,
        #' generate a `Family` objects.
        #' @param family `character(1)` \cr
        #' The family variable.
        gen_family = function(family) {
          if (family == "gaussian") { return(FamilyNormal$new())} 
          else if (family == "logit") { return(FamilyLogit$new())}
          else if (family == "poisson") { return(FamilyPoisson$new()) }
          else if (family == "multinom") { return(FamilyMultiNomial$new())}
          else {
            stop("Wrong family distribution!")
          }
        },
        #' @description 
        #' Generate the likelihood function given the density function.
        #' @param density_func `function(1)` \cr
        #' The density function.
        gen_ll = function(density_func) {
          ll = function(theta, Y, X) {
            if (is.matrix(Y)|is.data.frame(Y)){
              theta <- matrix(theta, ncol=ncol(Y))
            } else {
              theta <- matrix(theta, ncol=1)
            }
            return(-sum(density_func(theta, Y, X)))
          }
          return(ll)
        },
        #' @description 
        #' Fit the MLE.
        #' @param latent `integer(1)` \cr
        #' Latent class variable. Default is 1.
        #' @param algo `character(1)` \cr
        #' The algorithm used. The default is `mle`. 
        fit = function(latent=1, algo="mle", ...) {
            latent <- as.integer(latent)
            if ((is.na(latent)) | latent==0) {
              stop("Please provide the correct number of latent classes")
            }
            if (latent==1) {
              npar <- nrow(private$start)*ncol(private$start)
              likelihood_func <- self$gen_ll(private$family$gen_density())
              ll <- partial(likelihood_func, Y=private$Y, X=private$X)
              grad <- gen_gr(ll)
              constraint <- private$family$gen_constraint(private$Y, private$X)
              if (private$method == "L-BFGS-B") {
              result <- suppressWarnings(optim(private$start, ll, gr=grad,
                               #lower=constraint$lower,
                               #upper=constraint$upper,
                               method="L-BFGS-B", hessian=T))
              } else if (private$method == "Nelder-Mead") {
              result <- suppressWarnings(optim(private$start, ll,
                               method="Nelder-Mead"))
              } else if (private$method == "Newton-Raphson") {
                result <- newtonsys(grad, private$start)$zero
              }
              result$AIC <- 2 * npar + 2 * result$value
              result$BIC <- npar * log(nrow(private$X)) + 2 * result$value
              return(result)
            } 
        }
    ),
    private = list(
        family = NULL,
        start = NULL,
        Y = NULL,
        X = NULL,
        method = NULL
        )
)
