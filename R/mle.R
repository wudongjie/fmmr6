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
        #' @param family `formula(1)` \cr
        #' The distribution family to be used in the maximum likehood estimation.
        #' @return Return a R6 object of class mle
        initialize = function(Y, X, family, method="L-BFGS-B", start=NULL){
            private$family <- self$gen_family(family)
            if (is.null(start)) {
              private$start <- private$family$gen_start()(X)
            } else {
              private$start <- start
            }
            private$Y <- Y
            private$X <- X
            private$method <- method
        },
        gen_family = function(family) {
          if (family == "gaussian") { return(FamilyNormal$new())} 
          else if (family == "logit") { return(FamilyLogit$new())}
          else if (family == "poisson") { return(FamilyPoisson$new()) }
          else {
            stop("Wrong family distribution!")
          }
        },
        gen_ll = function(density_func) {
          ll = function(theta, Y, X) {
            return(-sum(density_func(theta, Y, X)))
          }
          return(ll)
        },
        fit = function(latent=1) {
            # test a linear regression mle
            # return(solve((t(private$X) %*% private$X)) %*% (t(private$X) %*% private$Y))
            # compute the log likelihood function
            #objective <- Minimize(private$likelihood_func)
            # compute the first derivative of the log likelihood function for each parameter
            #problem <- Problem(objective)
            # solve the first derivative of the log likelihood function = 0
            #tol <- 0.0005
          
            ## If family is a vector, then it is mixed with different families
            ## of distribution.
            latent <- as.integer(latent)
            if ((is.na(latent)) | latent==0) {
              stop("Please provide the correct number of latent classes")
            }
            if (latent==1) {
              likelihood_func <- self$gen_ll(private$family$gen_density())
              ll <- partial(likelihood_func, Y=private$Y, X=private$X)
              grad <- gen_gr(ll)
              constraint <- private$family$gen_constraint(private$X)
              if (private$method == "L-BFGS-B") {
              result <- optimx(private$start, ll, gr=grad,
                               lower=constraint$lower,
                               upper=constraint$upper,
                               method="L-BFGS-B")
              } else if (private$method == "Nelder-Mead") {
              result <- optimx(private$start, ll,
                               method="Nelder-Mead")
              } else if (private$method == "Newton-Raphson") {
                result <- newtonsys(grad, private$start)$zero
              }
              #result <- lbfgs::lbfgs(likelihood_func, grad, private$start)
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
