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
    inherit = EstimMethod,
    public = list(
        #' @description 
        #' Fit the MLE.
        #' @param algo (`character(1)`) \cr
        #' The algorithm used. The default is `mle`.
        #' @param ... (`list()`) \cr
        #' Other related parameters. 
        fit = function(algo="mle", ...) {
            latent <- as.integer(private$latent)
            if (private$glm_fit) {
                result <- OptimGLM$new()$fit(private$X, private$Y, latent, family="gaussian", weights=NULL)
            } else {
            if ((is.na(latent)) | latent==0) {
              stop("Please provide the correct number of latent classes")
            }
            npar <- nrow(private$start)*ncol(private$start)
            likelihood_func <- private$mixer$get_ll()
            ll <- partial(likelihood_func, Y=private$Y, X=private$X)
            grad <- gen_gr(ll)
            result <- suppressWarnings(optim(private$start, ll, gr=grad,
                       #lower=constraint$lower,
                       #upper=constraint$upper,
                       method="L-BFGS-B", hessian=T))
            result$AIC <- 2 * npar + 2 * result$value
            result$BIC <- npar * log(nrow(private$X)) + 2 * result$value
            }
            return(result)
        }
    )
)
