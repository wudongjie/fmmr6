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
            theta <- private$.start
            npar <- self$latent + nrow(theta) * ncol(theta) - 1
            if (self$data_model$family=="clogit") {
              hidden <- matrix(1, nrow=length(unique(self$data_model$X_ex[,ncol(self$data_model$X_ex)])), 
                               ncol=self$latent)
              ll <- partial(private$.likelihood_func, d=hidden, 
                            Y=self$data_model$Y, X=self$data_model$X_ex,
                            latent=self$latent, family=self$data_model$family, isLog=private$.use_llc, constraint=self$constraint)
            } else {
              hidden <- matrix(1, nrow=nrow(self$data_model$Y), ncol=self$latent)
              ll <- partial(private$.likelihood_func, d=hidden, 
                          Y=self$data_model$Y, X=self$data_model$X,
                          latent=self$latent, family=self$data_model$family, isLog=private$.use_llc, constraint=self$constraint)
            }
            gr <- gen_gr(ll)
            pi_vector = colSums(hidden)/nrow(hidden)
            sel_optim <- private$dist_list[[self$optim_method]]
            init_optim <- eval(sel_optim)$new()
            result <- init_optim$fit(self$data_model, theta, ll, gr, hidden, pi_vector,
                                     npar, self$latent, self$data_model$family)
            return(result)
        }
    )
)
