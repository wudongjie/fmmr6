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
    inherit = abstract_method,
    public = list(
        initialize = function(Y, X){
            private$Y <- Y
            private$X <- X
        },
        fit = function() {
            # test a linear regression mle
            return(solve((t(private$X) %*% private$X)) %*% (t(private$X) %*% private$Y))
        }
    ),
    private = list(
        Y = NULL,
        X = NULL
    )
)
