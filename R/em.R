#' @title EM Algorithm (EM) Object
#'
#' @author Dongjie Wu
#'
#' @description A concrete R6 object for EM Algorithm.
#'
#' @name em
#'
#' @return Returns R6 object of class em.
#'
#' @export
#' 
em <- R6Class("em",
               inherit = AbstractMethod,
               public = list(        
                 #' @description
                 #' Create a new instance of this [R6] [R6::R6Class] class.
                 #' @param family `formula(1)` \cr
                 #' The distribution family to be used in the EM algorithm.
                 #' @param start `vector(1)` \cr
                 #' The vector of start values for the EM algorithm.
                 #' @return Return a R6 object of class em.
                 initialize = function(mixer, Y, X, start=NULL, constraint=NULL){
                   private$X <- X
                   private$Y <- Y
                   private$latent <- mixer$get_latent()
                   private$mixer <- mixer
                   if (!is.null(start)) {
                     private$start <- start
                   } else {
                     private$start <- private$mixer$gen_start()(private$X)
                   }
                   if (!is.null(constraint)) {
                     private$constraint <- constraint
                   } else {
                     private$constraint <- private$mixer$gen_constraint()(private$X)
                   }
                 },
                 fit = function(){
                   likelihood_func <- private$mixer$mix_ll()
                   #z <- self$estep(private$start)
                   #z <- self$cstep(z)
                   z <- vectorize_dummy(kmeans(private$Y, private$latent)$cluster)
                   theta_update <- self$mstep(likelihood_func, z, private$start)
                   theta_update <- c(coef(theta_update))
                   print(z)
                   print(theta_update)
                   convergence <- 1
                   ll_value <- 0
                   counter <- 0
                   while(convergence > 1e-4) {
                     z <- self$estep(theta_update)
                     #z <- self$cstep(z)
                     result <- self$mstep(likelihood_func, z, theta_update)
                     theta_update <- c(coef(result))
                     convergence <- abs(result$value - ll_value)
                     ll_value <- result$value
                     counter <- counter + 1
                   }
                   print(paste0("convergence after ", counter, " iterations"))
                   return(result)
                  },
                 #' @description Given the data, start values, the number
                 #' of latent classes, compute the expected value of 
                 #' the indicator variable 
                 estep = function(theta_update){
                   hidden <- private$mixer$post_pr(theta_update, private$Y, private$X)
                   return(hidden)
                 },
                 cstep = function(hidden) {
                   assign_func = function(postpr) {
                     vec <- rep(0,length(postpr))
                     vec[which.max(postpr)] <- 1
                     return(vec)
                   }
                   return(t(apply(hidden,1,assign_func)))
                 },
                 #' @description Given the data, start values, the number
                 #' of latent classes, the indicator variable, the likelihood
                 #' function, solve the maximum value of the likelihood 
                 #' function and update the parameter vectors theta.
                 mstep = function(likelihood_func, hidden, theta){
                   partial <- function(f, ...) {
                     l <- list(...)
                     function(...) {
                       do.call(f, c(l, list(...)))
                     }
                   }
                   ll <- partial(likelihood_func, d=hidden, Y=private$Y, X=private$X)
                   gr1 <- gen_gr(ll)
                   lower <-  private$constraint$lower
                   upper <-  private$constraint$upper
                   result <- optimx(theta, ll, gr=gr1,method="L-BFGS-B",
                                    lower=lower, upper=upper, hessian=T)#,
                                    #control=list(starttests=F))
                   
                   # result <- suppressWarnings(optimx(theta, ll, method="Nelder-Mead",
                   #                                    hessian=T))
                   #browser()
                   # result <- list(coefficients=newtonsys(gr1, theta)$zero)
                   return(result)
                 }
                 ),
               private = list(
                 Y = NULL,
                 X = NULL,
                 latent = 1,
                 constraint = NULL,
                 likelihood_func = NULL,
                 start = NULL,
                 mixer = NULL
               )
               )

