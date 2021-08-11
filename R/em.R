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
                 fit = function(algo="em"){
                   likelihood_func <- private$mixer$mix_ll()
                   z <- vectorize_dummy(kmeans(cbind(private$Y,private$X), 
                                               private$latent)$cluster)
                   theta_update <- self$mstep(likelihood_func, z, private$start)
                   #theta_update <- c(coef(theta_update))
                   theta_update <- theta_update$par
                   convergence <- 1
                   ll_value <- 0
                   counter <- 0
                   while(convergence > 1e-4) {
                     z <- self$estep(theta_update)
                     if (algo=='cem') {
                       z <- self$cstep(z)
                     } else if (algo=='sem') {
                       z <- self$sstep(z)
                     }
                     result <- self$mstep(likelihood_func, z, theta_update)
                     #theta_update <- c(coef(result))
                     theta_update <- result$par
                     #convergence <- abs(result$value - ll_value)
                     #ll_value <- result$value
                     convergence <- abs(result$objective - ll_value)
                     ll_value <- result$objective
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
                 sstep = function(hidden) {
                   assign_func = function(postpr) {
                     vec <- rmultinom(1:length(postpr), size=1,prob=postpr)
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
                   #result <- optimx(theta, ll, method="L-BFGS-B", hessian=T)
                                    #lower=lower, upper=upper, hessian=T)#,
                                    #control=list(starttests=F))
                   
                   #result <- suppressWarnings(optimx(theta, ll, method="Nelder-Mead",
                   #                                  hessian=T))
                   # result <- list(coefficients=newtonsys(gr1, theta)$zero)
                   # result <- optim(theta, ll, gr=gr1, method="Nelder-Mead")
                   # result <- suppressWarnings(optim(theta, ll, gr=gr1, method="L-BFGS-B",
                   #                                 hessian=T)) 
                   result <- suppressWarnings(nlminb(theta, ll, gradient=gr1, control=list(factr=1e-12, maxit=1000)))
                                                    #control = list(parscale=c(1,1,colMeans(private$X), colMeans(private$X)))))
                                   #control = list(parscale = c(1,1,1,colMeans(private$X),1,colMeans(private$X))))
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

