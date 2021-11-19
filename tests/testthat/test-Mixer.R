test_that("test mix ll", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  theta <- matrix(c(1,5,3,1,2,4),ncol=1)
  y[1:30] <- 5*x1[1:30] + 3*x2[1:30] 
  y[31:100] <- 2*x1[31:100] + 4*x2[31:100]
  d1 <- matrix(1, nrow=100, ncol=1)
  d1[31:100] <- 0
  d2 <- matrix(1, nrow=100, ncol=1)
  d2[1:30] <- 0
  d <- cbind(d1,d2)
  formula <- y ~ x1 + x2
  X <- matrix(data = c(x1,x2), ncol=2)
  data <- data.frame(y=y,x1=x1,x2=x2)
  mix1 <- Mixer$new(family="gaussian", latent=2)
  ll <- mix1$ll
  browser()
  ll_expect <- d[,1]*(log(0.3) + dnorm(y, mean=5*x1+3*x2, sd=1, log=T))
  ll_expect <- ll_expect + d[,2]*(log(0.7) + dnorm(y, mean=2*x1+4*x2, sd=1, log=T))
  ll_expect <- sum(-ll_expect)
  ll_actual <- ll(theta, y, X, d)
  expect_equal(ll_actual, ll_expect)
})

test_that("test start value", {
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  Y <- 3*x1+4*x2
  normal1 <- Mixer$new(family="gaussian", latent=2)$gen_start()
  expect_true(is.function(normal1))
  start_actual <- normal1(Y,X)
  start_expect <- c(6, 1)
  expect_equal(dim(start_actual), start_expect)
  start_actual2 <- normal1(Y,x1)
  start_expect2 <- c(4, 1)
  expect_equal(dim(start_actual2), start_expect2)
})


test_that("test constraint", { 
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  Y <- 3*x1+4*x2
  normal1 <- Mixer$new()$gen_constraint()
  expect_true(is.function(normal1))
  con_actual <- normal1(Y,X)
  con_expect <- list(lower=matrix(c(0,-Inf,-Inf, 0,-Inf,-Inf),ncol=1),
                     upper=matrix(c(+Inf,+Inf,+Inf, +Inf,+Inf,+Inf), ncol=1))
  expect_equal(con_actual, con_expect)
  con_actual2 <- normal1(Y,x1)
  con_expect2 <- list(lower=matrix(c(0,-Inf, 0, -Inf), ncol=1),
                      upper=matrix(c(+Inf,+Inf, +Inf, +Inf), ncol=1))
  expect_equal(con_actual2, con_expect2)
})


test_that("test set family", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  y[1:30] <- 5*x1[1:30] + 3*x2[1:30] 
  y[31:100] <- 2*x1[31:100] + 4*x2[31:100]
  formula <- y ~ x1 + x2
  data <- data.frame(y=y,x1=x1,x2=x2)
  mix1 <- Mixer$new(family="gaussian", latent=2)
  family1 <- "gaussian"
  family2 <- c("gaussian", "poisson")
  latent1 <- 1
  latent2 <- 2
  k1 <- mix1$set_family(family1, latent1)
  expect_equal(length(k1), latent1)
  k2 <-mix1$set_family(family1, latent2)
  expect_equal(length(k2),latent2)
})


test_that("test posterior probability", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  y[1:30] <- 5*x1[1:30] + 3*x2[1:30] 
  y[31:100] <- 2*x1[31:100] + 4*x2[31:100]
  X <- matrix(data = c(x1,x2), ncol=2)
  pi_vector <- c(0.3, 0.7)
  theta <- matrix(c(1,5,3,1,2,4), ncol=1)
  post_actual <- Mixer$new()$post_pr(theta, pi_vector, y, X)
  post_expect <-c()
  post_expect <- c(post_expect, 0.3 * dnorm(y, mean=5*x1+3*x2, sd=1))
  post_expect <- c(post_expect, 0.7 * dnorm(y, mean=2*x1+4*x2, sd=1))
  post_expect <- matrix(post_expect, ncol=2)
  post_expect <- post_expect/rowSums(post_expect)
  expect_equal(post_actual, post_expect)
})


test_that("test mixture of multinomial", {
  multinom <- function(x) {exp(x)/(1+sum(exp(x)))}
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  X <- matrix(data = c(x1,x2), ncol=2)
  theta1 <- matrix(c(5,4,3,2,6,1,3,2,4,5,6,2), ncol=3)
  y1 <- cbind(theta1[1,1]*x1 + theta1[2,1]*x2,
          theta1[1,2]*x1 + theta1[2,2]*x2,
          theta1[1,3]*x1 + theta1[2,3]*x2)
  y1 <-  t(apply(y1, 1, multinom))
  y2 <- cbind(theta1[3,1]*x1 + theta1[4,1]*x2,
          theta1[3,2]*x1 + theta1[4,2]*x2,
          theta1[3,3]*x1 + theta1[4,3]*x2)
  y2 <-  t(apply(y2, 1, multinom))
  rmultinom_reduce <- function(x) {rmultinom(1:3,1,x)}
  y_d1 <- t(apply(y1, 1, rmultinom_reduce))
  y_d2 <- t(apply(y2, 1, rmultinom_reduce))
  y_d <- rbind(y_d1[1:30,], y_d2[31:100,])
  data <- data.frame(y=y_d,x1=x1,x2=x2)
  mix1 <- Mixer$new(family="multinom", latent=2)
  ll <- mix1$ll
  d1 <- matrix(1, nrow=100, ncol=1)
  d1[31:100] <- 0
  d2 <- matrix(1, nrow=100, ncol=1)
  d2[1:30] <- 0
  d <- cbind(d1,d2)
  browser()
  ll_actual <- ll(theta1, y_d, X, d)
  expect_lt(ll_actual,200)
})



