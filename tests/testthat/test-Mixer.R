test_that("test mix ll", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  theta <- c(6,8,1,5,3,1,2,4)
  y[1:36] <- 5*x1[1:36] + 3*x2[1:36] 
  y[37:100] <- 2*x1[37:100] + 4*x2[37:100]
  d1 <- matrix(1, nrow=100, ncol=1)
  d1[37:100] <- 0
  d2 <- matrix(1, nrow=100, ncol=1)
  d2[1:36] <- 0
  d <- cbind(d1,d2)
  formula <- y ~ x1 + x2
  X <- matrix(data = c(x1,x2), ncol=2)
  #browser()
  data <- data.frame(y=y,x1=x1,x2=x2)
  mix1 <- Mixer$new(family="gaussian", latent=2)
  ll <- mix1$get_ll()
  ll_expect <- d[,1]*(log(0.36) + dnorm(y, mean=5*x1+3*x2, sd=1, log=T))
  ll_expect <- ll_expect + d[,2]*(log(0.64) + dnorm(y, mean=2*x1+4*x2, sd=1, log=T))
  ll_expect <- sum(-ll_expect)
  ll_actual <- ll(theta, y, X, d)
  expect_equal(ll_actual, ll_expect)
})

test_that("test start value", {
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  normal1 <- Mixer$new(family="gaussian", latent=2)$gen_start()
  expect_true(is.function(normal1))
  start_actual <- normal1(X)
  start_expect <- c(0.5, 0.5, 1, 1, 1, 1, 1, 1)
  expect_equal(start_actual, start_expect)
  start_actual2 <- normal1(x1)
  start_expect2 <- c(0.5, 0.5, 1, 1, 1, 1)
  expect_equal(start_actual2, start_expect2)
})


test_that("test constraint", { 
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  normal1 <- Mixer$new()$gen_constraint()
  expect_true(is.function(normal1))
  con_actual <- normal1(X)
  con_expect <- list(lower=c(-Inf, -Inf, 0,-Inf,-Inf, 0,-Inf,-Inf),
                     upper=c(+Inf, +Inf, +Inf,+Inf,+Inf, +Inf,+Inf,+Inf))
  expect_equal(con_actual, con_expect)
  con_actual2 <- normal1(x1)
  con_expect2 <- list(lower=c(-Inf, -Inf, 0,-Inf, 0, -Inf),
                      upper=c(+Inf, +Inf, +Inf,+Inf, +Inf, +Inf))
  expect_equal(con_actual2, con_expect2)
})


test_that("test set family", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  y[1:30] <- 5*x1[1:30] + 3*x2[1:30] 
  y[31:100] <- 2*x1[31:100] + 4*x2[31:100]
  formula <- y ~ x1 + x2
  #browser()
  data <- data.frame(y=y,x1=x1,x2=x2)
  mix1 <- Mixer$new(family="gaussian", latent=2)
  family1 <- "gaussian"
  family2 <- c("gaussian", "poisson")
  latent1 <- 1
  latent2 <- 2
  expect_error(mix1$set_family(family1, latent1), "Only one latent class!")
  k <-mix1$set_family(family1, latent2)
  #print(k[1]==mix1$dist_list['gaussian'])
  expect_equal(length(k),latent2)
})


test_that("test posterior probability", {
  x1 <- runif(100, 0, 1)
  x2 <- runif(100, 0, 1)
  y <- runif(100)
  y[1:36] <- 5*x1[1:36] + 3*x2[1:36] 
  y[37:100] <- 2*x1[37:100] + 4*x2[37:100]
  X <- matrix(data = c(x1,x2), ncol=2)
  theta <- c(6,8,1,5,3,1,2,4)
  post_actual <- Mixer$new()$post_pr(theta, y, X)
  post_expect <-c()
  post_expect <- c(post_expect, 0.36 * dnorm(y, mean=5*x1+3*x2, sd=1))
  post_expect <- c(post_expect, 0.64 * dnorm(y, mean=2*x1+4*x2, sd=1))
  post_expect <- matrix(post_expect, ncol=2)
  post_expect <- post_expect/rowSums(post_expect)
  expect_equal(post_actual, post_expect)
})




