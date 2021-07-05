test_that("density function", {
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  Y <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  theta <- c(1,3,4)
  theta2 <- c(1,1)
  normal1 <- FamilyNormal$new()$gen_density()
  expect_true(is.function(normal1))
  normal_actual <- normal1(theta, Y, X)
  normal_expect <- dnorm(Y, mean = 3*x1 + 4*x2, sd=1, log=T)
  expect_equal(normal_expect, normal_actual)
  x1 <- matrix(x1) # ! the input x must be matrix
  normal_actual2 <- normal1(theta2, Y, x1)
  normal_expect2 <- dnorm(Y, mean = x1, sd=1, log=T)
  expect_equal(normal_actual2, normal_expect2)
})

test_that("test start value", {
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  normal1 <- FamilyNormal$new()$gen_start
  expect_true(is.function(normal1))
  start_actual <- normal1(X)
  start_expect <- c(1, 1, 1)
  expect_equal(start_actual, start_expect)
  start_actual2 <- normal1(x1)
  start_expect2 <- c(1, 1)
  expect_equal(start_actual2, start_expect2)
})


test_that("test constraint", { 
  x1 <- rnorm(1000, mean=1, sd=1)
  x2 <- rnorm(1000, mean=1, sd=1)
  X <- matrix(data = c(x1,x2), ncol=2)
  normal1 <- FamilyNormal$new()$gen_constraint
  expect_true(is.function(normal1))
  con_actual <- normal1(X)
  con_expect <- list(lower=c(0,-Inf,-Inf),
                       upper=c(+Inf,+Inf,+Inf))
  expect_equal(con_actual, con_expect)
  con_actual2 <- normal1(x1)
  con_expect2 <- list(lower=c(0,-Inf),
                      upper=c(+Inf,+Inf))
  expect_equal(con_actual2, con_expect2)
})

