test_that("test fit", {
  x1 <- rnorm(1000, mean=3, sd=1)
  x2 <- rnorm(1000, mean=5, sd=2)
  e <- rnorm(1000, mean=0, sd=1)
  
  y <- 2 * x1 + 3 * x2 + e
  data1 <- data.frame(list(y=y,x1=x1,x2=x2))
  X <- matrix(data = c(x1,x2), ncol=2)
  y2 = 4 * x1 + 5 * x2 + 5 + e
  data2 <- data.frame(list(y=y2,x1=x1,x2=x2))
  
  y3 = 4 * x1 + 1 * x2 + 2 * x1 * x2 + 1 + e
  data3 <- data.frame(list(y=y3,x1=x1,x2=x2))
  x3 <- x1*x2
  X3 <- matrix(data = c(x1,x2,x3), ncol=3)
  lm1 <- lm(y~x1+x2,rand_lm_data)
  print(lm1$coefficients)
  mod1 <- mle$new(y, X, family="gaussian", start=c(1,2,3))$fit()
  mod1_n <- mle$new(y, X, family="gaussian", 
                    start=c(1,2,3), method="Nelder-Mead")$fit()
  mod1_nr <-  mle$new(y, X, family="gaussian", 
                      start=c(1,2,3), method="Newton-Raphson")$fit()
  #print(mod1)
  #print(mod1_n)
  print(mod1_nr)
  mod2 <- mle$new(y2, X, family="gaussian", start=c(1,2,3))$fit()
  mod2_n <- mle$new(y2, X, family="gaussian", start=c(1,2,3), method="Nelder-Mead")$fit()
  mod2_nr <-  mle$new(y2, X, family="gaussian", 
                      start=c(1,2,3), method="Newton-Raphson")$fit()
  print(mod2_nr)
  #print(mod2_n)
  #print(mod2_nr)
  mod3 <- mle$new(y3, X3, family="gaussian", start=c(1,2,3,4))$fit()
  mod3_n <- mle$new(y3, X3, family="gaussian", 
                    start=c(1,2,3,4), method="Nelder-Mead")$fit()
  mod3_nr <-  mle$new(y3, X3, family="gaussian", 
                      start=c(1,2,3,4), method="Newton-Raphson")$fit()
  #print(mod3)
  #print(mod3_n)
  print(mod3_nr)
})


# test_that("test mix fit", {
#   data("NPreg")
#   print(NPreg)
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   Y <-  model.frame(formula, NPreg)[,1]
#   X <- model.matrix(formula, NPreg)
#   lfn <- FamilyNormal$new(Y, X, latent=2)
#   theta <- c(0.5,3,0.2,1,3,5,5,2,2)
#   mle1 <- mle$new(lfn, start=theta)
#   browser()
#   mod1 <- mle1$fit(latent=2)
#   mod2 <- flexmix::flexmix(yn~x+x2, data=NPreg, k=2)
#   print(mod1)
#   print(parameters(mod2, component=1))
#   print(parameters(mod2, component=2))
# })