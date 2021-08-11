test_that("test logit", {
  formula <- private~age+educ+income+female
  formula2 <- cbind(private, 1 - private)~age+educ+income+female
  formula_t <- Survived~factor(Pclass)+Fare+Sex
  formula_t2 <- cbind(Survived, 1 - Survived)~Pclass+Fare+Sex
  latent <- 2
  flex_result <- flexmix::flexmix(formula2, gsem_mixture, k=2,
                                  model = FLXMRglm(family = "binomial"))
  print(flex_result)
  print(flex_result@logLik)
  print(parameters(flex_result, component=1))
  print(parameters(flex_result, component=2))
  Y <- model.frame(formula, gsem_mixture)[,1]
  X <- model.matrix(formula, gsem_mixture)
  print(size(X))
  #print(X)
  mixt_result <- mixtools::logisregmixEM(Y, X, beta=matrix(0.9,5,2), k=2, addintercept = F)
  print(summary(mixt_result))
  model <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
                     method="em")
  browser()
  result <- model$fit()
  print(result)
  
})
#test_that("multiplication works", {
#   data("NPreg")
#   latent <- 2
#   NPreg$x2 <- (NPreg$x)^2
#   formula <- yn~x+x2
#   formula2 <- yp~x
#   print(lm(formula, NPreg))
#   # TODO: TEST WITHOUT THE START
#   model1 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
#             method="em")
#   model2 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
#                       method="em")
#   flex_result <- flexmix::flexmix(formula, NPreg, k=2)
#   flex_result2 <- flexmix::flexmix(formula2, NPreg, k=2,
#                                   model = FLXMRglm(family = "poisson"))
#   X <- matrix(c(NPreg$x,NPreg$x2),ncol = 2)
#   mixt_result <- mixtools::regmixEM(NPreg$yn, X, k=2)
#   mixt_result2 <- mixtools::poisregmixEM(NPreg$yp,NPreg$x, k=2)
#   result1 <- model1$fit()
#   result2 <- model2$fit()
#   print(flex_result@logLik)
#   print(flex_result2@logLik)
#   print(result1)
#   print(result2)
#   print(summary(mixt_result))
#   print(summary(mixt_result2))
# })
# 
# test_that("test poisson", {
#   latent <- 2
#   formula <- drvisits~private+medicaid+chronic
#   flex_result <- flexmix::flexmix(formula, gsem_mixture, k=2,
#                                   model = FLXMRglm(family = "poisson"))
#   print(summary(flex_result))
#   print(parameters(flex_result, component=1))
#   print(parameters(flex_result, component=2))
#   
#   model <- fmglm$new(formula, gsem_mixture, family="poisson", latent=2,
#                      method="em")
#   result1 <- model$fit()
#   print(result1)
# 
#   
# })
