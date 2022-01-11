test_that("multiplication works", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula <- yn~x+x2
  model_ols <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1)
  result_ols1 <- model_ols$fit()
  # print(model_ols$summarize())
  # model_ols2 <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1, optim_method="glm")
  # result_ols2 <- model_ols2$fit()
  # model_ols3 <- fmglm$new(formula, NPreg, family = "gaussian", method="mle", latent=1, optim_method="lm")
  # result_ols3 <- model_ols3$fit()
  # expect_equal(result_ols1$value, result_ols2$value, tolerance = 0.001)
  # expect_equal(result_ols1$value, result_ols3$value, tolerance = 0.001)
  # the model using the complete loglik and buildin optim
  model1 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
            method="em", use_llc=T, optim_method="base")
  # the model using the normal loglik and glm.fit
  model2 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
                      method="em", use_llc=F, optim_method="glm")
  model3 <- fmglm$new(formula, NPreg, family="gaussian", latent=2,
                       method="em", use_llc=T)
  X <- matrix(c(NPreg$x,NPreg$x2),ncol = 2)
  result1 <- model1$fit(algo="em")
  print(model1$summarize())
  result2 <- model2$fit(algo="em")
  result3 <- model3$fit(start="random", algo="em", rep=5)
  #print(model1$summarize())
  #print(model2$summarize())
  # test the cem algorithm

  #result1c <- model1$fit(algo="cem")
  #result2c <- model2$fit(algo="cem")
  # test the sem algorithm
  #result1s <- model1$fit(algo="sem", max_iter=100)
  #result2s <- model2$fit(algo="sem", max_iter=100)


  # expect_equal(result1$value, result2$value, tolerance = 0.001)
  # expect_equal(result2$value, result3$value, tolerance = 0.001)
  # expect_equal(result1s$value, result2s$value, tolerance = 0.001)
  # expect_equal(result1c$value, result2c$value, tolerance = 0.001)

})
# 
test_that("test poisson", {
  data("NPreg")
  latent <- 2
  NPreg$x2 <- (NPreg$x)^2
  formula2 <- yp~x
  model1 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
                      method="em", use_llc=T)
  model2 <- fmglm$new(formula2, NPreg, family="poisson", latent=2,
                      method="em", use_llc=F, optim_method="glm")
  # Test random start with 5 rep
  result1 <- model1$fit(start="random", rep=5)
  result2 <- model2$fit()
  print(model1$summarize())
  print(result1)
  print(result2)
  expect_equal(result1$value, result2$value, tolerance = 1)
})


# test_that("test logit", {
#   load(list.files(system.file('extdata', package = 'fmmr6'), full.names = T)[1])
#   data(gsem_mixture)
#   formula <- private~age+educ+income+female
#   formula2 <- cbind(private, 1 - private)~age+educ+income+female
#   latent <- 2
#   # flex_result <- flexmix::flexmix(formula2, gsem_mixture, k=2,
#   #                                 model = FLXMRglm(family = "binomial"))
#   # print(flex_result)
#   # print(flex_result@logLik)
#   # print(parameters(flex_result, component=1))
#   # print(parameters(flex_result, component=2))
#   Y <- model.frame(formula, gsem_mixture)[,1]
#   X <- model.matrix(formula, gsem_mixture)
#   #print(X)
#   #browser()
#   model1m <- fmglm$new(formula, gsem_mixture, family="logit", latent=1,
#                       method="mle", optim_method="glm")
#   model2m <- fmglm$new(formula, gsem_mixture, family="logit", latent=1,
#                       method="mle")
#   result1m <- model1m$fit()
#   result2m <- model2m$fit()
# 
#   model1 <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
#                       method="em", optim_method="glm")
#   model2 <- fmglm$new(formula, gsem_mixture, family="logit", latent=2,
#                      method="em")
#   result1 <- model1$fit()
#   result2 <- model2$fit()
#   print(result1)
# })


test_that("test multinomial", {
  gss_test <- gss94_educ[c("sib","ed3cat","ses2cat")]
  gss_test$sib <- factor(gss_test$sib)
  gss_test$ed3cat <- factor(gss_test$ed3cat)
  gss_test$ses2cat <- factor(gss_test$ses2cat)
  gss_test$hs_sib <- ifelse(((gss_test$ed3cat==2) & (gss_test$sib==1)), 1, 0)
  gss_test$col_sib <- ifelse(((gss_test$ed3cat==3) & (gss_test$sib==1)), 1, 0)
  gss_test$hs_ses <- ifelse(((gss_test$ed3cat==2) & (gss_test$ses2cat==1)), 1, 0)
  gss_test$col_ses <- ifelse(((gss_test$ed3cat==3) & (gss_test$ses2cat==1)), 1, 0)
  formula <- ed3cat ~ hs_sib + col_sib + hs_ses + col_ses
  formula2 <- ed3cat ~ sib + ses2cat
  cmat <- matrix(c(0,1,1,1,0,0),nrow=3, ncol=2)
  print(dim(cmat))
  model_s <- fmglm$new(formula2, gss_test, family="multinom",
                      latent=1, method="mle", use_llc=T, mn_base=1)
  model_s1 <- fmglm$new(formula2, gss_test, family="multinom",
                       latent=1, method="mle", use_llc=T, constraint=cmat)
  model1 <- fmglm$new(formula, gss_test, family="multinom",
                      latent=2, method="em", use_llc=T)
  model2 <- fmglm$new(formula, gss_test, family="multinom",
                        latent=2, method="em", use_llc=T, constraint=cmat)
  result_s <- model_s$fit()
  result_s1 <- model_s1$fit()
  result1 <- model1$fit()
  result2 <- model2$fit(algo="em")

  #print(model1$summarize())
  #print(model2$summarize())
})


