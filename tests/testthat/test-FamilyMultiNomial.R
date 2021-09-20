test_that("multiplication works", {
    gss_test <- gss94_educ[c("sib","ed3cat","ses2cat")]
    gss_test$sib <- factor(gss_test$sib)
    gss_test$ed3cat <- factor(gss_test$ed3cat)
    gss_test$ses2cat <- factor(gss_test$ses2cat)
    gss_test$hs_sib <- ifelse(((gss_test$ed3cat==2) & (gss_test$sib==1)), 1, 0)
    gss_test$col_sib <- ifelse(((gss_test$ed3cat==3) & (gss_test$sib==1)), 1, 0)
    gss_test$hs_ses <- ifelse(((gss_test$ed3cat==2) & (gss_test$ses2cat==1)), 1, 0)
    gss_test$col_ses <- ifelse(((gss_test$ed3cat==3) & (gss_test$ses2cat==1)), 1, 0)
    # Y <- fastDummies::dummy_columns(gss94$egp, remove_first_dummy = T)
    # Y <- Y[,2:ncol(Y)]
    # X <- fastDummies::dummy_columns(gss94$fa_egp, remove_first_dummy = T)
    # X <- X[, 2:ncol(X)]
    # new_data <- cbind(Y, X)
    formula <- ed3cat ~ hs_sib + col_sib + hs_ses + col_ses
    formula2 <- ed3cat ~ sib + ses2cat
    # print(model.frame(formula, gss94)[,1])
    # print(class(model.frame(formula, gss94)[,1]))
    # print(model.matrix(formula, gss94))
    model_mn <- fmglm$new(formula2, gss_test, family="multinom",
                          latent=1, method="mle")

    result_expect <- nnet::multinom(formula2, data = gss_test)
    print(summary(result_expect))
    result <- model_mn$fit(algo="mle")
    aic_actual <- result$AIC
    aic_expect <- AIC(result_expect)
    expect_equal(aic_actual, aic_expect, tolerance=1e-3)
})
