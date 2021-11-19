test_that("Test MultiNomial", {
    gss_test <- gss94_educ[c("sib","ed3cat","ses2cat")]
    gss_test$sib <- factor(gss_test$sib)
    gss_test$ed3cat <- factor(gss_test$ed3cat)
    gss_test$ses2cat <- factor(gss_test$ses2cat)
    gss_test$hs_sib <- ifelse(((gss_test$ed3cat==2) & (gss_test$sib==1)), 1, 0)
    gss_test$col_sib <- ifelse(((gss_test$ed3cat==3) & (gss_test$sib==1)), 1, 0)
    gss_test$hs_ses <- ifelse(((gss_test$ed3cat==2) & (gss_test$ses2cat==1)), 1, 0)
    gss_test$col_ses <- ifelse(((gss_test$ed3cat==3) & (gss_test$ses2cat==1)), 1, 0)
    formula <- ed3cat ~ sib + ses2cat
    # DataModel should be in a multinomial structure
    dm1 <- DataModel$new(gss_test, formula, family="multinom")
    expect_equal(dim(dm1$Y), c(length(gss_test$ed3cat), 2))
    # if there are two dependent variables and two latent classes, it is a 3 by 2 matrix.
    theta <- matrix(c(1,2,3,3,2,1),nrow=3,ncol=2)
    Y <- dm1$Y
    X <- dm1$X

    # The Family distribution
    density_fit <- FamilyMultiNomial$new()$gen_density()(theta,Y,X)
    expect_equal(length(density_fit), length(gss_test$ed3cat))
})
