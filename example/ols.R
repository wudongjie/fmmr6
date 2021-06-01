load("data/rand_lm_data.RData")

lm1 <- lm(y~x1+x2,rand_lm_data)
summary(lm1)

mod1 <- fmmr6::glfmm$new(y~x1+x2, rand_lm_data)$fit()
#k <- mod1

