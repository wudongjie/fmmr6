test_that("Test Gnm", {
    browser()
    erikson <- as.data.frame(gnm::erikson)
    lvl <- levels(erikson$origin)
    levels(erikson$origin) <- levels(erikson$destination) <-
        c(rep(paste(lvl[1:2], collapse = " + "), 2), lvl[3],
            rep(paste(lvl[4:5], collapse = " + "), 2), lvl[6:9])
    #erikson <- xtabs(Freq ~ origin + destination + country, data = erikson)
    formula <- Freq ~ country:origin + country:destination
    #z <- runif(243,0,1)
    z <- rbinom(243, 1, 0.5)
    data2 <- erikson
    data2$z <- z
    levelMatrix <- matrix(c(2, 3, 4, 6, 5, 6, 6,
                            3, 3, 4, 6, 4, 5, 6,
                            4, 4, 2, 5, 5, 5, 5,
                            6, 6, 5, 1, 6, 5, 2,
                            4, 4, 5, 6, 3, 4, 5,
                            5, 4, 5, 5, 3, 3, 5,
                            6, 6, 5, 3, 5, 4, 1), 7, 7, byrow = TRUE)
    nullModel <- gnm::gnm(Freq ~ country:origin + country:destination, 
                          family = poisson, data = erikson, verbose = FALSE)
    formula1 <- Freq ~ origin + destination
    testnls <-  nls(formula1, 
                    start=runif(41,0,1),
                    data=erikson)
    
    # TModel <- gnm::gnm(Freq ~ gnm::Topo(origin, destination, spec = levelMatrix) + country:origin +
    #                        country:destination,
    #                    family = poisson, data = erikson, verbose = FALSE)
    # TopoModel <- update(nullModel, ~ . +
    #                     gnm::Topo(origin, destination, spec = levelMatrix))
    # 
    
    model_mn <- fmglm$new(formula, erikson, family="poisson",
                          latent=2, method="em", optim_method="gnm", use_llc=F)
    browser()
    result <- model_mn$fit(algo="em", start="random")
    
    print(model_mn$summarize())
})
