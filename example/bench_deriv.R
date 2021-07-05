library(rbenchmark)

normal_formula <- ~ (1/sqrt(2*pi*s^2))*(exp(-(x-u)^2/(2*s^2)))
quote_formula <- quote((1/sqrt(2*pi*s^2))*(exp(-(x-u)^2/(2*s^2))))
benchmark("d" = { D(quote_formula, "x")},
          "deriv" = {deriv(normal_formula, "x")},
          "deriv3" = {deriv3(normal_formula, "x")},
          replications = 1000,
          columns = c("test", "replications", "elapsed", 
                      "relative", "user.self", "sys.self"))


