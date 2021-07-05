partial <- function(f, ...) {
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}


gen_gr = function(ll) {
  gr <- function(theta) {
    g <- grad(ll,theta)
    return(g)
  }
  return(gr)
}

vectorize_dummy = function(x) {
  stopifnot( all(x == floor(x)) )
  stopifnot( all(x > 0) )
  m <- max(x)
  to_dummy = function(val) {
    vec <- rep(0, m)
    vec[val] <- 1
    return(vec)
  }
  dvec <- sapply(x, to_dummy)
  return(t(dvec))
}

add_ast = function(ps) {
  ast = function(p) {
    if (p<0.001) {
      return("***")
    } else if (p<0.01) {
      return("**")
    } else if (p<0.05) {
      return("*")
    } else if (p<0.01) {
      return(".")
    } else (
      return(" ")
    )
  }
  v_ast <- sapply(ps, ast)
  return(v_ast)
}

