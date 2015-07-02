# Functions for the rstan interface...



writeStan <- function(intercept = TRUE, heating = TRUE, cooling = FALSE, baseLoad = 50, heatingBase = 55, heatingSlope = 15, coolingBase = 75, coolingSlope = 15) {
  # Data
  stanx <- c("data {",
             "  int<lower=0> N;",
             "  real x[N];",
             "  real Y[N];",
             "}")
  
  # Parameters
  stanx <- c(stanx, "parameters {",
             "  real<lower=0> sigma;")
  if(intercept) {
    stanx <- c(stanx, "  real<lower=0> baseLoad;")
  }
  if(heating) {
    stanx <- c(stanx, "  real heatingSlope;")
    stanx <- c(stanx, "  real<lower=min(x),upper=max(x)> heatingBase;")
  }
  if(cooling) {
    stanx <- c(stanx, "  real coolingSlope;")
    stanx <- c(stanx, "  real<lower=min(x),upper=max(x)> coolingBase;")
  }
  stanx <- c(stanx, "}")
  
  # Model
  stanx <- c(stanx, "model {")
  stanx <- c(stanx, "  real mu[N];")
  if(intercept) {
    rate <- .03
    shape <- baseLoad * rate
    # stanx <- c(stanx, paste0("  baseLoad ~ gamma(", shape, ", ", rate, ");"))
    stanx <- c(stanx, paste0("  baseLoad ~ normal(", baseLoad, ", 500);"))
  }
  if(heating) {
    stanx <- c(stanx, paste0("  heatingSlope ~ normal(", heatingSlope, ",10);"))
    stanx <- c(stanx, "  heatingBase ~ normal(55,8);")
  }
  if(cooling) {
    stanx <- c(stanx, paste0("  coolingSlope ~ normal(", coolingSlope, ",10);"))
    stanx <- c(stanx, "  coolingBase ~ normal(65,8);")
  }
  stanx <- c(stanx, "  sigma ~ cauchy(0,5);")
  stanx <- c(stanx, "  for (n in 1:N)")
  
  heatStr <- "if_else(x[n] > heatingBase, 0, heatingSlope) * (heatingBase - x[n])"
  coolStr <- "if_else(x[n] < coolingBase, 0, coolingSlope) * (x[n] - coolingBase)"
  if(intercept) {
    muStr <- "    mu[n] <- baseLoad +"
  } else {
    muStr <- "    mu[n] <-"
  }
  if(heating & !cooling) {
    muStr <- paste(muStr, heatStr, ";")
  } else if(!heating & cooling) {
    muStr <- paste(muStr, coolStr, ";")
  } else if(heating & cooling) {
    muStr <- paste(muStr, heatStr, "+", coolStr, ";")
  }
  stanx <- c(stanx, muStr)
  
  stanx <- c(stanx, "  Y ~ normal(mu,sigma);")
  stanx <- c(stanx, "}")
  stanx
}

