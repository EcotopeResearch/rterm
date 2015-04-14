\name{rterm-package}
\alias{rterm-package}
\alias{rterm}
\docType{package}
\title{
Temperature-Energy Regression Models for Energy Use in Buildings
}
\description{
This package contains functions to 
perform regressions of building energy use data on outdoor temperature 
and variables derived from outdoor temperature. This includes a 
frequentist change-point model, a frequentist variable-base-degree-day model,
and a Bayesian change-point model. All models apply to heating, cooling, or both.

This current, in-progress implementation contains the frequentist change 
point method. Other models will become available as development continues.
}
\details{
\tabular{ll}{
Package: \tab rterm\cr
Type: \tab Package\cr
Version: \tab 0.0.0.9000\cr
Date: \tab 2015-04-01\cr
License: \tab GPL-2\cr
}

}
\author{
Michael Logsdon

Maintainer: Michael Logsdon, michael@ecotope.com

}
\references{
References to come later
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# Architecture Office in Bremerton
data(rfm)
mod <- cplm(kwh ~ oat, data = rfm)
summary(mod)
plot(mod)


# Ecotope Office Energy Use
data(ecotope)
mod <- cplm(kwhd ~ oat, data = ecotope)
coef(mod)
plot(mod)
residsPlot(mod, "dateEnd") # Weather-adjusted time trend


# Ductless Heat Pump Study
data(dhp)
results <- cplmx(kwhd ~ avetemp, data = dhp,
                   id_vars = c("id", "post"))
summary(results)
plot(results, "heatingChangePoint")

}
