\name{rterm-package}
\alias{rterm-package}
\alias{rterm}
\docType{package}
\title{
Temperature-Energy regression models for building energy use data
~~ package title ~~
}
\description{
This package contains functions to 
perform regressions of building energy use data on outdoor temperature 
and variables derived from outdoor temperature. This includes a 
frequentist change-point model, a frequentist variable-base-degree-day model,
and a Bayesian change-point model. All models apply to heating, cooling, or both.
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab rterm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-01\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Michael Logsdon

Maintainer: Michael Logsdon, michael@ecotope.com
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(rfm)
mod <- cplm(kwhd ~ avetemp, data = rfm)
summary(mod)
plot(mod)
~~ simple examples of the most important functions ~~
}
