
.onAttach <- function(libname, pkgname) {
  if(rbinom(1, 1, .1)) packageStartupMessage("https://twitter.com/kobebryant")
}
