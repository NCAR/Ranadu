## This is loaded when the Ranadu package is loaded, to provide a special environment
## for the VSpec function. If lost for some reason, the environment can also be
## restored by sourcing Ranadu.R.
.onLoad <- function(libname, pkgname) {
  # new environment for VSpec:
  if (!exists('VSpecEnv', envir=emptyenv())) {
    assign('VSpecEnv', new.env(parent = emptyenv()), envir = globalenv())  # define if absent
  }
  # new environment for RKCK.integrate:
  if (!exists('RKCKEnv', envir=emptyenv())) {
    assign('RKCKEnv', new.env(parent = emptyenv()), envir = globalenv())  # define if absent
  }
  invisible()
  ## make dplyr, magrittr and tibble available:
  pkgs <- c('dplyr', 'magrittr', 'tibble')
  for (pkg in pkgs) {library(pkg, character.only=TRUE)}
}
