# Global environment for VSpec:
if (!exists('VSpecEnv', envir=emptyenv())) {
  VSpecEnv <- new.env(parent = emptyenv())  # define if absent
}
# This is just to keep roxygen2 from complaining about no-visible-binding when these are OK:
utils::globalVariables(c(".x", "Time", "cf", "cospec", "fpf2", "fpf3", "ncospec", "ogive", "x", 
  "xc", "y", "ybar", "ymax", "ymin", "freq"))
