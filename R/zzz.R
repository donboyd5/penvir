.onLoad <- function(libname, pkgname) {
  # Create environments and assign them globally
  assign("frs", new.env(parent = emptyenv()), envir = .GlobalEnv)
  assign("trs", new.env(parent = emptyenv()), envir = .GlobalEnv)
}
