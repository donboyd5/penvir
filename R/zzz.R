.onLoad <- function(libname, pkgname) {
  # Create a list to hold all environments
  package_envs <<- list(
    frs = new.env(parent = emptyenv()),
    trs = new.env(parent = emptyenv())
    # Add more environments as needed
  )
}
