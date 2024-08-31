
get_package_environments <- function(package_name) {
  # Get the namespace of the package
  pkg_ns <- asNamespace(package_name)

  # List all objects in the namespace
  obj_names <- ls(pkg_ns)

  # Get all objects
  all_objects <- mget(obj_names, envir = pkg_ns, inherits = FALSE)

  # Filter for environment objects
  env_objects <- all_objects[sapply(all_objects, is.environment)]

  return(env_objects)
}


# library(penvir)
# ns <- asNamespace("penvir")
# objs <- ls(ns)
# envs <- objs[sapply(objs, function(x) is.environment(get(x, envir = ns)))]
# populate(frs)
