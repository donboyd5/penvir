# tests/testthat/test-initialize-environments.R

# library(testthat)
# library(penvir)

# Testing initialize_environments() is difficult due to the way it is
# designed to work within the package's namespace. When you run this function in
# a test environment, it attempts to modify the package's internal environment,
# which is locked. To test initialize_environments(), we need to ensure that
# the test environment allows for the creation and modification of the
# package-specific environment. However, since the package's internal
# environment is locked, we cannot directly test initialize_environments().

EXPECTED_INTERNAL_ENVIRONMENTS <- c("frs", "trs")

test_that("initialize_environments sets up environments correctly", {
  # Load the package (this will trigger .onLoad and initialize_environments)
  library("penvir", character.only = TRUE)

  # Check that .penvir_env exists
  expect_true(exists(".penvir_env", envir = parent.env(environment())))

  # Check that the environments list exists within .penvir_env
  expect_true(exists("environments", envir = get(".penvir_env", envir = parent.env(environment()))))

  # Check that specific environments (e.g., frs, trs) are created
  environments <- get_environments()
  expect_named(environments, EXPECTED_INTERNAL_ENVIRONMENTS)
})
