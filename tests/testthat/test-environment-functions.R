
# test environment_exists -------------------------------------------------

test_that("environment_exists returns TRUE for existing environments", {
  expect_true(environment_exists("frs"))
})

test_that("environment_exists returns FALSE for non-existing environments when stop_on_fail = FALSE", {
  expect_false(environment_exists("nonexistent", stop_on_fail = FALSE))
})

test_that("environment_exists stops with an informative message when stop_on_fail = TRUE and fund does not exist", {
  environments <- get_environments()
  expected_message <- paste0(
    "Pension fund 'nonexistent' does not exist. Valid funds are: \n  ",
    paste(names(environments), collapse = ", "),
    "."
  )
  expect_error(
    environment_exists("nonexistent", stop_on_fail = TRUE),
    expected_message
  )
})


# test get_env ------------------------------------------------------------

test_that("get_env returns NULL for non-existing environments", {
  expect_null(get_env("nonexistent"))
})

test_that("get_env returns the environment for existing environments", {
  expect_type(get_env("frs"), "environment")
})
