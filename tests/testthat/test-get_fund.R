
EXPECTED_OBJECT_NAMES <- c("calculate_benefits", "beneficiaries")

test_that("stored fund has expected objects", {
  expect_equal(names(get_fund("frs")), EXPECTED_OBJECT_NAMES)
})
