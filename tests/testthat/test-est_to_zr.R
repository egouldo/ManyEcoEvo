test_that("est_to_zr converts values correctly", {
  res <- est_to_zr(beta_estimate = 2, beta_SE = 1, adjusted_df = 10)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("Zr", "VZr"))

  expected_t <- 2 / 1
  expected_r <- expected_t / sqrt(expected_t^2 + 10)

  expect_equal(res$Zr, atanh(expected_r))
  expect_equal(res$VZr, 1 / 10)
})

test_that("est_to_zr emits cli warning for missing inputs", {
  local_edition(3)

  expect_snapshot({
    est_to_zr(beta_estimate = NA, beta_SE = 1, adjusted_df = 10)
  })
})
