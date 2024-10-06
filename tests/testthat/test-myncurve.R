test_that("myncurve works for standard normal distribution", {
  result <- myncurve(mu = 0, sigma = 1, a = 1.5)

  # Expected probability for P(X ≤ 1.5) in standard normal distribution
  expected_prob <- pnorm(1.5, mean = 0, sd = 1)

  # Check if the returned probability is correct
  expect_equal(result$probability, expected_prob, tolerance = 1e-5)

  # Check if other returned values are correct
  expect_equal(result$mu, 0)
  expect_equal(result$sigma, 1)
  expect_equal(result$a, 1.5)
})

