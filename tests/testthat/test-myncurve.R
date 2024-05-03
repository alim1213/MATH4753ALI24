test_that("Area calculation test 1", {
  res <- myncurve(10, 4, 10)
  expect_equal(res$area, 0.5)
  expect_equal(res$mu, 10)
  expect_equal(res$sigma, 4)
})
test_that("Area calculation test 3", {
  res <- myncurve(1, 2, 3)
  expect_equal(res$area, 0.8413)
  expect_equal(res$mu, 1)
  expect_equal(res$sigma, 2)
})
test_that("Area calculation test 2", {
  res <- myncurve(1, 2, 4)
  expect_equal(res$area, 0.9332)
  expect_equal(res$mu, 1)
  expect_equal(res$sigma, 2)
})
