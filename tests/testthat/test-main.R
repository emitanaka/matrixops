test_that("multiplication works", {
  M <- matrix(1:4, nrow = 2)
  expect_equal(entry_subtract(M, e(1, 1), 3),
               matrix(c(-2, 2, 3, 4), nrow = 2))
})
