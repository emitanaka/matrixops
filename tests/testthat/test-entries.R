test_that("entry functions works", {
  M <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
  expect_equal(entry_add(M, e(1, 1), 4),
               matrix(c(5, 2, 3, 4), 2, 2, byrow = TRUE))
  expect_equal(entry_subtract(M, e(1, 1), 4),
               matrix(c(1 - 4, 2, 3, 4), 2, 2, byrow = TRUE))
  expect_equal(entry_multiply_by(M, e(1, 1), 4),
               matrix(c(4, 2, 3, 4), 2, 2, byrow = TRUE))
  expect_equal(entry_divide_by(M, e(1, 1), 4),
               matrix(c(1/4, 2, 3, 4), 2, 2, byrow = TRUE))

})
