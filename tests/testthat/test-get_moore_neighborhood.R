test_that("get_moore_neighborhood works in the middle", {
  expect_equal(
    get_moore_neighborhood(5, 5, c(10, 10)),
    list(
      c(4, 4), c(4, 5), c(4, 6),
      c(5, 4), c(5, 6),
      c(6, 4), c(6, 5), c(6, 6)
    )
  )
})

test_that("get_moore_neighborhood works at the edge", {
  expect_equal(
    get_moore_neighborhood(1, 5, c(10, 10)),
    list(
      c(10, 4), c(10, 5), c(10, 6),
      c(1, 4), c(1, 6),
      c(2, 4), c(2, 5), c(2, 6)
    )
  )
})

test_that("get_moore_neighborhood works in the corner", {
  expect_equal(
    get_moore_neighborhood(1, 1, c(10, 10)),
    list(
      c(10, 10), c(10, 1), c(10, 2),
      c(1, 10), c(1, 2),
      c(2, 10), c(2, 1), c(2, 2)
    )
  )
})

test_that("non-periodic get_moore_neighborhood works at the edge", {
  expect_equal(
    get_moore_neighborhood(1, 5, c(10, 10), periodic = FALSE),
    list(
      c(1, 4), c(1, 6),
      c(2, 4), c(2, 5), c(2, 6)
    )
  )
})

test_that("non-periodic get_moore_neighborhood works in the corner", {
  expect_equal(
    get_moore_neighborhood(1, 1, c(10, 10), periodic = FALSE),
    list(
      c(1, 2),
      c(2, 1), c(2, 2)
    )
  )
})



