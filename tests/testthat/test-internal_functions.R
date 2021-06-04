test_that("splitter_works", {
  expect_error(splitter_check(x_length = 10, splitter = 123))
  expect_error(splitter_check(x_length = 10, splitter = list("123", 1:10)))
  expect_error(splitter_check(x_length = 10, splitter = list(1:5)))
  expect_error(splitter_check(x_length = 10, splitter = list(1:9, 10, 10)))

  expect_warning(.pa_args(x_length = 5, splitter = list(1:4, 5), cores = 3))

  expect_identical(suppressWarnings(.pa_args(x_length = 5, splitter = list(1:4, 5), cores = 3))[["parts"]],
                   list(1:4, 5))

  expect_error(.splitter_check_at_if(x_length = 10, x_length_sel = 3,
                                     splitter = list(1:10)))

})

test_that(".l_recycler works", {
  .l <- list(x = 1,
             y = c("a", "b", "c", "d", "e"),
             z = 6)
  expect_identical(object = .l_recycler(.l),
                   expected = list(x = c(1,1,1,1,1),
                                   y = c("a", "b", "c", "d", "e"),
                                   z = c(6,6,6,6,6)))
  .l <- list(x = 1,
             y = c("a", "b", "c", "d", "e"),
             z = 6:8)
  expect_error(object = .l_recycler(.l))

  .l <- list(x = 1:5,
             y = c("a", "b", "c", "d", "e"),
             z = 6:10)
  expect_identical(object = .l_recycler(.l),
                   expected = .l)

  .l <- list(x = 1:5,
             y = c("a", "b", "c", "d", "e"),
             z = NULL)
  expect_error(object = .l_recycler(.l))

})
