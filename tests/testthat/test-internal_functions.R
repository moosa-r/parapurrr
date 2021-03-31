test_that("splitter_works", {
  expect_error(splitter_check(x_length = 10, splitter = 123))
  expect_error(splitter_check(x_length = 10, splitter = list("123", 1:10)))
  expect_error(splitter_check(x_length = 10, splitter = list(1:5)))
  expect_error(splitter_check(x_length = 10, splitter = list(1:9, 10, 10)))

  expect_warning(.pa_args(x_length = 5, splitter = list(1:4, 5), cores = 3))

  expect_identical(suppressWarnings(.pa_args(x_length = 5, splitter = list(1:4, 5), cores = 3))[["parts"]],
                   list(1:4, 5))

})
