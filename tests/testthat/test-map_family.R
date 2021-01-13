x = as.list(as.integer(1:5))
names(x) = paste0("element_", 1:5)

test_that("pa_map works", {
  expect_identical(purrr::map(x, sqrt),
                   parapurrr::pa_map(x, sqrt, verbose = TRUE))
})

test_that("pa_map_lgl works", {
  expect_identical(purrr::map_lgl(x, ~.x > 2),
                   parapurrr::pa_map_lgl(x, ~.x > 2, verbose = TRUE))
})

test_that("pa_map_int works", {
  expect_identical(purrr::map_int(x, ~.x * 2L),
                   parapurrr::pa_map_int(x, ~.x * 2L, verbose = TRUE))
})

test_that("pa_map_dbl works", {
  expect_identical(purrr::map_dbl(x, ~.x * 2.5),
                   parapurrr::pa_map_dbl(x, ~.x * 2.5, verbose = TRUE))
})

test_that("pa_map_chr works", {
  expect_identical(purrr::map_chr(x, sqrt),
                   parapurrr::pa_map_chr(x, sqrt, verbose = TRUE))
})

test_that("pa_map_df works", {
  expect_identical(purrr::map_df(x, sqrt),
                   parapurrr::pa_map_df(x, sqrt, verbose = TRUE))
})

test_that("pa_map_dfr works", {
  expect_identical(purrr::map_dfr(x, sqrt),
                   parapurrr::pa_map_dfr(x, sqrt, verbose = TRUE))
})

test_that("pa_map_dfc works", {
  expect_identical(purrr::map_dfc(x, sqrt),
                   parapurrr::pa_map_dfc(x, sqrt, verbose = TRUE))
})

test_that("pa_map works", {
  expect_identical(purrr::map(x, sqrt),
                   parapurrr::pa_map(x, sqrt, verbose = TRUE))
})

test_that("pa_map works", {
  expect_identical(purrr::map(x, sqrt),
                   parapurrr::pa_map(x, sqrt, verbose = TRUE))
})

