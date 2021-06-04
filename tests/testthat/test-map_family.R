n_cores <- 2

x <- as.list(as.integer(1:5))
names(x) <- paste0("element_", 1:5)

test_that("pa_map works", {
  expect_identical(purrr::map(x, sqrt),
                   pa_map(x, sqrt, cores = n_cores))
})

test_that("pa_map_lgl works", {
  expect_identical(purrr::map_lgl(x, ~.x > 2),
                   pa_map_lgl(x, ~.x > 2, cores = n_cores))
})

test_that("pa_map_int works", {
  expect_identical(purrr::map_int(x, ~.x * 2L),
                   pa_map_int(x, ~.x * 2L, cores = n_cores))
})

test_that("pa_map_dbl works", {
  expect_identical(purrr::map_dbl(x, ~.x * 2.5),
                   pa_map_dbl(x, ~.x * 2.5, cores = n_cores))
})

test_that("pa_map_chr works", {
  expect_identical(purrr::map_chr(x, sqrt),
                   pa_map_chr(x, sqrt, cores = n_cores))
})

test_that("pa_map_df works", {
  expect_identical(purrr::map_df(x, sqrt),
                   pa_map_df(x, sqrt, cores = n_cores))
})

test_that("pa_map_dfr works", {
  expect_identical(purrr::map_dfr(x, sqrt),
                   pa_map_dfr(x, sqrt, cores = n_cores))
})

test_that("pa_map_dfc works", {
  expect_identical(purrr::map_dfc(x, sqrt),
                   pa_map_dfc(x, sqrt, cores = n_cores))
})
