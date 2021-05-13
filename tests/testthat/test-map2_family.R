n_cores <- 2

x <- as.list(as.integer(1:5))
y <- as.list(as.integer(6:10))
names(x) <- paste0("element_", 1:5)

test_that("pa_map2 works", {
  expect_identical(purrr::map2(x, y, ~.x * .y),
                   parapurrr::pa_map2(x, y, ~.x * .y, cores = n_cores))
})

test_that("pa_map2_lgl works", {
  expect_identical(purrr::map2_lgl(x, y, ~.x + .y > 9),
                   parapurrr::pa_map2_lgl(x, y, ~.x + .y > 9, cores = n_cores))
})

test_that("pa_map2_int works", {
  expect_identical(purrr::map2_int(x, y, ~.x * .y),
                   parapurrr::pa_map2_int(x, y, ~.x * .y, cores = n_cores))
})

test_that("pa_map2_dbl works", {
  expect_identical(purrr::map2_dbl(x, y, ~.x * .y * 2.3),
                   parapurrr::pa_map2_dbl(x, y, ~.x * .y * 2.3, cores = n_cores))
})

test_that("pa_map2_chr works", {
  expect_identical(purrr::map2_chr(x, y, ~.x * .y),
                   parapurrr::pa_map2_chr(x, y, ~.x * .y, cores = n_cores))
})

test_that("pa_map2_df works", {
  expect_identical(purrr::map2_df(x, y, ~.x * .y),
                   parapurrr::pa_map2_df(x, y, ~.x * .y, cores = n_cores))
})

test_that("pa_map2_dfr works", {
  expect_identical(purrr::map2_dfr(x, y, ~.x * .y),
                   parapurrr::pa_map2_dfr(x, y, ~.x * .y, cores = n_cores))
})

test_that("pa_map2_dfc works", {
  expect_identical(purrr::map2_dfc(x, y, ~.x * .y),
                   parapurrr::pa_map2_dfc(x, y, ~.x * .y, cores = n_cores))
})
