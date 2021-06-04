n_cores <- 2

.l <- list(x = 1:5,
           y = 3:7,
           z = 7:11)

test_that("pa_pmap works", {
  expect_identical(purrr::pmap(.l, paste0),
                   pa_pmap(.l, paste0,
                           cores = n_cores))
})

test_that("pa_pmap_lgl works", {
  expect_identical(purrr::pmap_lgl(.l, function(x, y, z) (x + y) < z),
                   pa_pmap_lgl(.l, function(x, y, z) (x + y) < z,
                               cores = n_cores))
})

test_that("pa_pmap_int works", {
  expect_identical(purrr::pmap_int(.l, sum),
                   pa_pmap_int(.l, sum,
                               cores = n_cores))
})

test_that("pa_pmap_dbl works", {
  expect_identical(purrr::pmap_dbl(.l, ~sum(..1/2 + ..2 + ..3)),
                   pa_pmap_dbl(.l, ~sum(..1/2 + ..2 + ..3),
                               cores = n_cores))
})

test_that("pa_pmap_chr works", {
  expect_identical(purrr::pmap_chr(.l, sum),
                   pa_pmap_chr(.l, sum,
                               cores = n_cores))
})

test_that("pa_pmap_df works", {
  expect_identical(purrr::pmap_df(.l, function(x, y, z) c(x = x, y = y)),
                   pa_pmap_df(.l, function(x, y, z) c(x = x, y = y),
                              cores = n_cores))
})

test_that("pa_pmap_dfr works", {
  expect_identical(purrr::pmap_dfr(.l, function(x, y, z) c(x = x, y = y)),
                   pa_pmap_dfr(.l, function(x, y, z) c(x = x, y = y),
                               cores = n_cores))
})

test_that("pa_pmap_dfc works", {
  expect_identical(purrr::pmap_dfc(.l, function(x, y, z) c(x = x, y = y)),
                   pa_pmap_dfc(.l, function(x, y, z) c(x = x, y = y),
                               cores = n_cores))
})
