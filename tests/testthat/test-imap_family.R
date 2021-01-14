x = runif(5, min = 1, 100)
names(x) = runif(5, min = 1, 100)

test_that("pa_imap works", {
  expect_identical(purrr::imap(x, ~paste0(.x,.y)),
                   parapurrr::pa_imap(x, ~paste0(.x,.y), .verbose = TRUE))
})

test_that("pa_imap_lgl works", {
  expect_identical(purrr::imap_lgl(x, ~(.x > as.numeric(.y))),
                   parapurrr::pa_imap_lgl(x, ~(.x > as.numeric(.y)), .verbose = TRUE))
})

test_that("pa_imap_int works", {
  expect_identical(purrr::imap_int(x, ~as.integer(.x * as.numeric(.y))),
                   parapurrr::pa_imap_int(x, ~as.integer(.x * as.numeric(.y)), .verbose = TRUE))
})

test_that("pa_imap_dbl works", {
  expect_identical(purrr::imap_dbl(x, ~.x * as.numeric(.y)),
                   parapurrr::pa_imap_dbl(x, ~.x * as.numeric(.y), .verbose = TRUE))
})

test_that("pa_imap_chr works", {
  expect_identical(purrr::imap_chr(x, ~.x * as.numeric(.y)),
                   parapurrr::pa_imap_chr(x, ~.x * as.numeric(.y), .verbose = TRUE))
})

test_that("pa_imap_dfr works", {
  expect_identical(purrr::imap_dfr(x, ~.x * as.numeric(.y)),
                   parapurrr::pa_imap_dfr(x, ~.x * as.numeric(.y), .verbose = TRUE))
})

test_that("pa_imap_dfc works", {
  expect_identical(purrr::imap_dfc(x, ~.x * as.numeric(.y)),
                   parapurrr::pa_imap_dfc(x, ~.x * as.numeric(.y), .verbose = TRUE))
})
