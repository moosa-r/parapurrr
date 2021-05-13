n_cores <- 2

x <- as.list(as.integer(1:5))
names(x) <- paste0("element_", 1:5)

test_that("pa_map_if works", {
  expect_identical(purrr::map_if(.x = x, .p = ~.x%%2==0, .f = ~"is.even"),
                   parapurrr::pa_map_if(.x = x, .p = ~.x%%2==0, .f = ~"is.even",
                                        cores = n_cores))
  expect_identical(purrr::map_if(.x = x, .p = ~.x%%2==0,
                                 .f = ~"is.even", .else = ~"is.false"),
                   parapurrr::pa_map_if(.x = x, .p = ~.x%%2==0,
                                        .f = ~"is.even", .else = ~"is.false",
                                        cores = n_cores))
})

test_that("pa_map_at works", {
  expect_identical(purrr::map_at(.x = x, .at = c(1,4), .f = ~.x * 1000),
                   parapurrr::pa_map_at(.x = x, .at = c(1,4), .f = ~.x * 1000,
                                        cores = n_cores))
})
