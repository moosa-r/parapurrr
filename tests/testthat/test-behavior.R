n_cores <- 2

test_that("ellipsis is passed down", {
  elip <- function(...) {
    list(...)
  }
  # General behaviour
  expect_identical(object = pa_map(".x", .f = elip, dot = NA, dotdot = NULL),
                   expected = purrr::map(".x", elip, dot = NA, dotdot = NULL))

  expect_identical(object = pa_map(".x", ~elip(.x, ...), dot = NA, dotdot = NULL),
                   expected = purrr::map(".x", ~elip(.x, ...), dot = NA, dotdot = NULL))

  expect_identical(object = pa_map(".x", ~elip(.x), dot = NA, dotdot = NULL),
                   expected = purrr::map(".x", ~elip(.x), dot = NA, dotdot = NULL))
  # Other map variants
  expect_identical(object = pa_map2(".x", ".y", elip, dot = NA, dotdot = NULL),
                   expected = purrr::map2(".x", ".y", elip, dot = NA, dotdot = NULL))

  expect_identical(object = pa_imap(".x", elip, dot = NA, dotdot = NULL),
                   expected = purrr::imap(".x", elip, dot = NA, dotdot = NULL))

  expect_identical(object = pa_pmap(list(".x", ".y", ".z"), elip, dot = NA, dotdot = NULL),
                   expected = purrr::pmap(list(".x", ".y", ".z"), elip, dot = NA, dotdot = NULL))
})

test_that("Export works as intended", {
  # auto export object in the calling environment
  nested_map <- function() {
    should_export <- 123
    capture.output(pa_map(1:2, paste0, should_export,
                          cores = n_cores, .verbose = TRUE))
  }
  expect_match(object = nested_map(),
               regexp = "should_export", all = FALSE)
  # shouldn't automatically export now...
  nested2_map <- function() {
    shouldnt_export <- 123
    nested_int_map <- function() {
      pa_map(1:2, paste0, shouldnt_export,
             cores = n_cores)
    }
    nested_int_map()
  }
  expect_error(object = nested2_map(),
               regexp = "shouldnt_export")
  # ... unless explicitly stated
  nested3_map <- function() {
    should_export <- 123
    nested_int_map <- function() {
      pa_map(1:2, paste0, should_export,
             .export = "should_export",
             cores = n_cores)
    }
    nested_int_map()
  }
  expect_identical(object = nested3_map(),
                   expected = list("1123", "2123"))
  # Packages can be exported manually
  expect_match(object = capture.output(pa_map(1:2, sqrt,
                                              .packages = "rlang", .verbose = TRUE)),
               regexp = "rlang", all = FALSE)

})
