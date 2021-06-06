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

test_that("Manual backends work", {
  # Can be forced
  expect_warning(object = pa_map(1:3, sqrt, adaptor = NULL),
                 regexp = "Sequential")
  manual_register(TRUE)
  expect_match(object = capture_warnings(pa_map(1:3, sqrt)),
                 regexp = "manual")
  # works
  expect_length(object = suppressWarnings(unique(pa_map(1:3, ~Sys.getpid()))),
                n = 1)
  doParallel::registerDoParallel(n_cores)
  expect_length(object = suppressWarnings(unique(pa_map(1:3, ~Sys.getpid()))),
                n = n_cores)
  # revert
  manual_register(FALSE)
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

  ## different mods of auto_export works
  nested4_map <- function(...) {
    should_export <- 123
    shouldnt_export <- 456
    capture.output(pa_map(1:2, ...,
                          cores = n_cores, .verbose = TRUE))
  }
  ## auto export Works as intended
  # auto_export = TRUE
  expect_match(object = nested4_map(paste, should_export, auto_export = TRUE),
               regexp = "should_export", all = FALSE)
  expect_false(object = any(grepl(pattern = "shouldnt_export",
                                  x = nested4_map(paste, should_export, auto_export = TRUE))))
  expect_false(object = any(grepl(pattern = "shouldnt_export|should_export",
                                  x = nested4_map(paste, auto_export = TRUE))))
  # auto_export = FALSE
  expect_error(object = nested4_map(paste, should_export, auto_export = FALSE),
               regexp = "should_export")
  expect_match(object = nested4_map(paste, should_export, auto_export = FALSE,
                                    .export = "should_export"),
               regexp = "should_export", all = FALSE)
  # auto_export = "all"
  expect_true(object = any(grepl(pattern = "shouldnt_export|should_export",
                                  x = nested4_map(paste, auto_export = "all"))))
  # do scoping properly
  should_find <- 789
  expect_match(object = nested4_map(paste, should_find, auto_export = FALSE,
                                    .export = "should_find"),
               regexp = "should_find", all = FALSE)
})
