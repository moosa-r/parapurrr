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
