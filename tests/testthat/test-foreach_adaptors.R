n_cores <- 2

x <- as.list(as.integer(1:5))
names(x) <- paste0("element_", 1:5)

y <- purrr::map(x, sqrt)

was_parallel <- function(...) {
  x <- suppressWarnings(parapurrr::pa_map(1:10, ~Sys.getpid(), ...))
  length(unique(x)) > 1
}

test_that("doParallel works on windows and UNIX", {
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doParallel",
                                              cluster_type = "PSOCK"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doParallel",
                           cluster_type = "PSOCK",
                           cores = n_cores))
})

test_that("doParallel works on Unix", {
  skip_on_os("windows")
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doParallel",
                                              cluster_type = "FORK"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doParallel",
                           cluster_type = "FORK",
                           cores = n_cores))
})

test_that("doSNOW works", {
  skip_if_not_installed("doSNOW")
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doSNOW",
                                              cluster_type = "SOCK"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doSNOW",
                           cluster_type = "SOCK",
                           cores = n_cores))
})

test_that("doFuture works on windows", {
  skip_if_not_installed("doFuture")
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doFuture",
                                              cluster_type = "multisession"),
                   expected = y)
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doFuture",
                                              cluster_type = "cluster_PSOCK"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doFuture",
                           cluster_type = "multisession",
                           cores = n_cores))
  expect_true(was_parallel(adaptor = "doFuture",
                           cluster_type = "cluster_PSOCK",
                           cores = n_cores))
})

test_that("doFuture works on Unix", {
  skip_if_not_installed("doFuture")
  skip_on_os("windows")
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doFuture",
                                              cluster_type = "multicore"),
                   expected = y)
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doFuture",
                                              cluster_type = "cluster_FORK"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doFuture",
                           cluster_type = "multicore",
                           cores = n_cores))
  expect_true(was_parallel(adaptor = "doFuture",
                           cluster_type = "cluster_FORK",
                           cores = n_cores))
})

test_that("doMC works", {
  skip_if_not_installed("doMC")
  skip_on_os("windows")
  expect_identical(object = parapurrr::pa_map(x, sqrt,
                                              cores = n_cores,
                                              adaptor = "doMC"),
                   expected = y)

  expect_true(was_parallel(adaptor = "doMC",
                           cores = n_cores))
})

test_that("manual backend can be forced", {
  expect_warning(object = parapurrr::pa_map(x, ~Sys.getpid(), adaptor = NULL),
                 regexp = "Sequential")

  expect_false(object = was_parallel(adaptor = NULL))
  manual_register(TRUE)
  expect_false(object = was_parallel())
  manual_register(FALSE)
  expect_true(object = was_parallel(cores = n_cores))
})

test_that("doRNG works", {
  use_doRNG(TRUE)
  skip_if_not_installed("doRNG")
  expect_identical(object = {set.seed(100); pa_map(1:3, runif, cores = n_cores)},
                   expected = {set.seed(100); pa_map(1:3, runif, cores = n_cores)})
  use_doRNG(FALSE)
})

