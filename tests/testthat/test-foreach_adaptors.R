n_cores <- 2

x <- as.list(as.integer(1:5))
names(x) <- paste0("element_", 1:5)

test_that("doParallel works on windows and UNIX", {
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores,
                                              adaptor = "doParallel",
                                              cluster_type = "PSOCK"),
                   expected = purrr::map(x, sqrt))
})

test_that("doParallel works on Unix", {
  skip_on_os("windows")
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores,
                                              adaptor = "doParallel",
                                              cluster_type = "FORK"),
                   expected = purrr::map(x, sqrt))
})

test_that("doSNOW works", {
  skip_if_not_installed("doSNOW")
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doSNOW", cluster_type = "SOCK"),
                   expected = purrr::map(x, sqrt))
})

test_that("doFuture works on windows", {
  skip_if_not_installed("doFuture")
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "multisession"),
                   expected = purrr::map(x, sqrt))
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "cluster_PSOCK"),
                   expected = purrr::map(x, sqrt))
})

test_that("doFuture works on Unix", {
  skip_if_not_installed("doFuture")
  skip_on_os("windows")
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "multicore"),
                   expected = purrr::map(x, sqrt))
  expect_identical(object = parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "cluster_FORK"),
                   expected = purrr::map(x, sqrt))
})
