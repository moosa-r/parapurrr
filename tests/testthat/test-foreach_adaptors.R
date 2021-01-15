n_cores = 2

x = as.list(as.integer(1:5))
names(x) = paste0("element_", 1:5)

test_that("doParallel works on windows", {
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doParallel", cluster_type = "PSOCK"), NA)
})

test_that("doParallel works on Unix", {
  skip_on_os("windows")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doParallel", cluster_type = "FORK"), NA)
})

test_that("doSNOW works on windows", {
  skip_if_not_installed("doSNOW")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doSNOW", cluster_type = "SOCK"), NA)
})

test_that("doSNOW works on Unix", {
  skip_if_not_installed("doSNOW")
  skip_on_os("windows")
  skip_if_not_installed("Rmpi")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doSNOW"), cluster_type = "MPI")
  skip_if_not_installed("nws")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doSNOW"), cluster_type = "NWS")
})

test_that("doFuture works on windows", {
  skip_if_not_installed("doFuture")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "multisession"), NA)
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "cluster_PSOCK"), NA)
})

test_that("doFuture works on Unix", {
  skip_if_not_installed("doFuture")
  skip_on_os("windows")
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "multicore"), NA)
  expect_error(parapurrr::pa_map(x, sqrt, cores = n_cores, adaptor = "doFuture", cluster_type = "cluster_FORK"), NA)
})
