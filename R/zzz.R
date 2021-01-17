#' @title parapurrr: Do Purrr in Parallel
#'
#' @description This package Connects Purrr and Foreach packages. The only thing
#'   you need to do is to add a "pa_" suffix before your purrr functions. That's
#'   it! The purrr's mapping functions will be handled to Foreeach and you can
#'   use every available CPU cores in your computer. Of course, there are other
#'   extra arguments that will give you more control, but they are optional.
#'
#' @section Currently parapurrr implemented the parallel version of:
#' \enumerate{\item map}
#'
#' @docType package
#' @name parapurrr
#' @keywords internal
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  options(pa_cores = parallel::detectCores() - 1)
}
