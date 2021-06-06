#' Disable automatic doPar backend registering
#'
#' By default, parapurrr automatically register doPar backends for you.
#'   based on the provided "adaptor" argument in the function call, parapurrr
#'   will automatically handle (initiate, register and terminate) the selected
#'   doPar backend and it's infrastructures.
#'   But you can disable this behaviour and force manual control of the
#'   doPar backend. To do that your can: \enumerate{
#'   \item Call manual_backend(TRUE) once in your R session. After that,
#'    any value for "adaptor" argument in parapurrrr function calls will be
#'    ignored and you will have to manually handle the doPAr backend.
#'   \item Call any parapurrr function with adaptor = NULL.}
#'
#' @param force (logical) Stop automatic handling the doPar backend by
#'   parapurrr? Supply TRUE to stop automatic handling of dopar backend and
#'   force the manual mode.
#'
#' @return NULL, Internally will change the corresponding option in R
#'   environment.
#' @export
#' @examples
#' \dontrun{ manual_backend(TRUE) }
manual_backend <- function(force) {
  if (!is.null(force) && !is.na(force) & is.logical(force)) {
    options(pa_manual_backend = force)
  } else {
    stop("force should be either TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

#' Use doRNG package for reproducibility
#'
#' doRNG package provides functions to perform reproducible parallel foreach
#'   loops. By calling this function, you can force parapurrr to use doRNG
#'   with combination of your any selected doPar adaptor to create fully
#'   reproducible parallel function calls. see:
#'   \href{https://cran.r-project.org/package=doRNG}{doRNG:
#'   Generic Reproducible Parallel Backend for 'foreach' Loops}
#'
#' @param dorng (logical) Use doRNG instead of normal foreach loops?
#'
#' @return NULL, Internally will change the corresponding option in R
#'   environment.
#' @export
#' @examples
#' \dontrun{
#' use_doRNG(TRUE)
#' set.seed(100)
#' x <- pa_map(1:3, runif)
#' set.seed(100)
#' y <- pa_map(1:3, runif)
#' identical(x,y)}
use_doRNG <- function(dorng) {
  if (!is.null(dorng) && !is.na(dorng) & is.logical(dorng)) {
    options(pa_dorng = dorng)
  } else {
    stop("dorng should be either TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}
