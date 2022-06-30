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

    current_pa_force_adaptor <- getOption("pa_force_adaptor")
    if (!is.null(current_pa_force_adaptor)) {
      warning("The options previously set by the `force_adaptor(",
              ifelse(is.null(current_pa_force_adaptor),
                     yes = "NULL", no =  current_pa_force_adaptor),
              ")` function call ",
              "was reverted to the default settings after a conflict caused by ",
              "calling manual_backend(",
              force,
              ")")
      options(pa_force_adaptor = NULL,
              pa_force_cluster_type = NULL)
    }
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

#' Force An Adaptor
#'
#' You can force parapurrr to ignore the "adaptor" parameters supplied in
#'   parapurrr function and force using a specific adaptor.
#'
#' @param force_adaptor What adaptor to force? valis values are:
#'   "doMPI", "doParallel", "doSNOW", "doFuture", and "doMC".
#' @param force_cluster_type What cluster type to force?
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' force_adaptor(doFuture)}
force_adaptor <- function(force_adaptor = NULL, force_cluster_type = NULL) {
  if (isFALSE(force_adaptor) || is.null(force_adaptor)) {

    options(pa_force_adaptor = NULL,
            pa_force_cluster_type = NULL)

  } else {

    if (match(force_adaptor,
              c("doMPI", "doParallel", "doSNOW", "doFuture", "doMC"),
              nomatch = 0) == 0) {
      stop("Invalid adaptor. Valid values are: doMPI, doParallel, doSNOW, doFuture, doMC.",
           call. = FALSE)
    } else {
      options(pa_force_adaptor = force_adaptor)
      options(pa_force_cluster_type = force_cluster_type)

      if (isTRUE(getOption("pa_manual_backend"))) {
        warning("The options previously set by the `manual_backend(TRUE)` function call ",
                "was reverted to the default settings after a conflict caused by ",
                "calling force_adaptor(",
                ifelse(is.null(force_adaptor),
                       yes = "NULL", no =  force_adaptor),
                ", ",
                ifelse(is.null(force_cluster_type),
                       yes = "NULL", no =  force_cluster_type),
                ")")
        options(pa_manual_backend = FALSE)
      }
    }

  }

  invisible(NULL)
}
