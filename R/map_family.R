#' Parallel Version of purrr map family
#'
#' The syntax and the logic of pa_map* functions are the identical to purrr's
#'   map functions. Please refer to \code{\link[purrr]{map}} if you are not
#'   familiar with purrr mapping style. Except .x and .y, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreeach function and the selected forreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param cores (optional) Number of cores (i.e. workers) to be used. Default
#'   is: your computer CPU cores - 1
#' @param adaptor The foreeach adaptor to be used. Available options are:
#'   "DoParallel" (default).
#' @param cluster_type The Clusters archetecture to be used with the selected
#'   adaptor. Note that allowed values for this argument, depends on the
#'   "adaptor" argument: \itemize{
#'   \item if adaptor = "DoParallel":
#'     \enumerate{\item PSOCK (default for Windows)
#'                \item FORK (default for Unix)}
#'   }
#'
#' @inherit purrr::map return
#' @export
pa_map <- function(.x, .f,
                   cores = NULL,
                   adaptor = "DoParallel", cluster_type = NULL,
                   .export = NULL, .packages = NULL, .noexport = NULL,
                   .errorhandling = "stop",
                   .inorder = TRUE,
                   .verbose = FALSE) {

  int_fun <- quote(purrr::map(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = NULL,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_lgl <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- quote(purrr::map_lgl(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = NULL,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_int <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- quote(purrr::map_int(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = NULL,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_dbl <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- quote(purrr::map_dbl(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = NULL,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_chr <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- quote(purrr::map_chr(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = NULL,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_df <- function(.x, .f,
                      cores = NULL,
                      adaptor = "DoParallel", cluster_type = NULL,
                      .export = NULL, .packages = NULL, .noexport = NULL,
                      .errorhandling = "stop",
                      .inorder = TRUE,
                      .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_df()` requires dplyr.")
  }

  int_fun <- quote(purrr::map(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = dplyr::bind_rows,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_dfr <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfr()` requires dplyr.")
  }

  int_fun <- quote(purrr::map(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = dplyr::bind_rows,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

#' @rdname pa_map
#' @export
pa_map_dfc <- function(.x, .f,
                       cores = NULL,
                       adaptor = "DoParallel", cluster_type = NULL,
                       .export = NULL, .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfc()` requires dplyr.")
  }

  int_fun <- quote(purrr::map(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         .combine = c,
                         .init = NULL,
                         .final = dplyr::bind_cols,
                         .inorder = .inorder,
                         .multicombine = TRUE,
                         .maxcombine = NULL,
                         .errorhandling = .errorhandling,
                         .packages = .packages,
                         .export = .export,
                         .noexport = .noexport,
                         .verbose = .verbose)
  return(output)
}

# #' @rdname pa_map
# #' @export
# pa_map_raw <- function(.x, .f,
#                        cores = NULL,
#                        adaptor = "DoParallel", cluster_type = NULL,
#                        .export = NULL, .packages = NULL, .noexport = NULL,
#                        .errorhandling = "stop",
#                        .inorder = TRUE,
#                        .verbose = FALSE) {
#
#   int_fun <- quote(purrr::map_raw(x, .f))
#
#   output <- .pa_internal(.x = .x,
#                          .y = NULL,
#                          .f = .f,
#                          int_fun = int_fun,
#                          adaptor = adaptor,
#                          cores = cores,
#                          cluster_type = cluster_type,
#                          .combine = c,
#                          .init = NULL,
#                          .final = NULL,
#                          .inorder = .inorder,
#                          .multicombine = TRUE,
#                          .maxcombine = NULL,
#                          .errorhandling = .errorhandling,
#                          .packages = .packages,
#                          .export = .export,
#                          .noexport = .noexport,
#                          .verbose = .verbose)
#   return(output)
# }
