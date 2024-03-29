#' Create Names or Index
#' Copied from purrr's source, but modified the code to remove direct dependency
#'   to rlang. If the input x is named, the names will be returned, otherwise
#'   the elements' index will be returned.
#' @param x
#'
#' @return names or indexes of x
#' @noRd
.vec_index <- function(x) {
  # copied from tidyverse source, but modified to remove dependency
  y <- names(x)
  if (is.null(y)) {
    return(seq_along(x))
  } else {
    return(y)
  }
}

#' Parallel version of purrr imap family
#'
#' The syntax and the logic of pa_imap* functions are identical to purrr's
#'   imap functions. Please refer to \code{\link[purrr]{imap}} if you are not
#'   familiar with purrr mapping style. Except .x and .f, other arguments are
#'   optional and control the parallelization processes.
#'
#' Note that except cores, cluster_type, adaptor, auto_export, and splitter,
#'   documentation of other arguments, return section, and examples section
#'   are automatically imported from
#'   \href{https://cran.r-project.org/package=purrr}{purrr} and
#'   \href{https://cran.r-project.org/package=foreach}{foreach} packages.
#'
#' @inheritParams purrr::imap
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::imap return
#'
#' @export
pa_imap <- function(.x, .f, ...,
                    cores = NULL,
                    adaptor = "doParallel", cluster_type = NULL,
                    splitter = NULL, auto_export = TRUE, .export = NULL,
                    .packages = NULL, .noexport = NULL,
                    .errorhandling = "stop",
                    .inorder = TRUE,
                    .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_lgl <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_lgl, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_int <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_int, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_dbl <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_dbl, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_chr <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_chr, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_df <- function(.x, .f, ...,
                       cores = NULL,
                       adaptor = "doParallel", cluster_type = NULL,
                       splitter = NULL, auto_export = TRUE, .export = NULL,
                       .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_df()` requires dplyr.")
  }

  int_fun <- .pa_call(purrr::map2, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_dfr <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfr()` requires dplyr.")
  }

  int_fun <- .pa_call(purrr::map2, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

#' @rdname pa_imap
#' @export
pa_imap_dfc <- function(.x, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfc()` requires dplyr.")
  }

  int_fun <- .pa_call(purrr::map2, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .l = NULL,
                         .f = .f,
                         ... = ...,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         auto_export = auto_export,
                         splitter = splitter,
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

# #' @rdname pa_imap
# #' @export
# pa_imap_raw <- function(.x, .f, ...,
#                         cores = NULL,
#                         adaptor = "doParallel", cluster_type = NULL,
#                         splitter = NULL, auto_export = TRUE, .export = NULL,
#                         .packages = NULL, .noexport = NULL,
#                         .errorhandling = "stop",
#                         .inorder = TRUE,
#                         .verbose = FALSE) {
#
#   int_fun <- .pa_call(purrr::map_raw, x_split$.x, x_split$.y, .f, ...)
#
#   output <- .pa_internal(.x = .x,
#                          .y = .vec_index(.x),
#                          .l = NULL,
#                          .f = .f,
#                          ... = ...,
#                          int_fun = int_fun,
#                          adaptor = adaptor,
#                          cores = cores,
#                          cluster_type = cluster_type,
#                          auto_export = auto_export,
#                          splitter = splitter,
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
