#' Parallel Version of purrr map2 family
#'
#' The syntax and the logic of pa_map2 and pa_pmap family functions are
#'   identical to purrr's
#'   map functions. Please refer to \code{\link[purrr]{map2}} if you are not
#'   familiar with purrr mapping style. Except .x, .y or .l, and .f, other
#'   arguments are optional and control the parallelization processes.
#'
#' Note that except cores, cluster_type, adaptor, auto_export, and splitter,
#'   documentation of other arguments, return section, and examples section
#'   are automatically imported from
#'   \href{https://cran.r-project.org/package=purrr}{purrr} and
#'   \href{https://cran.r-project.org/package=foreach}{foreach} packages.
#'
#' @inheritParams purrr::map2
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::map2 return
#'
#' @export
pa_map2 <- function(.x, .y, .f, ...,
                    cores = NULL,
                    adaptor = "doParallel", cluster_type = NULL,
                    splitter = NULL, auto_export = TRUE, .export = NULL,
                    .packages = NULL, .noexport = NULL,
                    .errorhandling = "stop",
                    .inorder = TRUE,
                    .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_lgl <- function(.x, .y, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_lgl, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_int <- function(.x, .y, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_int, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_dbl <- function(.x, .y, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_dbl, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_chr <- function(.x, .y, .f, ...,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map2_chr, x_split$.x, x_split$.y, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_df <- function(.x, .y, .f, ...,
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
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_dfr <- function(.x, .y, .f, ...,
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
                         .y = .y,
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

#' @rdname pa_map2
#' @export
pa_map2_dfc <- function(.x, .y, .f, ...,
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
                         .y = .y,
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

# #' @rdname pa_map2
# #' @export
# pa_map2_raw <- function(.x, .y, .f, ...,
#                        cores = NULL,
#                        adaptor = "doParallel", cluster_type = NULL,
#                        splitter = NULL, auto_export = TRUE, .export = NULL,
#                        .packages = NULL, .noexport = NULL,
#                        .errorhandling = "stop",
#                        .inorder = TRUE,
#                        .verbose = FALSE) {
#
#   int_fun <- .pa_call(purrr::map_raw, x_split$.x, x_split$.y, .f, ...)
#
#   output <- .pa_internal(.x = .x,
#                          .y = .y,
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
