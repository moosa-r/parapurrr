#' Parallel Version of purrr map family
#'
#' The syntax and the logic of pa_map* functions are the identical to purrr's
#'   map functions. Please refer to \code{\link[purrr]{map}} if you are not
#'   familiar with purrr mapping style. Except .x and .f, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreach function and the selected foreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param cores (optional) Number of cores (i.e. workers) to be used. Default
#'   is: your computer CPU cores - 1
#' @param adaptor The foreach adaptor to be used. Available options are:
#'   \itemize{\item "doParallel" (default) \item "doMPI" \item "doSNOW"
#'   \item "doFuture"}
#' @param cluster_type The Clusters architecture to be used with the selected
#'   adaptor. Note that allowed values for this argument depends on the
#'   "adaptor" argument: \enumerate{
#'   \item if adaptor is doParallel":
#'     \itemize{
#'       \item in windows OS: "PSOCK" (default for Windows)
#'       \item in Unix-based OS: "FORK" (default for Unix), "PSOCK"
#'       }
#'   \item if adaptor is doMPI:
#'     \itemize{
#'       \item No cluster_type options here, let cluster_type be NULL
#'       }
#'   \item if adaptor is "doSNOW":
#'     \itemize{
#'     \item in windows OS: "SOCK" (default for Windows)
#'     \item in Unix-based OS: "MPI" (default for Unix), "NWS", "SOCK"
#'     }
#'   \item if adaptor is "doFuture":
#'     \itemize{
#'     \item in windows OS: "multisession" (default for Windows),
#'       "cluster_PSOCK"
#'     \item in Unix-based OS: "multicore" (default for Unix), "multisession",
#'       "cluster_FORK", "cluster_PSOCK"
#'     }
#'   }
#' @param auto_export (default = TRUE) Should variables in the function's
#'   calling environment be exported to workers? Default is set to TRUE for
#'   convenience; But to improve the performance, consider turning auto_export
#'   off and manually setting the exported variables using .export argument.
#' @param splitter (optional) Use this argument to instruct parapurrr how to
#'   handle your input elements to the available cores. The Splitter is a
#'   list where each of its elements is a numeric vector. Each numeric vector
#'   corresponds to a core, and the elements of each vector should be indexes
#'   of .x input's elements; Collectively they should have a one-to-one
#'   correspondence with .x indexes. See the Vignettes for further explanations
#'   and examples.
#'
#' @inherit purrr::map return
#' @export
pa_map <- function(.x, .f, ...,
                   cores = NULL,
                   adaptor = "doParallel", cluster_type = NULL,
                   splitter = NULL, auto_export = TRUE, .export = NULL,
                   .packages = NULL, .noexport = NULL,
                   .errorhandling = "stop",
                   .inorder = TRUE,
                   .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_lgl <- function(.x, .f, ...,
                       cores = NULL,
                       adaptor = "doParallel", cluster_type = NULL,
                       splitter = NULL, auto_export = TRUE, .export = NULL,
                       .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map_lgl, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_int <- function(.x, .f, ...,
                       cores = NULL,
                       adaptor = "doParallel", cluster_type = NULL,
                       splitter = NULL, auto_export = TRUE, .export = NULL,
                       .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map_int, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_dbl <- function(.x, .f, ...,
                       cores = NULL,
                       adaptor = "doParallel", cluster_type = NULL,
                       splitter = NULL, auto_export = TRUE, .export = NULL,
                       .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map_dbl, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_chr <- function(.x, .f, ...,
                       cores = NULL,
                       adaptor = "doParallel", cluster_type = NULL,
                       splitter = NULL, auto_export = TRUE, .export = NULL,
                       .packages = NULL, .noexport = NULL,
                       .errorhandling = "stop",
                       .inorder = TRUE,
                       .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map_chr, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_df <- function(.x, .f, ...,
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

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_dfr <- function(.x, .f, ...,
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

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

#' @rdname pa_map
#' @export
pa_map_dfc <- function(.x, .f, ...,
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

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

# #' @rdname pa_map
# #' @export
# pa_map_raw <- function(.x, .f, ...,
#                        cores = NULL,
#                        adaptor = "doParallel", cluster_type = NULL,
#                        splitter = NULL, auto_export = TRUE, .export = NULL,
#                        .packages = NULL, .noexport = NULL,
#                        .errorhandling = "stop",
#                        .inorder = TRUE,
#                        .verbose = FALSE) {
#
#   int_fun <- .pa_call(purrr::map_raw, x_split, .f, ...)
#
#   output <- .pa_internal(.x = .x,
#                          .y = NULL,
#                          .l = NULL,
#                          .f = .f,
#                          ... = ...,
#                          int_fun = int_fun,
#                          adaptor = adaptor,
#                          cores = cores,
#                          cluster_type = cluster_type,
#                          auto_export = auto_export,
#                           splitter = splitter,
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
