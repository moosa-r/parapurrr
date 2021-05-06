#' Parallel Version of purrr map_if and map_at function
#'
#' The syntax and the logic of pa_walk functions are the identical to purrr's
#'   walk function. Please refer to \code{\link[purrr]{map}} if you are not
#'   familiar with purrr mapping style. Except .x and .f, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreach function and the selected forreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::map_if
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::map_if return
#'
#' @export
pa_map_if <- function(.x, .p, .f, .else = NULL,
                      cores = NULL,
                      adaptor = "doParallel", cluster_type = NULL,
                      splitter = NULL, auto_export = TRUE, .export = NULL,
                      .packages = NULL, .noexport = NULL,
                      .errorhandling = "stop",
                      .inorder = TRUE,
                      .verbose = FALSE) {

  int_fun <- quote(purrr::map(x, .f))

  sel <- probe(.x, .p)
  output <- purrr::list_along(.x)

  output[sel] <- .pa_internal(.x = .x[sel],
                              .y = NULL,
                              .f = .f,
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

  if (purrr::is_null(.else)) {
    output[!sel] <- .x[!sel]
  } else {
    output[!sel] <- .pa_internal(.x = .x[!sel],
                                 .y = NULL,
                                 .f = .else,
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
  }
  output <- purrr::set_names(output, names(.x))

  return(output)
}

#' @rdname pa_map_if
#' @export
pa_map_at <- function(.x, .at, .f,
                      cores = NULL,
                      adaptor = "doParallel", cluster_type = NULL,
                      splitter = NULL, auto_export = TRUE, .export = NULL,
                      .packages = NULL, .noexport = NULL,
                      .errorhandling = "stop",
                      .inorder = TRUE,
                      .verbose = FALSE) {

  int_fun <- quote(purrr::map(x, .f))

  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  output <- purrr::list_along(.x)

  output[sel] <- .pa_internal(.x = .x[sel],
                              .y = NULL,
                              .f = .f,
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

  output[!sel] <- .x[!sel]
  output <- purrr::set_names(output, names(.x))

  return(output)
}
