#' Parallel Version of purrr walk function
#'
#' The syntax and the logic of pa_walk functions are the identical to purrr's
#'   walk function. Please refer to \code{\link[purrr]{walk}} if you are not
#'   familiar with purrr mapping style. Except .x and .f, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreach function and the selected forreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::walk
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::walk return
#'
#' @export
pa_walk <- function(.x, .f,
                    cores = NULL,
                    adaptor = "doParallel", cluster_type = NULL,
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
  invisible(output)
}

#' Parallel Version of purrr walk2 function
#'
#' The syntax and the logic of pa_walk functions are the identical to purrr's
#'   walk function. Please refer to \code{\link[purrr]{walk2}} if you are not
#'   familiar with purrr mapping style. Except .x, .y, .f, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreach function and the selected forreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::walk2
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::walk2 return
#'
#' @export
pa_walk2 <- function(.x, .y, .f,
                     cores = NULL,
                     adaptor = "doParallel", cluster_type = NULL,
                     .export = NULL, .packages = NULL, .noexport = NULL,
                     .errorhandling = "stop",
                     .inorder = TRUE,
                     .verbose = FALSE) {

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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
  invisible(output)
}

#' Parallel Version of purrr walk function
#'
#' The syntax and the logic of pa_walk functions are the identical to purrr's
#'   walk function. Please refer to \code{\link[purrr]{iwalk}} if you are not
#'   familiar with purrr mapping style. Except .x and .f, other arguments are
#'   optional and control the parallelization processes. They will be handled
#'   to foreach function and the selected forreach adaptor.
#'
#' Note that except cores, cluster_type, and adaptor, documentation of other
#'   arguments, return section, and examples section are automatically imported
#'   from purrr and foreach packages.
#'
#' @inheritParams purrr::walk
#' @inheritParams foreach::foreach
#' @inheritParams pa_map
#'
#' @inherit purrr::iwalk return
#'
#' @export
pa_iwalk <- function(.x, .f,
                     cores = NULL,
                     adaptor = "doParallel", cluster_type = NULL,
                     .export = NULL, .packages = NULL, .noexport = NULL,
                     .errorhandling = "stop",
                     .inorder = TRUE,
                     .verbose = FALSE) {

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
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
  invisible(output)
}
