#' Check if .l input is valid
#'
#' Check if the .l argument provided for pmap family functions is valid; i.e.
#'   each of its elements have the same length or length 1, and if elements
#'   with length one are present, recycle them
#'
#' @param .l .l argument provided for pmap family functions
#'
#' @return .l as provided with some elements recycled if necessary
#' @noRd
.l_recycler = function(.l) {
  lengs <- lengths(.l)
  if (length(unique(lengs)) == 1) {
    return(.l)

  } else {

    len_one <- which(lengs == 1)
    if (length(len_one) == 0) {
      stop("The elements of .l should all be of equal lenghes, or length 1 to be recycled.", call. = FALSE)
    } else {
      other_lengs <- unique(lengths(.l[-len_one]))
      if (length(other_lengs) == 1) {
        .l <- purrr::modify_at(.x = .l, .at = len_one,
                               .f = ~rep(.x, other_lengs))
        return(.l)
      } else {
        stop("The elements of .l should all be of equal lenghes, or length 1 to be recycled.", call. = FALSE)
      }
    }
  }
}

#' @rdname pa_map2
#' @export
pa_pmap <- function(.l, .f,
                    cores = NULL,
                    adaptor = "doParallel", cluster_type = NULL,
                    splitter = NULL, auto_export = TRUE, .export = NULL,
                    .packages = NULL, .noexport = NULL,
                    .errorhandling = "stop",
                    .inorder = TRUE,
                    .verbose = FALSE) {

  int_fun <- quote(purrr::pmap(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  return(output)
}

#' @rdname pa_map2
#' @export
pa_pmap_lgl <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- quote(purrr::pmap_lgl(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  return(output)
}

#' @rdname pa_map2
#' @export
pa_pmap_int <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- quote(purrr::pmap_int(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  return(output)
}

#' @rdname pa_map2
#' @export
pa_pmap_dbl <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- quote(purrr::pmap_dbl(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  return(output)
}

#' @rdname pa_map2
#' @export
pa_pmap_chr <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  int_fun <- quote(purrr::pmap_chr(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  return(output)
}

#' @rdname pa_map2
#' @export
pa_pmap_df <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`pmap_df()` requires dplyr.")
  }

  int_fun <- quote(purrr::pmap(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
                         .f = .f,
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
pa_pmap_dfr <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`pmap_dfr()` requires dplyr.")
  }

  int_fun <- quote(purrr::pmap(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
                         .f = .f,
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
pa_pmap_dfc <- function(.l, .f,
                        cores = NULL,
                        adaptor = "doParallel", cluster_type = NULL,
                        splitter = NULL, auto_export = TRUE, .export = NULL,
                        .packages = NULL, .noexport = NULL,
                        .errorhandling = "stop",
                        .inorder = TRUE,
                        .verbose = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`pmap_dfr()` requires dplyr.")
  }

  int_fun <- quote(purrr::pmap(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
                         .f = .f,
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

#' @rdname pa_map2
#' @export
pa_pwalk <- function(.l, .f,
                    cores = NULL,
                    adaptor = "doParallel", cluster_type = NULL,
                    splitter = NULL, auto_export = TRUE, .export = NULL,
                    .packages = NULL, .noexport = NULL,
                    .errorhandling = "stop",
                    .inorder = TRUE,
                    .verbose = FALSE) {

  int_fun <- quote(purrr::pmap(x, .f))

  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }

  output <- .pa_internal(.x = NULL,
                         .y = NULL,
                         .l = .l,
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
  invisible(.l)
}
