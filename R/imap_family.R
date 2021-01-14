.vec_index <- function(x) {
  #copied directly from purrr's source
  names(x) %||% seq_along(x)
}

pa_imap <- function(.x, .f,
                    cores = NULL, cluster_type = NULL,
                    adaptor = "DoParallel",
                    export_objs = NULL, export_packgs = NULL, no_export = NULL,
                    error_handeling = "stop",
                    in_order = TRUE,
                    verbose = FALSE) {

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}


pa_imap_lgl <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {

  int_fun <- quote(purrr::map2_lgl(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_int <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {

  int_fun <- quote(purrr::map2_int(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_dbl <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {

  int_fun <- quote(purrr::map2_dbl(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_chr <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {

  int_fun <- quote(purrr::map2_chr(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_df <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_df()` requires dplyr.")
  }

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = dplyr::bind_rows,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_dfr <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfr()` requires dplyr.")
  }

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = dplyr::bind_rows,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_dfc <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`map_dfc()` requires dplyr.")
  }

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = dplyr::bind_cols,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}

pa_imap_raw <- function(.x, .f,
                        cores = NULL, cluster_type = NULL,
                        adaptor = "DoParallel",
                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
                        error_handeling = "stop",
                        in_order = TRUE,
                        verbose = FALSE) {

  int_fun <- quote(purrr::map_raw(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .vec_index(.x),
                         .f = .f,
                         int_fun = int_fun,
                         adaptor = adaptor,
                         cores = cores,
                         cluster_type = cluster_type,
                         combine = c,
                         init = NULL,
                         final = NULL,
                         in_order = in_order,
                         multi_combine = TRUE,
                         max_combine = NULL,
                         error_handeling = error_handeling,
                         export_packgs = export_packgs,
                         export_objs = export_objs,
                         no_export = no_export,
                         verbose = verbose)
  return(output)
}
