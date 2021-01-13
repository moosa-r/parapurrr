pa_map2 <- function(.x, .y, .f,
                   cores = NULL, cluster_type = NULL,
                   adaptor = "DoParallel",
                   export_objs = NULL, export_packgs = NULL, no_export = NULL,
                   error_handeling = "stop",
                   in_order = TRUE,
                   verbose = FALSE) {

  int_fun <- quote(purrr::map2(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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


pa_map2_lgl <- function(.x, .y, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map2_lgl(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

pa_map2_int <- function(.x, .y, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map2_int(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

pa_map2_dbl <- function(.x, .y, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map2_dbl(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

pa_map2_chr <- function(.x, .y, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map2_chr(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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

pa_map2_df <- function(.x, .y, .f,
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
                         .y = .y,
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

pa_map2_dfr <- function(.x, .y, .f,
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
                         .y = .y,
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

pa_map2_dfc <- function(.x, .y, .f,
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
                         .y = .y,
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

pa_map2_raw <- function(.x, .y, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map_raw(x$.x, x$.y, .f))

  output <- .pa_internal(.x = .x,
                         .y = .y,
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
