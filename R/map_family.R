pa_map <- function(.x, .f,
                   cores = NULL, cluster_type = NULL,
                   adaptor = "DoParallel",
                   export_objs = NULL, export_packgs = NULL, no_export = NULL,
                   error_handeling = "stop",
                   in_order = TRUE,
                   verbose = FALSE) {

  int_fun <- quote(purrr::map(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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


pa_map_lgl <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map_lgl(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

pa_map_int <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map_int(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

pa_map_dbl <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map_dbl(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

pa_map_chr <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {

  int_fun <- quote(purrr::map_chr(x, .f))

  output <- .pa_internal(.x = .x,
                         .y = NULL,
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

pa_map_df <- function(.x, .f,
                      cores = NULL, cluster_type = NULL,
                      adaptor = "DoParallel",
                      export_objs = NULL, export_packgs = NULL, no_export = NULL,
                      error_handeling = "stop",
                      in_order = TRUE,
                      verbose = FALSE) {
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

pa_map_dfr <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {
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

pa_map_dfc <- function(.x, .f,
                       cores = NULL, cluster_type = NULL,
                       adaptor = "DoParallel",
                       export_objs = NULL, export_packgs = NULL, no_export = NULL,
                       error_handeling = "stop",
                       in_order = TRUE,
                       verbose = FALSE) {
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

# pa_map_raw <- function(.x, .f,
#                        cores = NULL, cluster_type = NULL,
#                        adaptor = "DoParallel",
#                        export_objs = NULL, export_packgs = NULL, no_export = NULL,
#                        error_handeling = "stop",
#                        in_order = TRUE,
#                        verbose = FALSE) {
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
#                          combine = c,
#                          init = NULL,
#                          final = NULL,
#                          in_order = in_order,
#                          multi_combine = TRUE,
#                          max_combine = NULL,
#                          error_handeling = error_handeling,
#                          export_packgs = export_packgs,
#                          export_objs = export_objs,
#                          no_export = no_export,
#                          verbose = verbose)
#   return(output)
# }
