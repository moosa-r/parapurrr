pa_walk <- function(.x, .f,
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
  invisible(output)
}

pa_walk2 <- function(.x, .y, .f,
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
  invisible(output)
}

pa_iwalk <- function(.x, .f,
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
  invisible(output)
}
