.pa_args <- function(x_length, cores = NULL, cluster_type = NULL) {
  cores <- min(ifelse(is.numeric(cores),
                      yes = cores,
                      no = getOption("pa_cores")),
               x_length)
  ## set main arguments
  if (is.null(cluster_type)) {
    cluster_type <- getOption("pa_cluster_type")
  } else {
    if (!(cluster_type == "FORK" || cluster_type == "PSOCK")) {
      stop("cluster_type should be 'PSOCK' or 'FORK'.", call. = FALSE)
    }
    if (getOption("pa_os") == "windows" && cluster_type == "FORK") {
      stop("Fork cluster is not supported in Windows.", call. = FALSE)
    }
  }

  ### divide the input
  if (cores <= 1) {
    parts <- list("1" = seq_len(x_length))
  } else {
    parts <- split(x = seq_len(x_length),
                   f = cut(seq_len(x_length), cores, labels = FALSE))
  }

  return(list("cores" = cores,
              "cluster_type" = cluster_type,
              "parts" = parts))
}


.pa_reg_clusters <- function(adaptor, cores, cluster_type) {
  if (!(length(adaptor) == 1L &&
        match(adaptor, c("DoParallel"), nomatch = 0) > 0) ) {
    stop("adaptor should be 'DoParallel'.", call. = FALSE)
  }
  switch(adaptor,
         "DoParallel" = {
           if (!requireNamespace("doParallel", quietly = TRUE)) {
             stop("Package 'doParallel' is required to be installed.")
           }
           cl = parallel::makeCluster(spec = cores,
                                      type = cluster_type)
           doParallel::registerDoParallel(cl)
           return(list("cluster" = cl,
                       "adaptor" = "DoParallel"))
         })
}


.pa_stop_clusters <- function(active_cl) {
  switch(active_cl$adaptor,
         "DoParallel" = {
           doParallel::stopImplicitCluster()
           parallel::stopCluster(active_cl$cluster)})

  invisible()
}


.pa_internal <- function(.x,
                         .y,
                         .f,
                         int_fun,
                         adaptor,
                         cores,
                         cluster_type,
                         combine,
                         init,
                         final,
                         in_order,
                         multi_combine,
                         max_combine,
                         error_handeling,
                         export_packgs,
                         export_objs,
                         no_export,
                         verbose) {
  # Check arguments
  if (!(length(in_order) == 1L && !is.na(in_order) && is.logical(in_order))) {
    stop("in_order should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!(length(verbose) == 1L && !is.na(verbose) && is.logical(verbose))) {
    stop("verbose should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!is.null(.y) && length(.x) != length(.y)) {
    stop(sprintf("Mapped vectors should have equal lengths.\n(the length of'.x' is %s and '.y' is %s)",
                 length(.x), length(.y)),
         call. = FALSE)
  }

  if (!(length(error_handeling) == 1L &&
        match(error_handeling, c("stop", "remove", "pass"), nomatch = 0) > 0)) {
    stop("error_handeling should be 'stop', 'remove' or 'pass'.",
         call. = FALSE)
  }

  if (!is.null(export_packgs) && !is.character(export_packgs)) {
    stop("export_packgs should be a character vector.")
  }

  if (!is.null(export_objs) && !is.character(export_objs)) {
    stop("export_objs should be a character vector.")
  }
  if (!is.null(no_export) && !is.character(no_export)) {
    stop("no_export should be a character vector.")
  }
  # cluster arguments
  int_args <- .pa_args(x_length = length(.x),
                       cores = cores,
                       cluster_type = cluster_type)
  # split the input
  foreach_input <- lapply(X = int_args$parts,
                          FUN = function(part_index) {
                            if (is.null(.y)) {
                              return(.x[part_index])
                            } else {
                              return(list(.x = .x[part_index],
                                          .y = .y[part_index]))
                            }
                          })
  # register cluster
  cl <- .pa_reg_clusters(adaptor = adaptor,
                         cores = int_args$cores,
                         cluster_type = int_args$cluster_type)
  on.exit(.pa_stop_clusters(cl))
  # perform!
  output <- foreach::foreach(x = foreach_input,
                             .combine = combine,
                             .init = init,
                             .final = final,
                             .inorder = in_order,
                             .multicombine = multi_combine,
                             .maxcombine = max(2,
                                               max_combine,
                                               int_args$cores),
                             .errorhandling = error_handeling,
                             .packages = export_packgs,
                             .export = c(export_objs, ".f"),
                             .noexport = c(no_export,
                                           ".x",
                                           ".y",
                                           "int_args",
                                           "cl",
                                           "cores",
                                           "cluster_type",
                                           "combine",
                                           "init",
                                           "final",
                                           "in_order",
                                           "multi_combine",
                                           "max_combine",
                                           "error_handeling",
                                           "export_packgs",
                                           "export_objs",
                                           "no_export",
                                           "verbose"),
                             .verbose = verbose) %dopar% {
                               output = eval(int_fun)
                               return(output)
                             }
  return(output)
}
