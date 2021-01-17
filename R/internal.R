#' Handle Main Parallel Arguments
#'
#' Handle cores (worker) numbers, cluster type and splitting indexes of input
#'   vector.
#'
#' @param x_length The length of input atomic vector or list
#' @param cores Number of workers (default: Core numbers - 1)
#' @param adaptor foreach adaptor, current options: doParallel, doSNOW, doFuture
#' @param cluster_type "PSOCK", "FORK", "SOCK", "MPI", "NWS", "multisession",
#'   "multicore", "cluster_FORK", or "cluster_PSOCK"
#'
#' @return a list with core numbers, cluster type and splitting indexes to be
#'   handled to downstream internal functions.
#' @noRd
.pa_args <- function(x_length,
                     cores = NULL,
                     adaptor = "doParallel",
                     cluster_type = NULL) {
  ## handle cluster type
  if (is.null(cluster_type)) {
    cluster_type <- switch(adaptor,
                           "doParallel" = switch(.Platform$OS.type,
                                                 "windows" = "PSOCK",
                                                 "unix" = "FORK"),
                           "doSNOW" = "SOCK",
                           "doFuture" = switch(.Platform$OS.type,
                                               "windows" = "multisession",
                                               "unix" = "multicore"))
  } else {
    switch(adaptor,
           "doParallel" = {
             if (match(cluster_type, c("FORK", "PSOCK"), nomatch = 0) == 0) {
               stop("In doParallel, cluster_type should be 'PSOCK' or 'FORK'.",
                    call. = FALSE)
             }
             if (.Platform$OS.type == "windows" && cluster_type == "FORK") {
               stop("Fork cluster type is not supported in Windows.",
                    call. = FALSE)
             }
           },
           "doSNOW" = {
             if (match(cluster_type, c("MPI", "NWS", "SOCK"), nomatch = 0) == 0 ) {
               stop("In doSNOW, cluster_type should be 'SOCK', 'MPI', or 'NWS'.", call. = FALSE)
             }
           },
           "doFuture" = {
             if (match(cluster_type, c("multisession", "multicore", "cluster_FORK", "cluster_PSOCK"), nomatch = 0) == 0 ) {
               stop("In doFuture, cluster_type (i.e. strategy) should be 'multisession', 'multicore', 'cluster_FORK' or 'cluster_PSOCK'.", call. = FALSE)
             }
             if (.Platform$OS.type == "windows" && (cluster_type == "multicore" || cluster_type == "cluster_FORK")) {
               stop(cluster_type, " strategy is not supported in Windows.", call. = FALSE)
             }
           },
           stop("Adaptor should be 'doParallel', 'doSNOW' or 'doFuture'.", call. = FALSE)
    )
  }

  ### handle cores
  cores <- min(ifelse(is.numeric(cores),
                      yes = cores,
                      no = getOption("pa_cores")),
               x_length)

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


#' Register Clusters
#' Register Clusters necessary for foreach function. currently only doParallel
#'   adaptor is supported, other adaptors will be implemented in the future.
#' @param adaptor foreach adaptor. Currently, parapurr supports: \enumerate{
#' \item "doParallel" \item "doSNOW" \item "doFuture"}
#' @param cores number of cores (i.e. workers)
#' @param cluster_type "PSOCK", "FORK", "SOCK", "MPI", "NWS", "multisession",
#'   "multicore", "cluster_FORK", or "cluster_PSOCK"
#'
#' @return a list containing cluster object (to be used later for terminating
#'   the cluster) and the adaptor name.
#' @noRd
.pa_reg_clusters <- function(adaptor, cores, cluster_type) {
  switch(adaptor,
         "doParallel" = {
           if (!requireNamespace("doParallel", quietly = TRUE)) {
             stop("Package 'doParallel' is required to be installed.")
           }
           cl <- parallel::makeCluster(spec = cores,
                                       type = cluster_type)
           doParallel::registerDoParallel(cl)
         },
         "doSNOW" = {
           if (!requireNamespace("doSNOW", quietly = TRUE)) {
             stop("Package 'doSNOW' is required to be installed. Also: \n",
                  "If you intend to use MPI clusters: package \"Rmpi\" should be installed.\n",
                  "If you intend to use NWS clusters: package \"nws\" should be installed.")
           }
           cl <- snow::makeCluster(spec = cores,
                                   type = cluster_type)
           doSNOW::registerDoSNOW(cl)
         },
         "doFuture" = {
           if (!requireNamespace("doFuture", quietly = TRUE)) {
             stop("Package 'doFuture' is required to be installed.")
           }
           doFuture::registerDoFuture()
           if (cluster_type == "cluster_FORK") {
             cl <- parallel::makeCluster(spec = cores,
                                         type = "FORK")
             future::plan(strategy = "cluster",
                          workers = cl)
           } else if (cluster_type == "cluster_PSOCK") {
             cl <- parallel::makeCluster(spec = cores,
                                         type = "PSOCK")
             future::plan(strategy = "cluster",
                          workers = cl)
           } else {
             cl <- NA
             future::plan(strategy = cluster_type,
                          workers = cores)
           }
         },
         stop("Adaptor should be 'doParallel', 'doSNOW' or 'doFuture'.", call. = FALSE)
  )
  return(list("cluster" = cl,
              "adaptor" = adaptor,
              "cluster_type" = cluster_type))
}


#' Terminate active cluster
#'
#' @param active_cl cluster object returned by .pa_reg_clusters function
#'
#' @return NULL, stops clusters as a side-effect.
#' @noRd
.pa_stop_clusters <- function(active_cl) {
  switch(active_cl$adaptor,
         "doParallel" = {
           doParallel::stopImplicitCluster()
           parallel::stopCluster(active_cl$cluster)
         },
         "doSNOW" = {
           snow::stopCluster(active_cl$cluster)
         },
         "doFuture" = {
           if (inherits(active_cl$cluster, "cluster")) {
             parallel::stopCluster(active_cl$cluster)
           }
           future::plan("sequential")
         }
  )

  invisible()
}

#' Internal backbone of parapurrr functions
#' This function is the only function that will be called by map, map2, and
#'   imap function families. every necessary steps from handling user input,
#'   registering and subsequently terminating the parallel cluster and up to
#'   handling the results to the user will be performed by .pa_internal.
#'   except int_fun, other arguments will be handled to either purrr's mapping
#'   function, parallel cluster registering function or foreach function.
#' @param .x refer to \code{\link[purrr]{map}} and \code{\link[purrr]{imap}}
#' @param .y refer to \code{\link[purrr]{map2}}
#' @param .f refer to \code{\link[purrr]{map}}, \code{\link[purrr]{imap}} and
#'   \code{\link[purrr]{map2}}
#' @param int_fun After the input has been splitted into multiple chunks,
#'   this purrr mapping expression will be applied to each chunk.
#' @param adaptor refer to \code{\link{.pa_reg_clusters}}
#' @param cores refer to \code{\link{.pa_reg_clusters}}
#' @param cluster_type refer to \code{\link{.pa_reg_clusters}}
#' @param auto_export Should variables in the calling environment be exported?
#' @param .combine refer to \code{\link[foreach]{foreach}}
#' @param .init refer to \code{\link[foreach]{foreach}}
#' @param .final refer to \code{\link[foreach]{foreach}}
#' @param .inorder refer to \code{\link[foreach]{foreach}}
#' @param .multicombine refer to \code{\link[foreach]{foreach}}
#' @param .maxcombine refer to \code{\link[foreach]{foreach}}
#' @param .errorhandling refer to \code{\link[foreach]{foreach}}
#' @param .packages refer to \code{\link[foreach]{foreach}}
#' @param .export refer to \code{\link[foreach]{foreach}}
#' @param .noexport refer to \code{\link[foreach]{foreach}}
#' @param .verbose refer to \code{\link[foreach]{foreach}}
#'
#' @return an object similar to what could be expected from the results of
#'   int_fun argument
#' @importFrom foreach %dopar%
#' @noRd
.pa_internal <- function(.x,
                         .y,
                         .f,
                         int_fun,
                         adaptor,
                         cores,
                         cluster_type,
                         auto_export,
                         .combine,
                         .init,
                         .final,
                         .inorder,
                         .multicombine,
                         .maxcombine,
                         .errorhandling,
                         .packages,
                         .export,
                         .noexport,
                         .verbose) {
  # Check arguments
  if (!(length(.inorder) == 1L && !is.na(.inorder) && is.logical(.inorder))) {
    stop(".inorder should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!(length(.verbose) == 1L && !is.na(.verbose) && is.logical(.verbose))) {
    stop(".verbose should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!is.null(.y) && length(.x) != length(.y)) {
    stop(sprintf("Mapped vectors should have equal lengths.\n(the length of'.x' is %s and '.y' is %s)",
                 length(.x), length(.y)),
         call. = FALSE)
  }

  if (!(length(.errorhandling) == 1L &&
        match(.errorhandling, c("stop", "remove", "pass"), nomatch = 0) > 0)) {
    stop(".errorhandling should be 'stop', 'remove' or 'pass'.",
         call. = FALSE)
  }

  if (!is.null(.packages) && !is.character(.packages)) {
    stop(".packages should be a character vector.")
  }

  if (!is.null(.export) && !is.character(.export)) {
    stop(".export should be a character vector.")
  }
  if (!is.null(.noexport) && !is.character(.noexport)) {
    stop(".noexport should be a character vector.")
  }
  if (!(length(auto_export) == 1L && !is.na(auto_export) && is.logical(auto_export))) {
    stop("auto_export should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  # cluster arguments
  int_args <- .pa_args(x_length = length(.x),
                       cores = cores,
                       adaptor = adaptor,
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
  # update export:
  if (auto_export) {
    .export = unique(c(.export, ls(envir = parent.frame(2)), ".f"))
  } else {
    .export = c(.export, ".f")
  }
  # perform!
  output <- foreach::foreach(x = foreach_input,
                             .combine = .combine,
                             .init = .init,
                             .final = .final,
                             .inorder = .inorder,
                             .multicombine = .multicombine,
                             .maxcombine = max(2,
                                               .maxcombine,
                                               int_args$cores),
                             .errorhandling = .errorhandling,
                             .packages = .packages,
                             .export = .export,
                             .noexport = c(.noexport,
                                           ".x",
                                           ".y",
                                           "int_args",
                                           "cl",
                                           "cores",
                                           "cluster_type",
                                           ".combine",
                                           ".init",
                                           ".final",
                                           ".inorder",
                                           ".multicombine",
                                           ".maxcombine",
                                           ".errorhandling",
                                           ".packages",
                                           ".export",
                                           ".noexport",
                                           ".verbose"),
                             .verbose = .verbose) %dopar% {
                               output = eval(int_fun)
                               return(output)
                             }
  return(output)
}
