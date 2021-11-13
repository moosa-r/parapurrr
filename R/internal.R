### Arguments Checks ####

#' Argument check for user's input
#'
#' @return nothing, stop the code in case of error
#' @noRd
.pa_arg_check = function(.errorhandling,
                         .export,
                         .inorder,
                         .l,
                         .noexport,
                         .packages,
                         .verbose,
                         .x,
                         .y,
                         adaptor,
                         auto_export,
                         cluster_type,
                         cores) {

  if (!(length(.inorder) == 1L && !is.na(.inorder) && is.logical(.inorder))) {
    stop(".inorder should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!(length(.verbose) == 1L && !is.na(.verbose) && is.logical(.verbose))) {
    stop(".verbose should be 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!is.null(.l)) {
    if (!(is.list(.l) && all(purrr::map_lgl(.l, is.vector)))) {
      stop(".l should be A list of vectors.", call. = FALSE)
    }
    .l <- .l_recycler(.l)
  }

  if (!is.null(.y) && length(.x) != length(.y)) {
    stop(sprintf("Mapped vectors should have equal lengths.\n(the length of '.x' is %s and '.y' is %s)",
                 length(.x), length(.y)),
         call. = FALSE)
  }

  if (!(length(.errorhandling) == 1L &&
        match(.errorhandling, c("stop", "remove", "pass"), nomatch = 0) > 0)) {
    stop(".errorhandling should be 'stop', 'remove' or 'pass'.",
         call. = FALSE)
  }

  if (!is.null(.packages) && !is.character(.packages)) {
    stop(".packages should be a character vector.", call. = FALSE)
  }

  if (!is.null(.export) && !is.character(.export)) {
    stop(".export should be a character vector.", call. = FALSE)
  }

  if (!is.null(.noexport) && !is.character(.noexport)) {
    stop(".noexport should be a character vector.", call. = FALSE)
  }

  if (!(length(auto_export) == 1L &&
        (auto_export == "all" || ( !is.na(auto_export) &&
                                   is.logical(auto_export)))
  )) {
    stop("auto_export should be, \"all\", 'TRUE' or 'FALSE'.", call. = FALSE)
  }

  if (!is.null(cores) &&
      !(rlang::is_integerish(cores) && cores >= 1)) {
    stop("Cores should be an integer or integer-like (with no decimal) number, equal to or greater than 1.",
         call. = FALSE)
  }

  if (getOption("pa_manual_backend") && !is.null(adaptor)) {
    warning("adaptor = ", adaptor,
            " is ignored, because forcing manual backend handling was enabled.\n",
            "To revert that and re-enable automatic backend registeration, run:",
            "manual_backend(FALSE)",
            immediate. = TRUE, call. = FALSE)
  }

  invisible()
}

#' Check User-Provided Splitter
#'
#' Check if the custom splitter provided by the user is valid; i.e. it is a
#'   list with numeric elements and its elements have a one-to-one
#'   correspondence with the indexes of .x elements.
#'
#' @param x_length Length of .x argument
#' @param splitter User-provided splitter. A list with numeric vectors.
#'
#' @return Nothing! If the splitter is not valid, code execution will be
#'   stopped.
#' @noRd
.splitter_check <- function(x_length, splitter) {
  ss <- unlist(splitter)
  if (!is.list(splitter) ||
      !rlang::is_integerish(ss)) {
    stop("Splitter should be a list where each of its elements is an integer or integer-like (without decimal points) vector.",
         call. = FALSE)
  }
  ss <- as.integer(ss)
  if (length(ss) != x_length ||
      !setequal(ss, seq_len(x_length))
  ) {
    stop("Splitter contents should have a one-to-one correspondence with the indexes of .x elements.",
         call. = FALSE)
  }
  invisible()
}

#### Handle Variables ####

#' Update export list with objects used in the .f
#'
#' To mimic automatic variable exporting in foreach, this function will
#'   add any object from the calling environment that is being called within
#'   the .f supplied by the user.
#'
#' @param env (environment) The evaluation environment of foreach
#' @param .f (function) as_mapper version of .f supplied by the user
#' @param auto_export (logical) auto_export argument of the exported functions
#' @param .export (Character) .export argument of the exported function
#'
#' @return A character vector
#' @noRd
.pa_export <- function(env, .f, .export, ...) {
  funs <- as.character(c(enquote(.f),
                         substitute(list(...))
  ))
  objs <- rlang::env_names(env)
  objs <- objs[vapply(X = objs,
                      FUN = function(string) {
                        any(grepl(pattern = sprintf("(?<!(\\.|\\w|\\\"|\\\'))%s(?!(\\.|\\w|\\\"|\\\'))", string),
                                  perl = TRUE,
                                  x = funs))
                      },
                      FUN.VALUE = logical(1))]
  .export <- unique(c(.export, objs))
  return(.export)
}

#' Create a call to be applied to input segments
#'
#' Creates a call object to be later evaluated in parallel and be applied
#'   to each splitted input.
#'
#' It handles ellipsis in the exported function's environment not in the
#'   spawned/forked instances, thus it is safer and less bug-prone.
#'
#' @param fun (class = function) a function to be passed to "what" argument in
#'   do.call
#' @param ... arguments to be passed to "arg" argument in do.call
#'
#' @return A call object
#' @noRd
.pa_call <- function(fun, ...) {
  call("do.call",
       what = substitute(fun),
       args = substitute(list(...)))
}

#' Handle Main Parallel Arguments with manual backend registering
#'
#' Handle cores (worker) numbers, cluster type and splitting indexes of input
#'   vector.
#'
#' @param x_length The length of input atomic vector or list
#' @param cores Number of workers (default: Core numbers - 1)
#' @param adaptor foreach adaptor, current options: doMPI, doParallel (default),
#'   doSNOW, doFuture, doMC
#' @param splitter User-provided splitter. A list with numeric vectors.
#'
#' @return A list with core numbers, cluster type and splitting indexes to be
#'   handled to downstream internal functions.
#' @noRd
.pa_args_manual <- function(x_length,
                            user_cores = NULL,
                            adaptor = NULL,
                            splitter = NULL) {

  ### Handle number of cores
  if (foreach::getDoParRegistered() &&
      foreach::getDoParName() != "doSEQ") {
    cluster_type <- foreach::getDoParName()
    cores <- as.integer(foreach::getDoParWorkers())
    if (!is.null(user_cores) && user_cores != cores) {
      warning(sprintf("You have supplied `cores = %s` but registered a doPar backend `%s`, with `%s` workers",
                      user_cores, cluster_type, cores),
              call. = FALSE)
    }
  } else {
    cores <- 1
    cluster_type <- "Sequential"
    if (is.null(adaptor) || getOption("pa_manual_backend")) {
      warning("By calling manual_backend(TRUE) or providing adaptor = NULL, ",
              "you have forced manual doPar backend handeling;\n",
              "But you did `not` register any backends before calling this function:\n",
              "*** Running your code in `Sequential` mode. ***",
              immediate. = TRUE, call. = FALSE)
    }
  }

  ### split the input
  if (!is.null(splitter) && length(splitter) != cores) {
    warning("Cores and splitter's lengths are inconsistent. Ignoring your provided splitter.",
            immediate. = TRUE, call. = FALSE)
    splitter <- NULL
  }

  if (is.null(splitter)) {
    if (cores > 1) {
      parts <- split(x = seq_len(x_length),
                     f = cut(seq_len(x_length), cores, labels = FALSE))
    } else {
      parts <- list("1" = seq_len(x_length))
    }
  } else {
    .splitter_check(x_length = x_length, splitter = splitter)
    parts <- splitter
  }

  return(list("cores" = cores,
              "cluster_type" = cluster_type,
              "parts" = parts))
}



#' Handle Main Parallel Arguments
#'
#' Handle cores (worker) numbers, cluster type and splitting indexes of input
#'   vector.
#'
#' @param x_length The length of input atomic vector or list
#' @param cores Number of workers (default: Core numbers - 1)
#' @param adaptor foreach adaptor, current options: doMPI, doParallel (default),
#'   doSNOW, doFuture, doMC
#' @param cluster_type "PSOCK", "FORK", "SOCK", "MPI", "multisession",
#'   "multicore", "cluster_FORK", or "cluster_PSOCK"
#' @param splitter User-provided splitter. A list with numeric vectors.
#'
#' @return A list with core numbers, cluster type and splitting indexes to be
#'   handled to downstream internal functions.
#' @noRd
.pa_args <- function(x_length,
                     cores = NULL,
                     adaptor = "doParallel",
                     cluster_type = NULL,
                     splitter = NULL) {
  ## handle cluster type
  if (is.null(cluster_type)) {
    cluster_type <- switch(adaptor,
                           "doMPI" = NULL,
                           "doParallel" = switch(.Platform$OS.type,
                                                 "windows" = "PSOCK",
                                                 "unix" = "FORK"),
                           "doSNOW" = "SOCK",
                           "doFuture" = switch(.Platform$OS.type,
                                               "windows" = "multisession",
                                               "unix" = "multicore"),
                           "doMC" = NULL)

  } else {

    switch(adaptor,
           "doMPI" = {
             if (!is.null(cluster_type)) {
               warning("Provided cluster_type is ignored when using doMPI.",
                       call. = FALSE)
               cluster_type <- NULL
             }
           },
           "doParallel" = {
             if (match(cluster_type,
                       c("FORK", "PSOCK"), nomatch = 0) == 0) {
               stop("In doParallel, cluster_type should be 'PSOCK' or 'FORK'.",
                    call. = FALSE)
             }
             if (.Platform$OS.type == "windows" && cluster_type == "FORK") {
               stop("Fork cluster type is not supported in Windows.",
                    call. = FALSE)
             }
           },
           "doSNOW" = {
             if (match(cluster_type,
                       c("MPI", "SOCK"),
                       nomatch = 0) == 0) {
               stop("In doSNOW, cluster_type should be 'SOCK' or 'MPI'.",
                    call. = FALSE)
             }
           },
           "doFuture" = {
             if (match(cluster_type,
                       c("multisession", "multicore",
                         "cluster_FORK", "cluster_PSOCK"),
                       nomatch = 0) == 0) {
               stop("In doFuture, cluster_type (i.e. strategy) should be 'multisession', 'multicore', 'cluster_FORK' or 'cluster_PSOCK'.",
                    call. = FALSE)
             }
             if (.Platform$OS.type == "windows" &&
                 (cluster_type == "multicore" ||
                  cluster_type == "cluster_FORK")) {
               stop(cluster_type, " strategy is not supported in Windows.",
                    call. = FALSE)
             }
           },
           "doMC" = {
             if (!is.null(cluster_type)) {
               warning("Provided cluster_type is ignored when using doMC.",
                       call. = FALSE)
               cluster_type <- NULL
             }
           },
           stop("Invalid adaptor.",
                call. = FALSE)
    )

  }

  ### divide the input
  if (is.null(splitter)) {
    ### handle cores
    cores <- min(ifelse(rlang::is_integerish(x = cores, n = 1),
                        yes = cores,
                        no = getOption("pa_cores")),
                 x_length)
    if (cores > 1) {
      parts <- split(x = seq_len(x_length),
                     f = cut(seq_len(x_length), cores, labels = FALSE))
    } else {
      parts <- list("1" = seq_len(x_length))
    }
  } else {
    .splitter_check(x_length = x_length, splitter = splitter)
    parts <- splitter
    ### handle cores
    if (rlang::is_integerish(cores) && length(splitter) != cores) {
      warning("Cores and splitter's lengths are inconsistent, using the splitter's length as cores: ", length(splitter),
              immediate. = TRUE, call. = FALSE)
    }
    cores <- length(splitter)

  }

  return(list("cores" = cores,
              "cluster_type" = cluster_type,
              "parts" = parts))
}

#### Handle Backend and Clusters ####
#' Register Clusters
#' Register Clusters necessary for foreach function. currently only doParallel
#'   adaptor is supported, other adaptors will be implemented in the future.
#' @param adaptor foreach adaptor. Currently, parapurr supports: \enumerate{
#' \item "doMPI" \item "doParallel" \item "doSNOW" \item "doFuture" \item "doMC"}
#' @param cores number of cores (i.e. workers)
#' @param cluster_type "PSOCK", "FORK", "SOCK", "MPI", "multisession",
#'   "multicore", "cluster_FORK", or "cluster_PSOCK"
#'
#' @return A list containing cluster object (to be used later for terminating
#'   the cluster) and the adaptor name.
#' @noRd
.pa_reg_clusters <- function(adaptor, cores, cluster_type) {
  switch(adaptor,
         "doMPI" = {
           if (!requireNamespace("doMPI", quietly = TRUE)) {
             stop("Package 'doMPI' is required to be installed.",
                  call. = FALSE)
           }
           cl <- doMPI::startMPIcluster(count = cores,
                                        verbose = FALSE)
           doMPI::registerDoMPI(cl)
         },
         "doParallel" = {
           if (!requireNamespace("doParallel", quietly = TRUE)) {
             stop("Package 'doParallel' is required to be installed.",
                  call. = FALSE)
           }
           cl <- parallel::makeCluster(spec = cores,
                                       type = cluster_type)
           doParallel::registerDoParallel(cl)
         },
         "doSNOW" = {
           if (!requireNamespace("doSNOW", quietly = TRUE)) {
             stop("Package 'doSNOW' is required to be installed. Also: \n",
                  "If you intend to use MPI clusters: package \"Rmpi\" should be installed.",
                  call. = FALSE)
           }
           cl <- snow::makeCluster(spec = cores,
                                   type = cluster_type)
           doSNOW::registerDoSNOW(cl)
         },
         "doFuture" = {
           if (!requireNamespace("doFuture", quietly = TRUE)) {
             stop("Package 'doFuture' is required to be installed.",
                  call. = FALSE)
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
         "doMC" = {
           if (!requireNamespace("doMC", quietly = TRUE)) {
             stop("Package 'doMC' is required to be installed.",
                  call. = FALSE)
           }
           cl <- NA
           doMC::registerDoMC(cores = cores)
         },
         stop("Adaptor should be 'doFuture', 'doMC', doMPI', 'doParallel' or 'doSNOW'.",
              call. = FALSE)
  )
  return(list("cluster" = cl,
              "adaptor" = adaptor,
              "cluster_type" = cluster_type))
}


#' Terminate active cluster and revert registered adaptor
#'
#' @param active_cl cluster object returned by .pa_reg_clusters function
#' @param last_adaptor foreach internal environment before registering adaptor
#'   by parapurrr's .pa_reg_clusters
#'
#' @return NULL, stops clusters as a side-effect.
#' @noRd
.pa_stop_clusters <- function(active_cl, last_adaptor) {
  switch(active_cl$adaptor,
         "doMPI" = {
           doMPI::closeCluster(active_cl$cluster)
           message("Please run `Rmpi::mpi.finalize() at the end of your script to properly terminate MPI execution enviroment.")
         },
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
         },
         "doMC" = {
           invisible()
         }
  )
  # Revert doPar registration to the status before running parapurrr function
  foreach_env <- foreach:::.foreachGlobals
  if (length(last_adaptor) == 0) {
    # based on: https://stackoverflow.com/a/25110203/11470581
    rm(list = ls(name = foreach_env),
       pos = foreach_env)
  } else {
    purrr::iwalk(.x = last_adaptor,
                 .f = ~assign(x = .y,
                              value = .x,
                              pos = foreach_env))
  }

  invisible()
}

#### Main Wrapper ####

#' Internal backbone of parapurrr functions
#' This function is the only function that will be called by map, map2, and
#'   imap function families. every necessary steps from handling user input,
#'   registering and subsequently terminating the parallel cluster and up to
#'   handling the results to the user will be performed by .pa_internal.
#'   except int_fun, other arguments will be handled to either purrr's mapping
#'   function, parallel cluster registering function or foreach function.
#' @param .x refer to \code{\link[purrr]{map}} and \code{\link[purrr]{imap}}
#' @param .y refer to \code{\link[purrr]{map2}}
#' @param .l refer to \code{\link[purrr]{map2}}
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
                         .l,
                         .f,
                         ...,
                         int_fun,
                         adaptor,
                         cores,
                         cluster_type,
                         auto_export,
                         splitter,
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
  .pa_arg_check(.errorhandling = .errorhandling,
                .export = .export,
                .inorder = .inorder,
                .l = .l,
                .noexport = .noexport,
                .packages = .packages,
                .verbose = .verbose,
                .x = .x,
                .y = .y,
                adaptor = adaptor,
                auto_export = auto_export,
                cluster_type = cluster_type,
                cores = cores)

  # Handle manual backend registering
  manual_backend <- getOption("pa_manual_backend") || is.null(adaptor)

  if (manual_backend) {
    int_args <- .pa_args_manual(x_length = ifelse(test = is.null(.l),
                                                  yes = length(.x),
                                                  no = length(.l[[1]])),
                                user_cores = cores,
                                adaptor = adaptor,
                                splitter = splitter)
  } else {
    int_args <- .pa_args(x_length = ifelse(test = is.null(.l),
                                           yes = length(.x),
                                           no = length(.l[[1]])),
                         cores = cores,
                         adaptor = adaptor,
                         cluster_type = cluster_type,
                         splitter = splitter)
    # register cluster
    last_adaptor <- as.list(foreach:::.foreachGlobals)
    cl <- .pa_reg_clusters(adaptor = adaptor,
                           cores = int_args$cores,
                           cluster_type = int_args$cluster_type)
    on.exit(.pa_stop_clusters(active_cl = cl,
                              last_adaptor = last_adaptor))
  }

  # split the input
  foreach_input <- lapply(X = int_args$parts,
                          FUN = function(part_index) {
                            if (!is.null(.l)) {
                              return(lapply(X = .l,
                                            FUN = function(.l_element) {
                                              return(.l_element[part_index])
                                            }))
                            } else if (!is.null(.y)) {
                              return(list(.x = .x[part_index],
                                          .y = .y[part_index]))
                            } else {
                              return(.x[part_index])
                            }
                          })

  # Prepare the environment
  .f <- purrr::as_mapper(.f, ...)

  if (auto_export == "all") {
    eval_env <- rlang::env_clone(env = parent.frame(2))
    .export <- unique(c(.export, ls(envir = eval_env)))
  } else {
    eval_env <- new.env(parent = parent.frame(2))
    if (auto_export) {
      .export <- .pa_export(env = parent.frame(2),
                            .f = .f,
                            .export = .export,
                            ...)
    }
  }

  # build foreach object
  foreach_obj <- foreach::foreach(x_split = foreach_input,
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
                                  .export = c(.export, ".f", "int_fun"),
                                  .noexport = c("%performer%",
                                                ".f",
                                                "foreach_obj",
                                                "foreach_input",
                                                "int_fun"),
                                  .verbose = .verbose)

  # Update evaluation environment
  rlang::env_bind(.env = eval_env,
                  `%performer%` = ifelse(int_args$cores > 1,
                                         yes = ifelse(getOption("pa_dorng"),
                                                      yes = doRNG::`%dorng%`,
                                                      no = foreach::`%dopar%`),
                                         no = foreach::`%do%`),
                  .f = .f,
                  foreach_obj = foreach_obj,
                  int_fun = int_fun
  )

  # perform!
  output <- rlang::eval_bare(env = eval_env,
                             expr = quote(
                               foreach_obj %performer% {
                                 output <- eval(int_fun)
                                 return(output)})
  )
  return(output)
}
