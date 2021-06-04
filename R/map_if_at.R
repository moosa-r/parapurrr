#' Detect incorrect splitter in map_if and map_at
#'
#' Check if the custom splitter provided by the user is valid for pa_map_if
#'   and pa_map_at. one source of confusion would be the case that the user
#'   provides the splitter for .x not the selected elements of .x (by .p and
#'   .at). This function will detects this issue and warns the user
#'
#' @param x_length Length of .x argument
#' @param x_length_sel Length of selected elements of .x
#' @param splitter User-provided splitter. A list with numeric vectors.
#'
#' @return Nothing! If the splitter is not valid, code execution will be
#'   stopped.
#' @noRd
.splitter_check_at_if <- function(x_length, x_length_sel, splitter) {

  splt_x <- try(.splitter_check(x_length = x_length,
                               splitter = splitter),
               silent = TRUE)

  splt_sel <- try(.splitter_check(x_length = x_length_sel,
                                 splitter = splitter),
                 silent = TRUE)

  if (!inherits(splt_x, "try-error") && inherits(splt_sel, "try-error")) {
    stop("In pa_map_if() and pa_map_at(), splitter should correspond to the results of .at or .p respecfully.\n",
         "Your provided splitter seems to be valid for .x not its selected elements:\n",
         sub("^Error : ", "- ", as.character(splt_sel)),
         call. = FALSE)
  }
}

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
pa_map_if <- function(.x, .p, ..., .f, .else = NULL,
                      cores = NULL,
                      adaptor = "doParallel", cluster_type = NULL,
                      splitter = NULL, auto_export = TRUE, .export = NULL,
                      .packages = NULL, .noexport = NULL,
                      .errorhandling = "stop",
                      .inorder = TRUE,
                      .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  sel <- probe(.x, .p)
  output <- purrr::list_along(.x)

  .splitter_check_at_if(x_length = length(.x),
                        x_length_sel = sum(sel),
                        splitter = splitter)

  output[sel] <- .pa_internal(.x = .x[sel],
                              .y = NULL,
                              .l = NULL,
                              .f = .f,
                              ... = ...,
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
    # user provided splitter will be ignored here. (splitter = NULL) another
    # option would be to add an argument named: splitter_else
    output[!sel] <- .pa_internal(.x = .x[!sel],
                                 .y = NULL,
                                 .l = NULL,
                                 .f = .else,
                                 int_fun = int_fun,
                                 adaptor = adaptor,
                                 cores = cores,
                                 cluster_type = cluster_type,
                                 auto_export = auto_export,
                                 splitter = NULL,
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
pa_map_at <- function(.x, .at, .f, ...,
                      cores = NULL,
                      adaptor = "doParallel", cluster_type = NULL,
                      splitter = NULL, auto_export = TRUE, .export = NULL,
                      .packages = NULL, .noexport = NULL,
                      .errorhandling = "stop",
                      .inorder = TRUE,
                      .verbose = FALSE) {

  int_fun <- .pa_call(purrr::map, x_split, .f, ...)

  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  output <- purrr::list_along(.x)

  .splitter_check_at_if(x_length = length(.x),
                        x_length_sel = sum(sel),
                        splitter = splitter)

  output[sel] <- .pa_internal(.x = .x[sel],
                              .y = NULL,
                              .l = NULL,
                              .f = .f,
                              ... = ...,
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
