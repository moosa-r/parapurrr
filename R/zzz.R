#' @section Supported purrr functions:
#' \enumerate{
#' \item \strong{map family}: map, map_chr, map_dbl, map_df, map_dfc, map_dfr, map_int, map_lgl
#' \item \strong{map2 family}: map2, map2_chr, map2_dbl, map2_df, map2_dfc, map2_dfr, map2_int, map2_lgl
#' \item \strong{conditional map family}: map_at, map_if
#' \item \strong{pmap family}: pmap, pmap_chr, pmap_dbl, pmap_df, pmap_dfc, pmap_dfr, pmap_int, pmap_lgl
#' \item \strong{imap family}: imap, imap_chr, imap_dbl, imap_df, imap_dfc, imap_dfr, imap_int, imap_lgl
#' \item \strong{walk family}: walk, walk2,iwalk, pwalk
#' }
#'
#' @section Supported foreach adaptors:
#' \enumerate{
#' \item doFuture
#' \item doMC
#' \item doMPI
#' \item doParallel (default adaptor)
#' \item doRNG
#' \item doSNOW
#' }
#'
#' @docType package
#' @name parapurrr
#' @keywords internal
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  options(pa_cores = parallel::detectCores() - 1,
          pa_manual_backend = FALSE,
          pa_dorng = FALSE)
}
