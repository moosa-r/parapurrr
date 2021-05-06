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
