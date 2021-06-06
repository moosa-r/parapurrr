##############################################################################
#### The following block of codes are internal function in purrr which were ##
#### not being exported. In order to implement the parallel version of some ##
#### of the purrr's function, I had to call these functions. Here I had     ##
#### 3 options:                                                             ##
#### 1- Using ::: to call functions which were not being exported in        ##
####   purrr's namespace                                                    ##
#### 2- Use x = utils::getFromNamespace("y", "purrr")                       ##
#### 3- Directly copying the function from the source if the licence of     ##
####    purrr allows that                                                   ##
##############################################################################
####  Copyright owners are purrr authors:                                   ##
####  Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>  ##
##############################################################################

#' @noRd
probe <- function(.x, .p, ...) {
  ## Directly Copied from purrr's source and redistributed under GPL-3 licence. ##
  ## Copyright owners are purrr authors:   ##
  ## Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/> ##
  ## https://github.com/tidyverse/purrr/blob/761a2224c437067c5d07beeeba06cde65a3e08a6/R/modify.R ##
  if (purrr::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    purrr::map_lgl(.x, .p, ...)
  }
}

#' @noRd
check_tidyselect <- function(){
  ## Directly Copied from purrr's source and redistributed under GPL-3 licence. ##
  ## Copyright owners are purrr authors:   ##
  ## Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/> ##
  ## https://github.com/tidyverse/purrr/blob/761a2224c437067c5d07beeeba06cde65a3e08a6/R/utils.R ##
  if (!rlang::is_installed("tidyselect")) {
    rlang::abort("Using tidyselect in `map_at()` requires tidyselect")
  }
}

#' @noRd
at_selection <- function(nm, .at){
  ## Directly Copied from purrr's source and redistributed under GPL-3 licence. ##
  ## Copyright owners are purrr authors:   ##
  ## Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/> ##
  ## https://github.com/tidyverse/purrr/blob/761a2224c437067c5d07beeeba06cde65a3e08a6/R/utils.R ##
  if (rlang::is_quosures(.at)) {
    check_tidyselect()
    .at <- tidyselect::vars_select(.vars = nm, !!!.at)
  }
  .at
}

#' @noRd
inv_which <- function(x, sel) {
  ## Directly Copied from purrr's source and redistributed under GPL-3 licence. ##
  ## Copyright owners are purrr authors:   ##
  ## Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/> ##
  ## https://github.com/tidyverse/purrr/blob/761a2224c437067c5d07beeeba06cde65a3e08a6/R/modify.R ##
  if (is.character(sel)) {
    names <- names(x)
    if (is.null(names)) {
      stop("character indexing requires a named object", call. = FALSE)
    }
    names %in% sel
  } else if (is.numeric(sel)) {
    if (any(sel < 0)) {
      !seq_along(x) %in% abs(sel)
    } else {
      seq_along(x) %in% sel
    }

  } else {
    stop("unrecognised index type", call. = FALSE)
  }
}
