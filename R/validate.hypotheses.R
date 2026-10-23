#' A function used to validate the input of the GORIC wrapper function
#'
#' @param hypothesis.set A list of strings that will get tested: Does it contain a valid input for the GORIC wrapper?
#'
#' @returns An error, but ideally nothing
#' @export
validate.hypotheses <- function(hypothesis.set){

  known <- c("ordering.gt", "ATT_gt_ATC")                                       # These are the default hypotheses names
  unknown <- setdiff(hypothesis.set, known)                                     # Catch all names for custom hypotheses

  if (length(unknown) > 0) stop("Unknown hypothesis set(s): ", paste(unknown, collapse = ", "))
}

# TODO: give back list of possible estimands
