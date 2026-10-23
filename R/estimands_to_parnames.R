#' Function to convert the estimands that the GORIC wrapper evaluates hypotheses over to parameter names of the EffectLiteR Approach
#'
#' @param estimand Argument of the GORIC wrapper function. A list of strings containing valid estimands of the EffectLiteR Approach
#' @param x_levels Chr / text vector. Levels of the treatment variable
#'
#' @returns Chr of parameter names that are valid within the EffectLiteR Approach. Like: chr [1:2] "adjmean0" "adjmean1"
#' @export
#'
#' @examples
#' estimand.to.parnames(estimand="adjmeans", x_levels=c("0","1"))
estimand.to.parnames <- function(estimand, x_levels) {
  switch(estimand,
  # switch takes a string e and matches it against the branches within
  # so if e is "adjmean" it executes the code behin the = sign; if NO MATCh it returns null
         adjmeans = paste0("adjmean", seq_along(x_levels) - 1),
         ATE      = paste0("Eg", seq_len(length(x_levels)-1)),                  # Gives "Eg1" if 2 levels; "Eg1" "Eg2" if three levels and so on...
         ATT      = paste0("ATT_", seq_along(x_levels) - 1),
         ATC      = paste0("ATC_", seq_along(x_levels) - 1)
  )
}

# TODO: allow multiple estimands like: estimand.to.parnames(estimand=c("adjmeans", "ATE"), x_levels=c("0","1"))
