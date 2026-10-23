#' A function to evaluate informative order and range constraints over different estimands (e.g. Average and Conditional Effects) from the EffectLiteR framework
#'
#' @param elr.object An object of class 'effectlite' [package "EffectLiteR"]
#' @param estimand A vector of strings, containing valid estimands from the EffectLiteR Approach
#' @param hypothesis.set A vector of strings, containing valid predefined hypotheses. Can be left empty for defaults
#' @param hypotheses.custom A vector of strings containing valid hypotheses
#'
#' @returns Some gorica values
#' @export
constrained.inference <- function(
    elr.object,
    estimand = c("adjmeans", "ATE", "ATT", "ATC"),
    hypothesis.set = NULL, #c("ordering.gt", "ATT>ATC"),
    hypotheses.custom=list()){                                                  # add custom hypotheses here

  ## Grab Input
  estimand_choices <- c("adjmeans", "ATE", "ATT", "ATC")                        # allowed choices for estimands

  if (any(!estimand %in% estimand_choices)) {
    stop("Invalid estimand specified.")
  }                                                                             # validate estimand inputs
  hypothesis.set <- match.arg(hypothesis.set)



  ## Get the specified parameters
  x_levels <- elr.object@input@vlevels$x                                        # get levels of treatment var


  parnames <- unlist(estimand.to.parnames(estimand, x_levels))


  ## Build predefined hypotheses and COMBINE with custom ones
    preset.hypotheses <- build_hypotheses(hypothesis.set, parnames)             # Call Hypothesis Function for default hypotheses


    hypotheses <- c(preset.hypotheses, hypotheses.custom)                       # merge default with custom hypotheses


  if (length(hypotheses) == 0) stop("No hypotheses specified.")                 # Error if there are no hypotheses, however this could happen, not sure, better be safe I guess?!

  ate_names <- paste0("Eg", seq_len(length(x_levels) - 1))
  parnames.dropped <- setdiff(parnames, ate_names)                              # Since Eg1 = adjmean1-adjmean0 it needs to be dropped, else the vcov is not invertible

  estimates.elr <- elr.object@results@est[parnames.dropped]
  VCOV.estimates <- elr.object@results@vcov.def[parnames.dropped,parnames.dropped]

  ## Run gorica
  gorica.obj <- restriktor::goric(estimates.elr,
                      VCOV = VCOV.estimates,
                      hypotheses=(hypotheses)
                      )

  # then give hypothesis, loglik weights, goric weight

  # hypotheses.test <- list("adjmean0 > adjmean1", "adjmean0 < adjmean1")  <- works
  # hypotheses.test2 <- list("adjmean0 > adjmean1; adjmean0 < adjmean1")  <- works not as ";" is read as "AND" (bain reads it a OR)
  return(gorica.obj)
}
