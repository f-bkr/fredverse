#' Generate a Table according to Steyer's Theory of Causal Effects
#'
#' @param table A dataframe that represents the Table from which data shall be generated
#' @param N The sample size of the generated data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' my.table <- data.frame(
#'               U = 1:6,
#'               Z_1 = c(0,0,0,1,1,1),
#'               pU = rep(1/6, 6),
#'               pX1 = c(.5,.5,.5,.5,.5,.5),
#'               tau0 = c(70,70,70,20,10,30),
#'               tau1 = c(80,80,80,40,20,60)
#' )
#'
#' data <- data.from.causality.table(table=ma.table, N=100)
data.from.causality.table <- function(table=NULL, N=1000, sigma=5){
  ## Just generate some Table I made
  if (is.null(table)) {
    table <- data.frame(
      U = 1:6,
      Z_1 = c(0,0,0,1,1,1),
      pU = rep(1/6, 6),
      pX1 = c(.3,.5,.4,.8333,.8333,.8333),
      tau0 = c(70,70,70,20,10,30),
      tau1 = c(80,80,80,40,20,60)
    )
  }

  ## Draw a sample of persons/units from the U given in the table
  U <- sample(
    table$U,
    size = N,
    replace = TRUE,
    prob = table$pU  # This should be constant in most cases we look at
  )
  ## Give the sampled units their true values and treatment probability
  data <- merge(
    data.frame(id = 1:N, U = U),
    table,
    by = "U"
  )
  ## use the treatment probability of a unit to assign treatment
  data$X <- rbinom(
    N,
    size = 1,
    prob = data$pX1
  )
  ## generate Outcome based on tau
  data$Y <- ifelse(data$X==0, data$tau0+sigma, data$tau1+sigma)

  return(data)
}
