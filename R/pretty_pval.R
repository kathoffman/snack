#' Format p-values for a report
#'
#' @param pval a numeric vector of p-values
#'
#' @return a character string of formatted p-values
#' @export
#'
fmt_pval <- function(pval){
  pval <- ifelse(pval < .001, "<.001",
                 ifelse(pval < .01, round(pval, 3),
                        ifelse(round(pval, 2) == .05, round(pval, 3),
                               round(pval, 2))))
  return(pval)
}
