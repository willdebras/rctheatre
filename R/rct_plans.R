
#' load_plans
#'
#' @return returns a data object with the plans references in the rct function
#' @export
#'
#' @examples load_plans()
load_plans <- function() {

  if (!exists("rct.plans")) load(file = url("http://www.umich.edu/~bwest/plans.Rdata"), envir = globalenv())

}
