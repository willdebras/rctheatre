#RCT function

load_plans <- function() {

  if (!exists("rct.plans")) load(file = url("http://www.umich.edu/~bwest/plans.Rdata"), envir = globalenv())

}
