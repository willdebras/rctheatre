
#' rct function
#'
#' @param cases
#' @param type
#' @param border
#' @param xlim
#' @param ylim
#' @param fill
#' @param xlab
#' @param ylab
#' @param lab.cex
#' @param seed
#' @param plot.new
#' @param label
#' @param lab.col
#' @param draw.plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rct <- function(cases, type="square", border="#1239DC", xlim=c(0, 1), ylim=c(0, 1),
                fill=NULL, xlab=NULL, ylab="", lab.cex=1, seed=NULL, plot.new=TRUE,
                label=FALSE, lab.col="#1239DC", draw.plot=TRUE, ...) {

  # Check the specified "floor plan" exists, otherwise use default
  if (!(type %in% names(rct.plans))) type <- "square"
  rct <- rct.plans[[type]]

  # Get floor plan
  plan <- rct$plan
  n <- dim(plan)[1]
  m <- length(cases)

  # Recycle fill vector to have as many colours as cases
  if (length(fill) < m) fill <- rep(fill, ceiling(m/length(fill)))[1:m]

  # Assign seats
  plan$taken <- FALSE
  # Set random seed
  if (!is.null(seed)) set.seed(seed)
  seats <- sample(1:n, sum(cases))
  plan$taken[seats] <- TRUE

  # Shaded to "pac man green"
  if (is.null(fill)) fill <- "#A8FA04"

  # Colour each case type differently
  plan$col <- NA
  plan$col[seats] <- unlist(sapply(1:m, function(i) rep(fill[i], cases[i])))

  # Get text labels (if they exist)
  l <- rct$labels
  if (is.null(l)) label <- FALSE

  # Format label: "x cases in n"
  if (is.null(xlab)) xlab <- paste(prettyNum(sum(cases), big.mark=","),
                                   "cases in", prettyNum(n, big.mark=","))

  # Plot RCT
  if (draw.plot) {
    # If plot device doesn't exist or if plot.new=TRUE, create new plot
    if ((dev.cur() == 1) | plot.new) plot(xlim, ylim, type="n",
                                          axes=FALSE, xlab=xlab, ylab=ylab, ...)
    # Draw plan and fill seats
    rect(plan$xleft, plan$ybottom, plan$xright, plan$ytop,
         border=border, col=plan$col)
    # Add text labels (if required)
    if (label) {
      if (is.na(l$pos[1])) l$pos <- NULL
      text(l$x, l$y, l$text, pos=l$pos, col=lab.col, xpd=NA, cex=lab.cex)
    }
    # Return the plan data (including taken seats and colours)
    # if the chart is not plotted
  } else return(plan)
}


