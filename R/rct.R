
#' rct function
#'
#' @param cases single number or vector giving the number of seats to shade. If a vector is supplied, the values indicate how many seats of each colour to shade. The sum of this vector gives the total number of seats shaded
#' @param type the floor plan to be used. Current options are "square", "theatre" (the original Rifkin and Bouwer floor plan), "stadium" and "bigsquare"
#' @param border the color for the outlines of the floor plan
#' @param xlim range of x axis. Note that the theate sits in the unit square with bottom corner (0, 0) and top corner (1, 1)
#' @param ylim range of y axis
#' @param fill vector of colours for shading seats, defaults to grey
#' @param xlab text label to appear below floor plan. Defaults to "x cases in n"
#' @param ylab
#' @param lab.cex character expansion factor (see 'par') to specify size of text labels (if any) on the floor plan
#' @param seed specify the starting seed value for the random number generator. Setting this makes it possible to reproduce exactly the same shaded seats on successive calls of rct
#' @param plot.new if FALSE, the theatre is drawn over the top of an existing plot
#' @param label if TRUE, any text labels for the specified floor plan will be displayed
#' @param lab.col colour used for any text labels
#' @param draw.plot if this is FALSE, the RCT is not drawn and instead a data frame is returned showing the seats that would have been shaded and the colours that would have been used
#' @param ... any additional parameters are passed through to the plot call that sets up the chart
#'
#' @return returns a risk characterization theatre plot
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


