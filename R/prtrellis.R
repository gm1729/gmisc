#' Display Many Lattice Plots On One Page
#'
#' @param x A list of trellis plots.
#' @param layout The number of columns and rows. By default a semi-intelligent guess is made based on the number of plots. FEATURE_TO_ADD: Should take the size of the plotting window into account. Also should use a similar approach for xqplot.
#' @return NULL. The function is used to create a plot.
#' @examples
#' library(lattice)
#' prtrellis( list(
#' lattice::barchart(HairEyeColor,auto.key=T),
#' lattice::xyplot( height ~ age, Loblolly, groups = Seed, auto.key =TRUE)))
prtrellis <- function(x, layout = guess(x)) {
  guess <- function(x) {
    n <- length(x)
    try1 <- ceiling(sqrt(n))
    try2 <- ceiling((sqrt(1+4*n)-1)/2)
    n1 <- try1^2
    n2 <- try2*(try2 + 1)
    if(n1 < n2) c(try1,try1)
    else c(try2+1, try2)
  }
  spl <- function(i,layout) {
    c(1+(i-1)%%layout[1],1+(i-1)%/%layout[1],layout)
  }
  n <- length(x)
  lapply(1:n,function(i) {
    print(x[[i]], split=spl(i,layout=layout),
          more=i<n,layout =layout)
  })
  invisible(x)
}
