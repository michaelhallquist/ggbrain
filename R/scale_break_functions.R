#' breaks function to encourage integer-valued breaks, based on input from pretty
#' @param n number of breaks (default = 5)
#' @param ... Additional arguments passed to the pretty() function
#' @details Code from here: https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
#' @keywords internal
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#' breaks function for including min + max with labels, and a few unlabeled ticks in between
#' @param n number of breaks added within the min-max range
#' @param ... 
range_breaks <- function(n=3, ...) {
  fxn <- function(x) {
    breaks <- round(seq(from = min(x, na.rm = T), to = max(x, na.rm = T), length.out = n + 2), ...)
    # breaks <- signif(c(min(x, na.rm = T), max(x, na.rm = T)), 2)
    
    bnames <- as.character(breaks)
    bnames[2:(length(bnames) - 1)] <- "" # don't label interior breaks
    names(breaks) <- bnames
    breaks
    #print(breaks)
  }
  return(fxn)
}
