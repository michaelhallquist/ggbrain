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
#' @param digits number of decimal places to display 
range_breaks <- function(n=3, digits=2) {
  fxn <- function(x) {
    if (is.null(x) || all(is.na(x) || all(is.infinite(x)))) {
      breaks <- logical(0) # no breaks
    } else {
      breaks <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), length.out = n + 2)

      bnames <- as.character(round(breaks, digits)) # for formatting, don't display so many digits
      bnames[2:(length(bnames) - 1)] <- "" # don't label interior breaks
      names(breaks) <- bnames
    }
    return(breaks)
  }
  return(fxn)
}
