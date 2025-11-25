# internal, single-level list flattener to replace rlang::flatten
# drops NULLs, preserves order, and wraps atomic elements in a list
flatten_list_once <- function(x) {
  stopifnot(is.list(x))

  out <- list()
  for (i in seq_along(x)) {
    elt <- x[[i]]
    if (is.null(elt)) {
      next
    } else if (is.list(elt)) {
      out <- c(out, elt)
    } else {
      out <- c(out, list(elt))
    }
  }

  out
}
