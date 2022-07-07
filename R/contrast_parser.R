#' helper function to calculate contrasts of one or more images using a combination of
#'   image arithmetic and logical subsetting
#' @param expr a string or expression containing the image calculation to be performed
#' @param data a data.frame containing all variables used in \code{expr}. This will be used
#'   to perform contrast calculations
#' @param default_val the value to be returned for any element of the contrast calculation that
#'   does not pass through the arithmetic, either because of logical subsetting or because it
#'   is exactly zero. In general, leave this as \code{NA_real_} unless you know what you're doing.
#' @examples 
#' \dontrun{
#'   data <- matrix(rnorm(1000), ncol=5) %>% data.frame() %>%
#'     setNames(c("a", "f", "dsf", "abc", "b"))
#'   data$sdf <- rbinom(200, 1, .5)
#'  res <- contrast_parser("a + f + dsf [ sdf == 1 ]  * abc[ abc > 1 ] + b",data)
#'  res <- contrast_parser("dsf [ sdf == 1 ] + b", data)
#'  res <- contrast_parser("dsf * abc", data)
#' }
#' @author Michael Hallquist
#' @importFrom checkmate assert_data_frame assert_subset
#' @keywords internal
contrast_parser <- function(expr, data = NULL, default_val=NA_real_) {
  
  if (is.expression(expr)) { # expand expression as character vector
    expr <- capture.output(cat(as.character(expr), sep = "\n"))
  } else if (!checkmate::test_string(expr)) {
    stop("expr must be an expression or a character string")
  }
  
  checkmate::assert_data_frame(data)
  
  open_brack <- gregexpr("[", expr, fixed=T)[[1]]
  close_brack <- gregexpr("]", expr, fixed=T)[[1]]
  if (open_brack[1] == -1L) open_brack <- numeric(0) # necessary since no match returns -1
  if (close_brack[1] == -1L) close_brack <- numeric(0)
  
  if (length(open_brack) > length(close_brack)) {
    stop("At least one opening bracket does not have a closing bracket")
  } else if (length(open_brack) < length(close_brack)) {
    stop("At least one closing bracket does not have an opening bracket")
  } else {
    brack_df <- data.frame(open=open_brack, close=close_brack)
  }
  
  if (nrow(brack_df) == 0L) {
    # no usage of brackets -- life is easy
    nobrack_expr <- expr
    img_vars <- all.vars(as.formula(paste("~", expr)))
    brack_vars <- NULL
    subset_vars <- NULL
  } else {
    brack_expr <- apply(brack_df, 1, function(x) { 
      trimws(substr(expr, x["open"]+1, x["close"]-1))
    })
    
    # what vars are used in the brackets? need to preserve them in the lookup process
    brack_vars <- sapply(brack_expr, function(e) {
      all.vars(as.formula(paste("~", e)))
    }, USE.NAMES = FALSE)
    
    # remove bracket expressions so that this can be used later in the requested calculation
    nobrack_expr <- gsub("\\[[^\\]]+\\]", "", expr, perl=TRUE)
    
    img_vars <- sapply(seq_len(nrow(brack_df)), function(i) {
      start_pos <- ifelse(i > 1, brack_df$close[i-1] + 1, 1)
      #end_pos <- ifelse(i < nrow(brack_df), brack_df$open[i] - 1, nchar(expr))
      end_pos <- brack_df$open[i]
      test <- substr(expr, start_pos, end_pos)
      sub("(\\w+)\\s*\\[$", "\\1", test, perl=TRUE)
    })
    
    # retain any characters after the last bracket (e.g., additional image variables)
    if (nchar(expr) > max(brack_df$close)) {
      img_vars <- c(img_vars, substr(expr, max(brack_df$close) + 1, nchar(expr)))
      addl_chars <- TRUE
    } else {
      addl_chars <- FALSE
    }
    
    # the last element of each split must be the variable that is subset based
    # on the use of brack_df to find the text preceding each bracket set
    # thus, use the length of each element of the list to identify variable positions
    # that need to be subset in calculations
    ops_split <- strsplit(trimws(img_vars), "\\s*[\\*/\\-\\+]\\s*", perl=TRUE)
    
    # drop empty splits (empty space or 0 chars around operator) since that will throw off numbering
    ops_split <- sapply(ops_split, function(vars) { vars[vars != ""] })
    
    # use cumulative sum to index subset variables across all
    subset_vars <- cumsum(sapply(ops_split, length))
    
    if (isTRUE(addl_chars)) {
      # if there are chars after the last bracket set, these go into the final element of the ops_split list
      # but there is, by definition, no subsetting there, so remove that from the subset_vars
      subset_vars <- subset_vars[-length(subset_vars)]
    }
    
    # now, retain just the variables of interest as img_vars (minus the operands)
    img_vars <- unlist(ops_split)
  }
  
  if (!checkmate::test_subset(img_vars, names(data))) {
    mismatch <- setdiff(img_vars, names(data))
    stop("The following columns in the contrast expression could not be found in the data: ", 
         paste(mismatch, collapse=", "))
  }
  
  # start with a copy of relevant variables
  out_data <- data[,union(img_vars, brack_vars)]
  
  # will skip if no subset
  for (vi in seq_along(subset_vars)) {
    # logical test of which elements match -- invert this to figure out what to zero
    l_test <- !with(out_data, eval(parse(text=brack_expr[vi])))
    
    # zero the elements that were not in the match so that they do not pass in the arithmetic
    out_data[[img_vars[subset_vars][vi]]][l_test] <- 0
  }
  
  # now that subsetting is complete, perform the requested arithmetic
  out_var <- with(out_data, eval(parse(text=nobrack_expr)))
  
  # if any calculated value is precisely 0, then assume that no voxels/values 
  # passed through the arithmetic and apply the default value
  out_var[abs(out_var) < 1e-6] <- default_val
  
  # at present, always bind dim1 and dim2 back together with contrast values and
  # return appropriately labeled data.frame that mirrors the $slice_data structure
  checkmate::assert_subset(c("dim1", "dim2"), names(data))
  
  out_df <- data.frame(data[,c("dim1", "dim2")], value=out_var)
  
  return(out_df)
}

# for testing
# data <- matrix(rnorm(1000), ncol=5) %>% data.frame() %>%
#   setNames(c("a", "f", "dsf", "abc", "b"))
# data$sdf <- rbinom(200, 1, .5)
# 
# res <- contrast_parser("a + f + dsf [ sdf == 1 ]  * abc[ abc > 1 ] + b",data)
# res <- contrast_parser("dsf [ sdf == 1 ] + b", data)
# res <- contrast_parser("dsf * abc", data)
