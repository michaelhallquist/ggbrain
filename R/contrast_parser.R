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
#'  data <- matrix(rnorm(1000), ncol=5) %>% data.frame() %>%
#'    setNames(c("a", "f", "dsf", "abc", "b"))
#'  data$bin1 <- rbinom(200, 1, .5)
#'  data$bin2 <- rbinom(200, 1, .5)
#'  data$dim1 <- rep(1:20, each=10)
#'  data$dim2 <- rep(1:10, 20)
#'
#'  res <- contrast_parser("a + f + dsf [ bin1 == 1 ] * abc[ abc > 1 ] + b", data)
#'  res <- contrast_parser("dsf [ bin1 == 1 ] + b", data)
#'  res <- contrast_parser("dsf * abc", data)
#'  res <- contrast_parser("1 = a > 3; 2 = b > 3; 3 = a > 3 & b > 3")
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

  # check for compound contrast, use recursive evaluation
  if (grepl(";", expr, fixed=TRUE)) {
    expr_list <- strsplit(expr, "\\s*;\\s*", perl = TRUE)[[1]]

    c_list <- lapply(expr_list, function(e) {
      if (!grepl("^\\s*[\\w+_\\.]+\\s*=\\s*[^=]+", e, perl = TRUE)) {
        stop(glue::glue("Compound expression {e} does not follow the <value> = <expr> syntax"))
      }

      eq_pos <- regexpr("=", e, fixed = TRUE)
      val <- trimws(substr(e, 1, eq_pos - 1)) # just value to be used (left-hand side)
      e <- trimws(substr(e, eq_pos + 1, nchar(e))) # just expression after equals

      df <- contrast_parser(e, data = data, default_val = default_val)
      #   dplyr::rename(!!val := value)

      ret <- list(val = val, df = df)

      return(ret)
    })

    # iteratively set elements of the resulting value vector so that the later
    # parts of the compound expression, if TRUE, overwrite earlier parts
    comb_df <- data.frame(data[, c("dim1", "dim2")], value = NA_character_)
    for (cc in c_list) {
      comb_df$value[cc$df$value == TRUE] <- as.character(cc$val)
    }

    return(comb_df)

    # iteratively join these data.frames together (clunkier)
    #c_df <- Reduce(function(x, y) inner_join(x, y, by = c("dim1", "dim2")), c_list)
  }

  open_brack <- gregexpr("[", expr, fixed=TRUE)[[1]]
  close_brack <- gregexpr("]", expr, fixed=TRUE)[[1]]
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
    simple_subset <- FALSE
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

    # if there is just one image and one subset, we have a simple subset and should merge result back against labeled data
    simple_subset <- length(img_vars) == 1L && nrow(brack_df) == 1L

    # the last element of each split must be the variable that is subset based
    # on the use of brack_df to find the text preceding each bracket set
    # thus, use the length of each element of the list to identify variable positions
    # that need to be subset in calculations
    ops_split <- strsplit(trimws(img_vars), "\\s*[\\*/\\-\\+]\\s*", perl=TRUE)

    # drop empty splits (empty space or 0 chars around operator) since that will throw off numbering
    ops_split <- sapply(ops_split, function(vars) vars[vars != ""])

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
  out_data <- data[, union(img_vars, brack_vars), drop=FALSE]

  # will skip if no subset
  for (vi in seq_along(subset_vars)) {
    # logical test of which elements match -- invert this to figure out what to zero
    l_test <- !with(out_data, eval(parse(text=brack_expr[vi])))

    # zero the elements that were not in the match so that they do not pass in the arithmetic
    out_data[[img_vars[subset_vars][vi]]][l_test] <- 0
  }

  # now that subsetting is complete, perform the requested arithmetic
  out_var <- with(out_data, eval(parse(text = nobrack_expr)))

  if (!is.logical(out_var)) {
    # if any calculated value is precisely 0, then assume that no voxels/values
    # passed through the arithmetic and apply the default value
    out_var[abs(out_var) < 1e-6] <- default_val
  }

  # at present, always bind dim1 and dim2 back together with contrast values and
  # return appropriately labeled data.frame that mirrors the $slice_data structure
  checkmate::assert_subset(c("dim1", "dim2"), names(data))

  out_df <- data.frame(data[, c("dim1", "dim2")], value = out_var)
  if (isTRUE(simple_subset)) {
    attr(out_df, "img_source") <- img_vars
  }
  
  return(out_df)
}

# for testing
# data <- matrix(rnorm(1000), ncol=5) %>% data.frame() %>%
#   setNames(c("a", "f", "dsf", "abc", "b"))
# data$bin1 <- rbinom(200, 1, .5)
# data$bin2 <- rbinom(200, 1, .5)
# data$dim1 <- rep(1:20, each=10)
# data$dim2 <- rep(1:10, 20)

# res <- contrast_parser("a + f + dsf [ bin1 == 1 ]  * abc[ abc > 1 ] + b", data)
# res <- contrast_parser("dsf [ bin1 == 1 ] + b", data)
# res <- contrast_parser("dsf * abc", data)

# compound logical expressions (conjunction)
# res <- contrast_parser("1 = bin1 == 1L; 2 = bin2 == 1L; 3 = bin1 == 1L & bin2 == 1L", data)

# simple logical expression
#res <- contrast_parser("bin1 == 1L", data)
