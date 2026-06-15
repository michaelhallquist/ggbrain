# Transform bracket subsetting into elementwise masking so array dimensions are
# retained rather than using base R's vector-subset semantics.
replace_array_subset_value <- function(expr, replacement) {
  if (is.symbol(expr) && identical(expr, as.name("value"))) {
    return(replacement)
  }
  if (!is.call(expr)) return(expr)
  as.call(lapply(as.list(expr), replace_array_subset_value, replacement = replacement))
}

transform_array_subset_calls <- function(expr) {
  if (!is.call(expr)) return(expr)

  if (identical(expr[[1L]], as.name("["))) {
    if (length(expr) != 3L) {
      stop("Cluster contrast subsetting must use exactly one condition inside brackets.")
    }
    value_expr <- transform_array_subset_calls(expr[[2L]])
    condition_expr <- replace_array_subset_value(expr[[3L]], expr[[2L]])
    return(as.call(list(
      as.name(".mask_array"),
      value_expr,
      transform_array_subset_calls(condition_expr)
    )))
  }

  as.call(lapply(as.list(expr), transform_array_subset_calls))
}

contains_unsupported_cluster_call <- function(expr) {
  if (!is.call(expr)) return(FALSE)

  call_head <- expr[[1L]]
  if (identical(call_head, as.name("case_when"))) return(TRUE)
  if (is.call(call_head) &&
      identical(call_head[[1L]], as.name("::")) &&
      length(call_head) == 3L &&
      identical(call_head[[3L]], as.name("case_when"))) {
    return(TRUE)
  }

  any(vapply(as.list(expr)[-1L], contains_unsupported_cluster_call, logical(1L)))
}

#' Evaluate an image or numeric/logical contrast over complete 3D volumes
#'
#' @param img_obj A ggbrain_images object.
#' @param definition A numeric or logical image expression.
#' @param contrasts Optional named character contrast definitions available to the expression.
#' @return A list with values, mask, and reference_image.
#' @keywords internal
evaluate_definition_3d <- function(img_obj, definition, contrasts = NULL) {
  checkmate::assert_class(img_obj, "ggbrain_images")
  checkmate::assert_string(definition)

  if (is.null(contrasts)) {
    contrasts <- list()
  } else if (is.character(contrasts)) {
    contrasts <- as.list(contrasts)
  }
  checkmate::assert_list(contrasts, types = "character")
  if (length(contrasts) > 0L &&
      (is.null(names(contrasts)) || any(names(contrasts) == ""))) {
    stop("Cluster contrast definitions must be named.")
  }
  if (anyDuplicated(names(contrasts))) {
    stop("Cluster contrast definition names must be unique.")
  }

  image_names <- img_obj$get_image_names()
  expected_dims <- img_obj$dim()
  zero_tol <- img_obj$zero_tol
  image_cache <- new.env(parent = emptyenv())
  contrast_cache <- new.env(parent = emptyenv())
  resolving <- character(0)
  reference_images <- character(0)

  get_image <- function(name) {
    if (!exists(name, envir = image_cache, inherits = FALSE)) {
      value <- as.array(img_obj$get_images(name, drop = TRUE))
      value[is.na(value)] <- 0
      assign(name, value, envir = image_cache)
    }
    if (!name %in% reference_images) reference_images <<- c(reference_images, name)
    get(name, envir = image_cache, inherits = FALSE)
  }

  mask_array <- function(value, keep) {
    if (length(keep) == 1L && is.logical(keep)) {
      keep <- array(rep(isTRUE(keep), prod(expected_dims)), dim = expected_dims)
    }
    if (!is.logical(keep) || !identical(dim(keep), expected_dims)) {
      stop("Cluster contrast bracket conditions must evaluate to a logical volume matching the input images.")
    }
    if (length(value) == 1L) {
      value <- array(rep(value, prod(expected_dims)), dim = expected_dims)
    }
    if (!identical(dim(value), expected_dims)) {
      stop("The expression before a cluster contrast bracket must evaluate to a volume matching the input images.")
    }
    value[is.na(keep) | !keep] <- 0
    value
  }

  evaluate_expression <- function(expression, label) {
    parsed <- tryCatch(
      parse(text = expression),
      error = function(e) {
        stop(glue::glue("Could not parse cluster contrast '{label}': {e$message}"))
      }
    )
    if (length(parsed) != 1L) {
      stop("Cluster definitions must contain one numeric or logical expression; categorical/semicolon conjunctions are not supported.")
    }
    expr <- parsed[[1L]]
    if (contains_unsupported_cluster_call(expr)) {
      stop("Categorical case_when() contrasts are not supported for cluster definitions.")
    }

    transformed <- transform_array_subset_calls(expr)
    variables <- all.vars(transformed)
    unknown <- setdiff(variables, c(image_names, names(contrasts)))
    if (length(unknown) > 0L) {
      stop(glue::glue(
        "Cluster contrast '{label}' contains image or contrast name(s) not found: ",
        "{paste(unknown, collapse = ', ')}. Available images: {paste(image_names, collapse = ', ')}",
        if (length(contrasts) > 0L) ". Available contrasts: {paste(names(contrasts), collapse = ', ')}" else ""
      ))
    }

    eval_env <- new.env(parent = baseenv())
    eval_env$.mask_array <- mask_array
    for (variable in variables) {
      is_self_image <- variable %in% image_names &&
        length(resolving) > 0L &&
        identical(variable, resolving[[length(resolving)]])
      if (variable %in% names(contrasts) && !is_self_image) {
        eval_env[[variable]] <- resolve_contrast(variable)
      } else if (variable %in% image_names) {
        eval_env[[variable]] <- get_image(variable)
      } else {
        stop(glue::glue("Unable to resolve cluster contrast variable '{variable}'."))
      }
    }

    value <- tryCatch(
      eval(transformed, envir = eval_env),
      error = function(e) {
        stop(glue::glue("Error evaluating cluster contrast '{label}': {e$message}"))
      }
    )
    if (length(value) == 1L) {
      stop(glue::glue(
        "Cluster contrast '{label}' returned a scalar; it must evaluate to a per-voxel numeric or logical volume."
      ))
    }
    if (!identical(dim(value), expected_dims)) {
      value_dims <- dim(value)
      value_dims <- if (is.null(value_dims)) "<none>" else paste(value_dims, collapse = "x")
      stop(glue::glue(
        "Cluster contrast '{label}' returned dimensions {value_dims}; expected {paste(expected_dims, collapse = 'x')}."
      ))
    }
    if (!is.numeric(value) && !is.logical(value)) {
      stop("Cluster definitions must evaluate to numeric or logical volumes; categorical contrasts are not supported.")
    }
    value
  }

  resolve_contrast <- function(name) {
    if (exists(name, envir = contrast_cache, inherits = FALSE)) {
      return(get(name, envir = contrast_cache, inherits = FALSE))
    }
    if (name %in% resolving) {
      cycle_start <- match(name, resolving)
      cycle <- c(resolving[cycle_start:length(resolving)], name)
      stop(glue::glue("Circular cluster contrast dependency detected: {paste(cycle, collapse = ' -> ')}"))
    }

    resolving <<- c(resolving, name)
    on.exit(resolving <<- resolving[-length(resolving)], add = TRUE)
    value <- evaluate_expression(contrasts[[name]], name)
    assign(name, value, envir = contrast_cache)
    value
  }

  split_definition <- contrast_split(trimws(definition))
  expression <- trimws(split_definition$value)
  if (expression %in% names(contrasts)) {
    values <- resolve_contrast(expression)
  } else if (expression %in% image_names) {
    values <- get_image(expression)
  } else {
    values <- evaluate_expression(expression, definition)
  }

  if (length(reference_images) == 0L) {
    stop("Cluster contrast did not reference any input image.")
  }

  if (is.logical(values)) {
    mask <- !is.na(values) & values
    numeric_values <- array(as.numeric(values), dim = expected_dims)
    numeric_values[!mask] <- 0
  } else {
    mask <- is.finite(values) & abs(values) > zero_tol
    numeric_values <- values
    numeric_values[!is.finite(numeric_values)] <- 0
  }

  list(
    values = numeric_values,
    mask = mask,
    reference_image = reference_images[[1L]]
  )
}
