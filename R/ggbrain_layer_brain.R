#' @title R6 class for a single layer of a ggbrain panel using fill geom
#' @details 
#'   Note that this class is exported only for power users and rarely needs to be called directly
#'     in typical use of the package. Instead, look at `geom_brain()`.
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes
#' @importFrom rlang as_label
#' @return a `ggbrain_layer_brain` R6 class with fields related to a brain visual layer (relates to `geom_brain`)
#' @export
ggbrain_layer_brain <- R6::R6Class(
  classname = "ggbrain_layer_brain",
  inherit = ggbrain_layer,
  private = list(
    alpha_cutoff = NULL,
    alpha_power = 2,
    threshold_opacity = function(statistic) {
      pmin(1, abs(statistic) / private$alpha_cutoff)^private$alpha_power
    },
    voxel_alpha = function(data, expr) {
      statistic <- if (is.null(expr)) data$value else rlang::eval_tidy(expr, data)
      checkmate::assert_numeric(statistic, any.missing = TRUE)
      if (!length(statistic) %in% c(1L, nrow(data))) {
        stop("Mapped alpha must have one value per pixel or one constant value")
      }
      statistic <- rep_len(statistic, nrow(data))
      if (!is.null(private$alpha_cutoff)) {
        return(private$threshold_opacity(statistic))
      }
      if (any(statistic < 0 | statistic > 1, na.rm = TRUE)) {
        stop("Mapped alpha must be between 0 and 1, or supply alpha_cutoff for a statistic")
      }
      statistic
    },
    update_voxel_alpha = function() {
      if (!is.null(private$pvt_alpha_column)) {
        private$pvt_data[[private$pvt_alpha_column]] <- private$voxel_alpha(private$pvt_data, private$pvt_mapping$alpha)
      }
    }
  ),
  active = list(
    #' @field fill controls color of the filled in pixels for non-NA (valid) voxels. Note that this
    #'   \strong{sets} the fill color, while the \code{mapping=aes(fill=<value>)} would \strong{map} the fill to a column
    #'  in the data, consistent with ggplot2 logic.
    fill = function(value) {
      if (missing(value)) {
        private$pvt_fill
      } else {
        checkmate::assert_string(value)
        private$pvt_fill <- value
        private$pvt_has_fill <- TRUE
        private$pvt_fill_column <- NULL
      }
    },

    #' @field mapping the ggplot2 aesthetic mapping between the data columns and the display
    #' @details To set mapping, you must provide a ggplot2 aes() object. A geom_brain() layer requires
    #'   a `fill` aesthetic mapping, which controls the fill color of regions.
    mapping = function(value) {
      if (missing(value)) {
        private$pvt_mapping
      } else {
        checkmate::assert_class(value, "uneval")
        if (!is.null(value$outline)) {
          stop("geom_brain() supports fill and alpha mappings. Use geom_outline() for outlines.")
        }
        mapped_alpha <- !is.null(value$alpha) || !is.null(private$alpha_cutoff)
        if (mapped_alpha && !is.null(private$pvt_alpha)) stop("Choose fixed alpha or voxelwise opacity")
        if (mapped_alpha && is.null(value$fill) && is.null(private$pvt_fill)) {
          value$fill <- ggplot2::aes(fill = value)$fill
        }
        opacity <- if (mapped_alpha && !self$is_empty()) private$voxel_alpha(private$pvt_data, value$alpha)
        private$pvt_mapping <- value
        private$pvt_alpha_column <- if (mapped_alpha) ".ggbrain_alpha"
        if (!self$is_empty()) private$pvt_data[[".ggbrain_alpha"]] <- opacity
        private$pvt_fill_expr <- "{new_val}"
        if (is.null(value$fill)) {
          private$pvt_fill_column <- NULL
          private$pvt_has_fill <- FALSE
        } else {
          private$pvt_fill_column <- all.vars(value$fill) # pull out the fill column from aes
          if (length(private$pvt_fill_column) > 1L) stop("Cannot pass multiple columns as a fill mapping")

          # if the user modifies the fill column such as as.factor(), we need to carry through the expression to geom_raster
          # inside the add_raster method of ggbrain_layer, we use 'new_val' as the column name to plot. Create a glue expression here
          # that will be evaluated at the time of the add_raster.
          fill_expr <- rlang::as_label(value$fill)
          if (fill_expr != private$pvt_fill_column) {
            private$pvt_fill_expr <- sub(private$pvt_fill_column, "{new_val}", fill_expr, fixed = TRUE)
            # if user is wrapping column name in factor or as.factor, treat as factor internally
            # also strip factor conversion from fill expression so that ggplot doesn't relevel the factor inline, undermining unify_scales
            if (private$pvt_fill_expr == "factor({new_val})" ||
              private$pvt_fill_expr == "as.factor({new_val})") {
              private$pvt_categorical_fill <- TRUE
              private$pvt_fill_expr <- sub("(as.)*factor\\({new_val}\\)", "{new_val}", private$pvt_fill_expr, perl=TRUE)
            }
          }
          private$pvt_has_fill <- TRUE
        }
        if (!self$is_empty()) private$validate_layer()
      }
    }
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param name the name of this layer, used for referencing in layer and panel modifications
    #' @param definition an optional character string defining the image or contrast that should be used
    #'   to lookup data from a ggbrain_slices object. This is mostly used internally by the ggbrain + syntax
    #'   to allow layers to be defined without data in advance of the plot.
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
    #' @param mapping An [ggplot2::aes()] mapping for fill and optional voxelwise alpha.
    #'   See [geom_brain()] for mapping columns, images and contrasts.
    #' @param alpha_cutoff Positive statistic magnitude at which opacity reaches 1.
    #' @param alpha_power Positive, finite exponent for the fade below `alpha_cutoff`.
    #'   The default is 2 (quadratic). See [geom_brain()] for the opacity formula.
    #' @param fill A character string indicating the color used to fill all non-NA pixels in this layer. This is used in
    #'   distinction to \code{mapping=aes(fill=<variable>)}.
    #' @param fill_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
    #'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
    #' @param fill_holes the size of holes (in pixels) inside clusters to be filled by nearest neighbor imputation prior to display
    #' @param remove_specks the size of specks (in pixels) to be removed from each slice prior to display
    #' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    initialize = function(name = NULL, definition = NULL, 
      limits = NULL, breaks = integer_breaks(), show_legend = TRUE, interpolate = NULL, unify_scales=TRUE, alpha = NULL,
      mapping = ggplot2::aes(fill=value), fill = NULL, fill_scale = NULL, blur_edge = NULL,
      fill_holes = NULL, remove_specks = NULL, trim_threads = NULL, data = NULL,
      alpha_cutoff = NULL, alpha_power = 2) {
      checkmate::assert_number(alpha_cutoff, finite = TRUE, lower = 0, null.ok = TRUE)
      if (!is.null(alpha_cutoff) && alpha_cutoff == 0) stop("alpha_cutoff must be positive")
      private$alpha_cutoff <- alpha_cutoff
      checkmate::assert_number(alpha_power, finite = TRUE, lower = 0)
      if (alpha_power == 0) stop("alpha_power must be positive")
      if (is.null(alpha_cutoff) && alpha_power != 2) stop("Set alpha_cutoff to use a custom alpha_power")
      private$alpha_power <- alpha_power

      # common initialization steps
      super$initialize(
        name, definition, limits, breaks, show_legend, interpolate, unify_scales,
        alpha, blur_edge, fill_holes, remove_specks, trim_threads, data = NULL
      )

      # fill-specific initialization steps
      if (!is.null(fill)) self$fill <- fill # fixed fill
      self$fill_scale <- fill_scale

      if (is.null(mapping) && !is.null(alpha_cutoff)) mapping <- ggplot2::aes(fill = value)
      if (!is.null(mapping)) self$mapping <- mapping
      if (!is.null(data)) self$data <- data
    },

    #' @description Draw the fill and its layer-specific opacity key.
    #' @param base_gg The ggplot to which the layer is added.
    add_to_gg = function(base_gg) {
      if (is.null(private$pvt_alpha_column)) return(super$add_to_gg(base_gg))
      if (!is.null(base_gg$scales$get_scales("alpha"))) {
        base_gg <- base_gg + ggnewscale::new_scale("alpha")
      }
      gg <- super$add_to_gg(base_gg)
      cutoff <- private$alpha_cutoff
      breaks <- c(0, 0.5, 1)
      labels <- breaks
      title <- "Opacity"
      if (!is.null(cutoff)) {
        expr <- private$pvt_mapping$alpha
        source <- if (is.null(expr)) self$definition else rlang::as_label(expr)
        title <- paste0("|", source, "| (power ", format(private$alpha_power), ")")
        magnitudes <- c(0, cutoff / 2, cutoff)
        labels <- c("0", format(magnitudes[2L]), paste0("\u2265", format(cutoff)))
        breaks <- private$threshold_opacity(magnitudes)
      }
      # Keep independent layers' alpha guides from merging and overprinting their swatches.
      guide <- if (self$show_legend) {
        ggplot2::guide_legend(order = length(gg$layers), override.aes = list(fill = "grey35"))
      } else "none"
      gg + ggplot2::scale_alpha_identity(name = title, limits = c(0, 1), breaks = breaks,
                                         labels = labels, guide = guide)
    }
  )
)
