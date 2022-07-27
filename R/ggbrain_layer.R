#' R6 class for a single layer of a ggbrain panel
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt
#' @importFrom ggnewscale new_scale_fill
#' @importFrom Matrix sparseMatrix
#' @importFrom imager as.cimg erode_square
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_name = NULL,
    pvt_definition = NULL,
    pvt_source = NULL, # character string specifying which layer (image/contrast) within ggbrain_slices pertains
    pvt_data = NULL, # the data.frame containing values for this layer
    pvt_use_labels = NULL, # whether to use value or label column of data
    pvt_label_col = ".default", # the label column within the data that should be used for plotting -- .default uses the first label column available
    pvt_unify_scales = NULL, # whether to equate scale limits or level across panels
    pvt_color_scale = NULL,
    pvt_show_legend = NULL,
    pvt_interpolate = FALSE,
    pvt_is_empty = NULL,
    pvt_bisided = FALSE, # only set by color_scale active binding
    pvt_outline_size = NULL,
    pvt_outline_color = "white",
    pvt_outline_only = FALSE, # whether this layer only generates an outline

    # function that traces the outline of the slice, grouped by image
    slice_to_outline = function(df, group_fac = TRUE) {
      # if we have a factor, splot on this and get outlines for each component
      if (is.factor(df$value)) {
        df_split <- df %>% group_split(value)
        by_roi <- TRUE
      } else {
        df_split <- list(df)
        by_roi <- FALSE
      }

      df_melt <- bind_rows(lapply(df_split, function(dd) {
        df_drop <- dd %>% na.omit() # drop NAs so that only valid image points are preserved
        # convert to a 0/1 matrix where 1s denote present voxels
        slc_mat <- as.matrix(sparseMatrix(i = df_drop$dim1, j = df_drop$dim2, x = 1, dims = c(max(df$dim1), max(df$dim2))))
        slc_img <- imager::as.cimg(slc_mat) # convert to cimg object

        # follows this logic: https://stackoverflow.com/questions/29878844/how-do-i-find-the-perimeter-of-objects-in-a-binary-image
        er <- imager::erode_square(slc_img, 3) # erode by very small square
        # res1 <- abs(er - slc_img) # retained images become -1 -- not sure why this is needed...
        res <- slc_img - er # retained images become -1
        ret <- reshape2::melt(as.matrix(res), varnames = c("dim1", "dim2"))
        if (isTRUE(by_roi)) {
          ret <- ret %>%
            mutate(value = as.integer(value), value = if_else(value == 0, NA_character_, as.character(dd$value[1])))
        } else {
          ret <- ret %>%
            mutate(value = as.integer(value), value = if_else(value == 0, NA_integer_, 1L))
        }

        # plot(res)
        return(ret)
      }))

      # TODO: test this approach against the simpler boundary function in imager

      return(df_melt)
    },

    # helper function to make positive and negative scales symmetric
    symmetrize_limits = function(df_neg, df_pos) {
      # if positive and negative limits are already set, then respect these
      # (most commonly, this is due to unify_scales)

      # don't symmetrize if not requested
      if (!private$pvt_color_scale$symmetric) return(invisible(NULL))

      neg_lims <- private$pvt_color_scale$neg_scale$limits
      pos_lims <- private$pvt_color_scale$pos_scale$limits

      if (is.null(neg_lims)) {
        neg_lims <- c(min(df_neg$value, na.rm = TRUE), max(df_neg$value, na.rm = TRUE))
      }

      if (is.null(pos_lims)) {
        pos_lims <- c(min(df_pos$value, na.rm = TRUE), max(df_pos$value, na.rm = TRUE))
      }

      biggest <- max(abs(neg_lims[1]), pos_lims[2])
      smallest <- min(abs(neg_lims[2]), pos_lims[1])

      private$pvt_color_scale$neg_scale$limits <- -1 * c(biggest, smallest)
      private$pvt_color_scale$pos_scale$limits <- c(smallest, biggest)
    },

    # private method to set default fill scale when not provided
    set_default_scale = function() {
      # don't set default if no data exist or if an existing scale is present
      if (!is.null(private$pvt_color_scale) || is.null(private$pvt_data)) {
        return(invisible(NULL)) # don't modify extant scale
      } else if (isTRUE(private$pvt_outline_only) && !is.null(private$pvt_outline_color)) {
        return(invisible(NULL)) # for outline only situation, don't try to set a scale
      }

      # detect appropriate default scale
      if (private$pvt_name == "underlay") {
        self$color_scale <- scale_fill_gradient(low = "grey8", high = "grey92")
        if (is.null(self$show_legend)) self$show_legend <- FALSE # default to hiding underlay scale
      } else {
        has_pos <- any(private$pvt_data$value > 0, na.rm = TRUE)
        has_neg <- any(private$pvt_data$value < 0, na.rm = TRUE)
        if (has_pos && has_neg) {
          #self$color_scale <- scale_fill_distiller(palette = "RdBu") # red-blue diverging
          self$color_scale <- scale_fill_bisided(
            neg_scale = scale_fill_distiller(palette = "Blues", direction = 1),
            pos_scale = scale_fill_distiller(palette = "Reds")
          ) # internal scale stack
        } else if (has_neg) {
          self$color_scale <- scale_fill_distiller(palette = "Blues", direction = 1)
        } else if (has_pos) {
          self$color_scale <- scale_fill_distiller(palette = "Reds")
        } else {
          warning("Cannot find positive or negative values") # TODO: support discrete/character labels
          self$color_scale <- scale_color_brewer(palette = "Set3")
        }
        if (is.null(self$show_legend)) self$show_legend <- TRUE
      }
      return(invisible(NULL))
    }
  ),
  active = list(
    #' @field name the name of this layer, used for referencing in layer and panel modifications
    name = function(value) {
      if (missing(value)) {
        return(private$pvt_name)
      } else {
        checkmate::assert_string(value)
        private$pvt_name <- value
      }
    },

    #' @field definition a character string specifying the image name or contrast that defines this layer
    definition = function(value) {
      if (missing(value)) {
        return(private$pvt_definition)
      } else {
        checkmate::assert_string(value)
        private$pvt_definition <- value
      }
    },

    #' @field source a character string specifying the layer source within a relevant ggbrain_slices object.
    #'   This is used to lookup the right layer information when combining slices and layers together
    #'   Note that multiple layers can potentially have the same source, which is why a 1:1 mapping to name does not work
    source = function(value) {
      if (missing(value)) {
        return(private$pvt_source)
      } else {
        checkmate::assert_string(value)
        private$pvt_source <- value
      }
    },

    #' @field data the data.frame containing relevant data for this layer.
    data = function(value) {
      if (missing(value)) {
        return(private$pvt_data)
      } else {
        if (is.null(value)) {
          private$pvt_data <- NULL # clear data
          private$pvt_is_empty <- TRUE
        } else {
          checkmate::assert_data_frame(value)
          if (!all(c("dim1", "dim2", "value") %in% names(value))) {
            stop("data must contain dim1, dim2, and value")
          }

          private$pvt_data <- value
          private$pvt_is_empty <- FALSE # always make is_empty FALSE when we have data
          private$set_default_scale() # add default scale, if needed, after modifying data
        }
      }
    },

    #' @field color_scale a scale_fill_* object containing the ggplot2 fill scale for this layer
    color_scale = function(value) {
      if (missing(value)) {
        return(private$pvt_color_scale)
      } else if (is.null(value)) {
        private$pvt_color_scale <- NULL # reset
      } else if (checkmate::test_class(value, "Scale")) {
        stopifnot(value$aesthetics == "fill")

        # make sure that NAs are always drawn as transparent
        # if na.value is NA, omit it so that NA is not shown as a factor level
        if (!is.na(value$na.value) && value$na.value != "transparent") {
          value$na.value <- "transparent"
        }

        # default color scale to name of layer if not otherwise provided
        if (is.null(value$name) || inherits(value$name, "waiver")) {
          value$name <- self$name
        }

        if (inherits(value, "ScaleBisided")) {
          private$pvt_bisided <- TRUE # flag this layer as bisided

          # propagate na.value to pos and neg components
          value$pos_scale$na.value <- value$na.value
          value$neg_scale$na.value <- value$na.value
          value$pos_scale$name <- value$name
          value$neg_scale$name <- "" # no label on negative scale
        } else {
          private$pvt_bisided <- FALSE
        }

        private$pvt_color_scale <- value
      } else {
        stop("Cannot understand color_scale input. Should be a scale_fill_* object.")
      }
    },

    #' @field show_legend a logical indicating whether to show or hide the fill/color scale
    show_legend = function(value) {
      if (missing(value)) {
        return(private$pvt_show_legend)
      } else {
        checkmate::assert_logical(value, len = 1L)
        private$pvt_show_legend <- value
      }
    },

    #' @field use_labels if TRUE, use the $label column as the color on the plot for this layer. If a character string is passed,
    #'   this specifies the label column to plot from the label data.frame associated with the corresponding image
    use_labels = function(value) {
      if (missing(value)) {
        private$pvt_use_labels
      } else {
        if (checkmate::test_string(value)) {
          private$pvt_use_labels <- TRUE
          private$pvt_label_col <- value
        } else {
          checkmate::assert_logical(value, len = 1L)
          private$pvt_use_labels <- value
        }
      }
    },

    #' @field label_col A string indicating which column in the layer's data.frame should be used for plotting labeled
    #'   data for this layer. If not provided/specified, the first label column in the data will be used.
    label_col = function(value) {
      if (missing(value)) {
        private$pvt_label_col
      } else {
        checkmate::assert_string(value)
        private$pvt_label_col <- value
      }
    },

    #' @field unify_scales a logical indicating whether to unify scale limits and levels when this layer
    #'   is added across many panels
    unify_scales = function(value) {
      if (missing(value)) {
        private$pvt_unify_scales
      } else {
        checkmate::assert_logical(value, len=1L)
        private$pvt_unify_scales <- value
      }
    },

    #' @field bisided read-only access to whether this layer uses a bisided color scale
    bisided = function(value) {
      if (missing(value)) private$pvt_bisided
      else stop("Cannot assign bisided")
    },

    #' @field controls size of outline drawn around non-NA (valid) voxels
    outline_size = function(value) {
      if (missing(value)) {
        private$pvt_outline_size
      } else {
        checkmate::assert_integerish(value, len=1L, lower=1)
        private$pvt_outline_size <- value
      }
    },

    #' @field outline_color controls color of outline draw around non-NA (valid) voxels
    outline_color = function(value) {
      if (missing(value)) {
        private$pvt_outline_color
      } else {
        checkmate::assert_string(value)
        private$pvt_outline_color <- value
      }
    },

    #' @field outline_only if TRUE, then only the outline is drawn for this layer, not the fill
    outline_only = function(value) {
      if (missing(value)) {
        private$pvt_outline_only
      } else {
        checkmate::assert_logical(value, len = 1L)
        private$pvt_outline_only <- value
        # set default outline size if outline_only is turned on, but no outline_size is set
        if (isTRUE(value) && is.null(private$pvt_outline_size)) private$pvt_outline_size <- 1L
      }
    }
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param name the name of this layer, used for referencing in layer and panel modifications
    #' @param definition an optional character string defining the image or contrast that should be used
    #'   to lookup data from a ggbrain_slices object. This is mostly used internally by the ggbrain + syntax
    #'   to allow layers to be defined without data in advance of the plot.
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    #' @param color_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param use_labels if TRUE, plot the label column as the color
    #' @param label_col a character string indicating which label within the layer data.frame to use for plotting
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param outline_size controls the thickness of outlines
    #' @param outline_color controls the color of outlines
    #' @param outline_only controls whether only the outline is drawn
    initialize = function(name = NULL, definition = NULL, data = NULL, color_scale = NULL, limits = NULL,
                          breaks = NULL, show_legend = TRUE, interpolate = NULL, use_labels = FALSE, label_col = NULL, unify_scales=TRUE,
                          outline_size = NULL, outline_color = NULL, outline_only = FALSE) {

      if (is.null(name)) name <- "layer"
      checkmate::assert_numeric(limits, len = 2L, null.ok = TRUE)
      checkmate::assert_logical(interpolate, len=1L, null.ok = TRUE)

      # uses active bindings to validate inputs
      self$name <- name
      self$color_scale <- color_scale
      self$show_legend <- show_legend
      self$definition <- definition
      self$use_labels <- use_labels
      if (!is.null(label_col)) self$label_col <- label_col
      self$unify_scales <- unify_scales

      # allow for empty layers with data added later
      self$data <- data

      if (!is.null(interpolate)) {
        private$pvt_interpolate <- interpolate
      }

      if (!is.null(limits)) self$set_limits(limits)
      if (!is.null(breaks)) self$set_breaks(breaks)
      if (!is.null(outline_size)) self$outline_size <- outline_size
      if (!is.null(outline_color)) self$outline_color <- outline_color
      if (!is.null(outline_only)) self$outline_only <- outline_only
    },

    #' @description set the limits for this layer's scale
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's scale
    set_limits = function(limits) {
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_color_scale$limits <- limits
      return(self)
    },

    #' @description set the limits for this layer's positive scale (only relevant to bisided)
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's positive scale
    set_pos_limits = function(limits) {
      stopifnot(isTRUE(private$pvt_bisided))
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_color_scale$pos_scale$limits <- limits
      return(self)
    },

    #' @description set the limits for this layer's positive scale (only relevant to bisided)
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's positive scale
    set_neg_limits = function(limits) {
      stopifnot(isTRUE(private$pvt_bisided))
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_color_scale$neg_scale$limits <- limits
      return(self)
    },

    #' @description set the breaks element of this layer's scale
    #' @param breaks a function used to label the breaks
    set_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_color_scale$breaks <- breaks
    },

    #' @description set the breaks element of this layer's positive scale (only relevant to bisided)
    #' @param breaks a function used to label the positive breaks
    set_pos_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_color_scale$pos_scale$breaks <- breaks
    },

    #' @description set the breaks element of this layer's negative scale (only relevant to bisided)
    #' @param breaks a function used to label the negative breaks
    set_neg_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_color_scale$neg_scale$breaks <- breaks
    },

    #' @description plot this layer alone (mostly for debugging)
    plot = function() {
      if (self$is_empty()) stop("Cannot generate plot because this layer has no data! Use $data to add.")

      # create empty plot
      g <- ggplot(data = private$pvt_data, aes(x=dim1, y=dim2, fill=value))

      # add relevant marks to layer
      g <- self$add_to_gg(g)

      return(g)
    },

    #' @description method to add this layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      # skip out if no data exist in this layer
      if (self$is_empty()) {
        warning(glue::glue("No data in layer {self$name}! Not adding to ggplot object."))
        return(base_gg)
      }
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      n_scales <- length(base_gg$scales$scales) # will be less than n_layers when show.legend is FALSE
      ret <- base_gg # ggplot object to modify and return

      # Handle bisided situation
      # Note that geom_raster does best when there is a value at every square. Otherwise, this throws:
      # "Raster pixels are placed at uneven vertical intervals and will be shifted"
      # This appears to be ignorable, but is concerning and slightly annoying as a warning message
      # Thus, we create parallel data.frames with NAs, rather than a simple filter for pos or neg
      if (isTRUE(private$pvt_bisided)) {
        df <- private$pvt_data %>%
          dplyr::mutate(value=if_else(value < 0, value, NA_real_))

        df_pos <- private$pvt_data %>%
          dplyr::mutate(value=if_else(value > 0, value, NA_real_))

        private$symmetrize_limits(df, df_pos)

        cscale <- private$pvt_color_scale$neg_scale
      } else {
        df <- private$pvt_data
        cscale <- private$pvt_color_scale
      }

      # compute outline data.frames if requested
      if (!is.null(self$outline_size) && self$outline_size > 0L) {
        df_outline <- private$slice_to_outline(df)

        if (isTRUE(private$pvt_bisided)) {
          df_outline_pos <- private$slice_to_outline(df_pos)
        }
      }

      # only add fills if outline_only is FALSE
      # TODO: allow outlines to be mapped to data values... need some sort of hook. Probably outline_color is NULL, but outline_only = TRUE
      if (isFALSE(private$pvt_outline_only)) {
        new_val <- paste0("value", n_layers + 1L) # new_scale_fill depends on the fill aesthetic mapping differing by layer
        df <- df %>%
          dplyr::rename(!!new_val := value)

        if (n_layers > 0L) {
          # if there is already a layer, we need to create a new slot for a fill aesthetic
          ret <- base_gg + ggnewscale::new_scale_fill()
        }

        ret <- ret +
          geom_raster(
            data = df, mapping = aes_string(x = "dim1", y = "dim2", fill = new_val),
            show.legend = private$pvt_show_legend, interpolate = private$pvt_interpolate
          ) +
          cscale

        if (isTRUE(private$pvt_bisided)) {
          # add positive scale above negative to have it appear above it
          new_val <- paste0("value", n_layers + 2L) # additional layer for positive values
          df_pos <- df_pos %>%
            dplyr::rename(!!new_val := value)

          ret <- ret + ggnewscale::new_scale_fill() +
            geom_raster(
              data = df_pos, mapping = aes_string(x = "dim1", y = "dim2", fill = new_val),
              show.legend = private$pvt_show_legend, interpolate = private$pvt_interpolate
            ) +
            private$pvt_color_scale$pos_scale

          if (isTRUE(private$pvt_show_legend)) {
            # force color bar order: +1 is negative, +2 is positive. Lower orders are positioned lower on legend
            ret$scales$scales[[n_scales + 1]]$guide <- guide_colorbar(order = n_scales + 2, available_aes = c("fill", "fill_new"))
            ret$scales$scales[[n_scales + 2]]$guide <- guide_colorbar(order = n_scales + 1, available_aes = c("fill", "fill_new"))
          }
        }

      }

      if (!is.null(self$outline_size) && self$outline_size > 0L) {
        # na.omit is needed for geom_raster to draw hollowed outlines
        ret <- ret + geom_raster(data = df_outline %>% na.omit(), inherit.aes = FALSE, mapping = aes(x = dim1, y = dim2, fill = NULL), fill = self$outline_color, show.legend = FALSE)

        if (isTRUE(private$pvt_bisided)) {
          ret <- ret + geom_raster(data = df_outline_pos %>% na.omit(), inherit.aes = FALSE, mapping = aes(x = dim1, y = dim2, fill = NULL), fill = self$outline_color, show.legend = FALSE)
          #ret <- ret + geom_raster(data = df_outline_pos, mapping = aes(fill = NULL), fill = outline_color, show.legend = FALSE)
          #ret <- ret + geom_path(data = df_outline_pos %>% arrange(lu, dim1), color = outline_color, show.legend = FALSE, size = 2)
        }
      }

      return(ret)
    },

    #' @description return the data.frame associated with this layer
    #' @param add_layer_name if TRUE, adds a \code{layer_name} column to the data.frame for record-keeping.
    #'   Default: FALSE.
    get_data = function(add_layer_name = FALSE) {
      if (isTRUE(add_layer_name)) {
        df <- private$pvt_data
        df$layer_name <- private$pvt_name
        return(df)
      } else {
        return(private$pvt_data)
      }
    },

    #' @description returns TRUE if all values are NA or if the data has 0 rows
    is_empty = function() {
      private$pvt_is_empty
    }
  )
)

# cf. https://stackoverflow.com/questions/67279921/how-to-use-ggplot-add-inside-another-package

#' S3 method to support adding ggbrain_layer objects to an existing ggplot object
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_layer <- function(object, plot, object_name) {
  object$add_to_gg(plot) # adds the layer to the extant plot
}

## Trying to sort a two-sided scale with pos/neg gradients
## The problem is that if we have thresholded values, then the min/max may not be 0, but something like 3.09 etc.
# df <- data.frame(x=1:200, y = 1,
#                  stat_vals=c(
#                    sort(truncnorm::rtruncnorm(100, a=4, mean=6, sd=2)),
#                    sort(truncnorm::rtruncnorm(100, b=-4, mean=-6, sd=2))
#                  ))

# df <- df %>% mutate(stat_res = scales::rescale(stat_vals, c(0, 1)))

# #https://stackoverflow.com/questions/18700938/ggplot2-positive-and-negative-values-different-color-gradient
# ggplot(df, aes(x=x, y=y, fill=stat_vals)) + geom_tile() +
#   scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"),
#                        values=rescale(c(-1,0-.Machine$double.eps,0,0+.Machine$double.eps,1)))
