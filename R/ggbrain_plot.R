#' @title An R6 class for constructing a ggbrain plot from a ggbrain_slices object
#' @details 
#'   Note that this class is exported only for power users and rarely needs to be called directly
#'     in typical use of the package. Instead, look at `ggbrain()`.
#' @importFrom ggplot2 scale_fill_distiller ggplot
#' @importFrom dplyr if_else %>% bind_rows left_join mutate select
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom checkmate assert_integerish assert_class
#' @return a `ggbrain_plot` R6 class containing fields related to a ggbrain plot object
#' @export
ggbrain_plot <- R6::R6Class(
  classname="ggbrain_plot",
  private = list(
    pvt_slices = NULL, # ggbrain_slices object with data that define plot
    pvt_layers = NULL, # list of ggbrain_layer objects
    pvt_ggbrain_panels = NULL,
    pvt_nslices = NULL,
    pvt_annotations = NULL, # list of annotations that are added to each panel -- should be one element per slice
    pvt_region_labels = NULL, # list of ggbrain_label objects that should be added to plot for labeling regions
    pvt_panel_settings = NULL, # list of user settings to be passed through to panels
    pvt_bg_color = "gray90", # overall background color of plot
    pvt_text_color = "gray5", # overall text color of plot
    pvt_title = NULL,
    pvt_base_size = 14, # font size

    # compile all annotations into a slice-wise list that can be added to each panel
    compiled_annotations = function() {
      if (is.null(private$pvt_annotations)) return(NULL) # nothing to add
      all_ann <- dplyr::bind_rows(lapply(private$pvt_annotations, function(adf) {
        stopifnot(inherits(adf, "data.frame")) # sanity check

        # replicate annotations settings for all slices if slice_index is absent -- assume that user wants it on all slices
        if (!"slice_index" %in% names(adf)) {
          adf <- dplyr::bind_rows(lapply(seq_len(private$pvt_nslices), function(ss) {
            df_ss <- adf
            df_ss$slice_index <- ss
            return(df_ss)
          }))
        } else {
          checkmate::assert_integerish(adf$slice_index, lower=1L, upper=private$pvt_nslices)
        }

        # fill in coord_label from slice data
        if (any(adf$label == ".coord_label")) {
          lab_df <- data.frame(slice_index = private$pvt_slices$slice_index, .coord_label = private$pvt_slices$coord_label)
          adf <- adf %>%
            dplyr::left_join(lab_df, by = "slice_index") %>%
            dplyr::mutate(label = if_else(label == ".coord_label", .coord_label, label)) %>% # selectively replace '.coord_label' with its value
            dplyr::select(-.coord_label)
        }

        # Always convert into a nested data.frame with slice_index as key and all other columns as a list. This allows for
        #   different data types across each element of the pvt_annotations list (e.g., x as character versus x as numeric).
        adf <- adf %>% tidyr::nest(annotate_settings=-slice_index)

        return(adf)
      }))

      # split by slice for adding to panels -- use .drop=FALSE and create a factor so that the list is always the length
      # of the number of slices, with empty elements for any panel that lacks annotation
      all_ann %>%
        mutate(slice_index = factor(slice_index, levels=seq_len(private$pvt_nslices))) %>%
        group_by(slice_index, .drop=FALSE) %>%
        group_split(.keep=FALSE)
    }
  ),
  active = list(
    #' @field slices a ggbrain_slices object containing all slice data for this plot
    slices = function(value) {
      if (missing(value)) {
        return(private$pvt_slices)
      } else if (is.null(value)) {
        return(invisible(NULL)) # do nothing
      } else {
        checkmate::assert_class(value, "ggbrain_slices")
        private$pvt_slices <- value
        private$pvt_nslices <- length(value$slice_index)
      }
    },
    #' @field layers a list of ggbrain_layer objects for this plot. Note that in assignment, the
    #'   input can be a list of ggbrain_layer objects, or a list of lists where each inner element
    #'   specifies the settings for that layer. Example: `list(list(name='hello', fill_scale=scale_fill_distiller())`
    layers = function(value) {
      if (missing(value)) {
        private$pvt_layers
      } else {
        if (inherits(value, "ggbrain_layer")) {
          value <- list(value) # wrap as single element list
        }

        checkmate::assert_list(value)
        l_gg <- sapply(value, function(x) inherits(x, "ggbrain_layer"))
        l_li <- sapply(value, function(x) inherits(x, "list"))
        l_names <- sapply(value, "[[", "name")
        if (any(dupes <- duplicated(l_names))) {
          #warning(glue::glue("Layer names cannot be duplicates. We will make the following names unique: {paste(l_names[dupes], collapse=', ')}"))
          l_names <- make.unique(l_names)
          for (ii in seq_along(value)) value[[ii]]$name <- l_names[ii] # assign unique names back into layer object
        }

        if (all(l_gg)) {
          # the input is a list of ggbrain_layer objects, which can be assigned directly
          private$pvt_layers <- value
        } else if (all(l_li)) {
          # the input is a list of lists
          # use ggbrain_layer constructor to create layers for each element
          private$pvt_layers <- do.call(ggbrain_layer$new, value)
        } else {
          stop("Cannot determine how to assign layers based on input.")
        }
        names(private$pvt_layers) <- l_names # use $name element to name list of layers
      }
    },

    #' @field annotations a list of annotations to be added to this plot
    annotations = function(value) {
      if (missing(value)) {
        return(private$pvt_annotations)
      } else {
        checkmate::assert_list(value, null.ok = TRUE)
        private$pvt_annotations <- value
      }
    },

    #' @field region_labels a list of region_labels to be added to this plot
    region_labels = function(value) {
      if (missing(value)) {
        return(private$pvt_region_labels)
      } else {
        checkmate::assert_list(value, null.ok=TRUE)
        private$pvt_region_labels <- value
      }
    },

    #' @field panel_settings a list of panel settings (aesthetics) to be added to this plot
    panel_settings = function(value) {
      if (missing(value)) {
        return(private$pvt_panel_settings)
      } else {
        checkmate::assert_list(value, null.ok = TRUE)
        private$pvt_panel_settings <- value
      }
    },

    #' @field title overall plot title, added to composite plot by \code{patchwork::plot_annotation()}
    title = function(value) {
      if (missing(value)) {
        return(private$pvt_title)
      } else if (is.null(value)) {
        return(invisible(NULL)) # do nothing
      } else {
        if (inherits(value, "expression")) {
          stopifnot(length(value) == 1L)
          private$pvt_title <- value
        } else {
          checkmate::assert_string(value)
          private$pvt_title <- value
        }
      }
    },

    #' @field bg_color background color of plot
    bg_color = function(value) {
      if (missing(value)) {
        return(private$pvt_bg_color)
      } else if (is.null(value)) {
        return(invisible(NULL)) # do nothing
      } else {
        checkmate::assert_string(value)
        private$pvt_bg_color <- value
      }
    },

    #' @field text_color the color of text use across panels (can be overridden by panel settings)
    text_color = function(value) {
      if (missing(value)) {
        return(private$pvt_text_color)
      } else if (is.null(value)) {
        return(invisible(NULL)) # do nothing
      } else {
        checkmate::assert_string(value)
        private$pvt_text_color <- value
      }
    },

    #' @field base_size the base size of text used in ggplot theming
    base_size = function(value) {
      if (missing(value)) {
        return(private$pvt_base_size)
      } else if (is.null(value)) {
        return(invisible(NULL)) # do nothing
      } else {
        checkmate::assert_number(value, lower=0)
        private$pvt_base_size <- value
      }
    }
  ),
  public=list(
    #' @description instantiate a new instance of a ggbrain_plot object
    #' @param title overall plot title
    #' @param bg_color background color of plot
    #' @param text_color text color of plot
    #' @param base_size base size of text used in ggplot theming
    #' @param slice_data a ggbrain_slices object generated by ggbrain_images$get_slices()
    initialize=function(title = NULL, bg_color = NULL, text_color = NULL, base_size = NULL, slice_data = NULL) {
      self$title <- title
      self$bg_color <- bg_color
      self$text_color <- text_color
      self$base_size <- base_size
      self$slices <- slice_data
    },

    #' @description adds one or more ggbrain_layer objects to this plot
    #' @param layers a list of ggbrain_layer objects (can also be a list that just specifies names, definitions, etc.)
    add_layers = function(layers = NULL) {
      if (!is.null(layers)) {
        checkmate::assert_list(layers)
        stopifnot(all(sapply(layers, function(x) "name" %in% names(x))))
        private$pvt_layers <- c(private$pvt_layers, layers) # append in order
      }
      return(self)
    },

    #' @description removes all existing layers from this ggbrain_plot object
    reset_layers = function() {
      private$pvt_layers <- NULL
    },

    #' @description generate the plot
    #' @param layers a list of layers to be displayed on each panel, the order of which yields the
    #'   bottom-to-to drawing order within ggplot2. Each element of \code{layers} should be a list
    #'   that follows the approximate structure of the ggbrain_layer class, minimally including
    #'   the layer \code{name}, which is used to lookup data of images or contrasts within the
    #'   slice_data object. If NULL, all layers in the slices object will be plotted. If only
    #'   a character string is passed, then those layers will be plotted with default scales.
    #' @param slice_indices An optional subset of slice indices to display from the stored slice data
    #' @details In addition to \code{name}, the elements of a layer can include
    #'   \code{fill_scale} a ggplot2 scale object for coloring the layer. Should be a scale_fill_* object.
    #'   \code{limits} the numeric limits to use for the color scale of this layer
    #'   \code{breaks} the scale breaks to use for the color scale of this layer
    #'   \code{show_legend} if FALSE, the color scale will not appear in the legend
    generate_plot = function(layers=NULL, slice_indices=NULL) {
      possible_layer_names <- names(private$pvt_layers)
      if (is.null(layers)) {
        if (!is.null(private$pvt_layers)) {
          layers <- private$pvt_layers
        } else {
          stop("No layers specified and none are available in the $layers field. Cannot continue")
        }
      } else if (is.character(layers)) {
        checkmate::assert_subset(layers, possible_layer_names)
        layers <- private$pvt_layers[layers] # get relevant subset
      } else {
        stop("Unclear layers input to generate_plot()")
      }

      checkmate::assert_list(layers)
      layer_sources <- sapply(layers, "[[", "source")

      # each slice forms a ggbrain_panel
      slice_df <- private$pvt_slices$as_tibble()

      # each slice can have formatting settings for its panel
      panel_settings <- private$pvt_panel_settings

      if (!is.null(panel_settings) && length(panel_settings) != private$pvt_nslices) {
        stop(glue::glue("The length of the plot panel settings ({length(panel_settings)}) does not match the number of slices ({private$pvt_nslices})."))
      }

      if (!is.null(slice_indices)) {
        checkmate::assert_subset(slice_indices, seq_len(private$pvt_nslices))
        slice_df <- slice_df %>%
          dplyr::filter(slice_index %in% !!slice_indices)

        panel_settings <- panel_settings[slice_indices]
      }

      # get a list of the same length as the slice data that contains annotations for each slice
      all_annotations <- private$compiled_annotations()

      # handle inline specification of categorical fill layers by passing through additional labels columns to get_uvals
      f_cat <- sapply(layers, "[[", "categorical_fill")
      if (any(f_cat)) {
        ll <- list()
        f_cols <- sapply(layers, "[[", "fill_column")
        f_src <- sapply(layers, "[[", "source")
        for (pos in which(f_cat)) ll[[f_src[pos]]] <- unname(f_cols[pos])
      } else {
        ll <- NULL
      }

      # lookup ranges of each layer and unique values of labels
      img_ranges <- private$pvt_slices$get_ranges(slice_indices)
      img_uvals <- private$pvt_slices$get_uvals(slice_indices, add_labels = ll)
      
      # generate a list of panel objects that combine layers and slice data
      private$pvt_ggbrain_panels <- lapply(seq_len(nrow(slice_df)), function(i) {
        # match slice data with layers

        comb_data <- slice_df$slice_data[[i]][layer_sources] # subset to only relevant data

        # list of layers
        slc_layers <- lapply(seq_along(layers), function(j) {
          l_obj <- layers[[j]]$clone(deep = TRUE)
          l_obj$data <- comb_data[[j]] # set slice-specific data (this will also set properties such as whether fill layer is categorical)
          if (isTRUE(l_obj$categorical_fill)) l_obj$fill_scale$na.translate <- FALSE # don't park "NA" in legend for empty tiles

          if (isTRUE(l_obj$unify_scales)) {
            if (isTRUE(l_obj$categorical_fill)) {
              f_col <- l_obj$fill_column

              # unify factor levels across slices
              f_levels <- img_uvals %>%
                dplyr::filter(layer == !!l_obj$source & .label_col == !!f_col) %>%
                dplyr::pull(uvals)

              # For now, don't attempt to unify ordered types since this will mangle the order.
              # This should work as expected because levels are preserved for labeled data.
              # I think this may only be essential for inline factor coding in aes and that it may only be a problem because we factor() the
              # label column in $validate_layer(), rather than setting the factor at the overall image level prior to slicing.
              if (!is.ordered(l_obj$data[[f_col]])) {
                l_obj$data[[f_col]] <- factor(l_obj$data[[f_col]], levels = f_levels)
              }

              l_obj$fill_scale$drop <- FALSE # don't drop unused levels (would break unified legend)
            } else {
              if (isTRUE(l_obj$bisided)) {
                pos_lims <- img_ranges %>%
                  filter(layer == !!l_obj$source) %>%
                  select(low_pos, high_pos) %>%
                  unlist()
                l_obj$set_pos_limits(pos_lims)

                neg_lims <- img_ranges %>%
                  filter(layer == !!l_obj$source) %>%
                  select(low_neg, high_neg) %>%
                  unlist()
                l_obj$set_neg_limits(neg_lims)
              } else {
                lims <- img_ranges %>%
                  filter(layer == !!l_obj$source) %>%
                  select(low, high) %>%
                  unlist()
                l_obj$set_limits(lims)
              }
            }
          }

          return(l_obj)
        })

        if (!is.null(private$pvt_region_labels)) {
          slc_labels <- lapply(private$pvt_region_labels, function(ll) {
            ll$data <- slice_df$slice_labels[[i]][[ll$image]]
            return(ll)
          })
        } else {
          slc_labels <- NULL
        }

        unify_scales <- sapply(layers, "[[", "unify_scales")

        # panel settings i
        pan_i <- panel_settings[[i]]

        # use colors of overall plot if not specified
        if (is.null(pan_i$bg_color)) pan_i$bg_color <- private$pvt_bg_color
        if (is.null(pan_i$text_color)) pan_i$text_color <- private$pvt_text_color
        if (is.null(pan_i$base_size)) pan_i$base_size <- private$pvt_base_size

        ggbrain_panel$new(
          layers = slc_layers,
          title = pan_i$title,
          bg_color = pan_i$bg_color,
          text_color = pan_i$text_color,
          border_color = pan_i$border_color,
          border_size = pan_i$border_size,
          xlab = pan_i$xlab,
          ylab = pan_i$ylab,
          theme_custom = pan_i$theme_custom,
          annotations = all_annotations[[i]]$annotate_settings, # get slice-relevant annotations as list of tibbles
          region_labels = slc_labels
        )
      })

      return(self)
    },
    #' @description return a plot of all panels as a patchwork object
    #' @param guides Passes through to patchwork::plot_layout to control how legends are combined across plots. The default
    #'   is "collect", which collects legends within a given nesting level (removes duplicates).
    #' @param ... additional arguments. Not used currently   
    plot = function(guides = "collect", ...) {
      checkmate::assert_string(guides)
      checkmate::assert_subset(guides, c("collect", "keep", "auto"))

      # extract ggplot objects from panels and plot with patchwork wrap_plots
      pp <- patchwork::wrap_plots(lapply(private$pvt_ggbrain_panels, function(x) x$gg)) +
        patchwork::plot_layout(guides=guides) +
        patchwork::plot_annotation(
          title = private$pvt_title,
          theme = theme(
            plot.background = ggplot2::element_rect(fill = private$pvt_bg_color, color = NA),
            plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0, size = 1.4*private$pvt_base_size, color = private$pvt_text_color)
          )
        )
      
      class(pp) <- c("ggbrain_patchwork", class(pp)) # add ggbrain_patchwork class so that S3 plot method properly captures these objects
      return(invisible(pp))
    }
  )
)

#' S3 method to allow for plot() syntax with ggbrain_panel objects
#' @param x the \code{ggbrain_plot} object to be plotted
#' @param ... additional argument passed to the plot method
#' @export
plot.ggbrain_plot <- function(x, ...) {
  x$plot()
}
