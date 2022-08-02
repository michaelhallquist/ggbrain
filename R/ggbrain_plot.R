#' An R6 class for constructing a ggbrain plot from a ggbrain_slices object
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom purrr transpose
#' @importFrom patchwork wrap_plots
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

    # compile all anotations into a slice-wise list that can be added to each panel
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
    annotations = function(value) {
      if (missing(value)) {
        return(private$pvt_annotations)
      } else {
        checkmate::assert_list(value, null.ok = TRUE)
        private$pvt_annotations <- value
      }
    },
    region_labels = function(value) {
      if (missing(value)) {
        return(private$pvt_region_labels)
      } else {
        checkmate::assert_list(value, null.ok=TRUE)
        private$pvt_region_labels <- value
      }
    }
  ),
  public=list(
    #' @description instantiate a new instance of a ggbrain_plot object
    #' @param slice_data a ggbrain_slices object generated by ggbrain_images$get_slices()
    initialize=function(slice_data = NULL) {
      checkmate::assert_class(slice_data, "ggbrain_slices")
      private$pvt_slices <- slice_data
      private$pvt_nslices <- length(slice_data$slice_index)
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

      if (!is.null(slice_indices)) {
        slice_df <- slice_df %>%
          dplyr::filter(slice_index %in% !!slice_indices)
      }

      # get a list of the same length as the slice data that contains annotations for each slice
      all_annotations <- private$compiled_annotations()

      # lookup ranges of each layer and unique values of labels
      img_ranges <- private$pvt_slices$get_ranges(slice_indices)
      img_uvals <- private$pvt_slices$get_uvals(slice_indices)

      # generate a list of panel objects that combine layers and slice data
      private$pvt_ggbrain_panels <- lapply(seq_len(nrow(slice_df)), function(i) {
        # match slice data with layers

        comb_data <- slice_df$slice_data[[i]][layer_sources] # subset to only relevant data

        # list of layers
        slc_layers <- lapply(seq_along(layers), function(j) {
          l_obj <- layers[[j]]$clone(deep = TRUE)
          df <- comb_data[[j]]
          l_obj$data <- df # set slice-specific data (this will also set properties such as whether fill layer is categorical)

          if (isTRUE(l_obj$unify_scales)) {
            if (isTRUE(l_obj$categorical_fill)) {
              f_col <- l_obj$fill_column
              # unify factor levels across slices
              f_levels <- img_uvals %>%
                filter(layer == !!l_obj$source) %>%
                pull(uvals)
              l_obj$data[[f_col]] <- factor(l_obj$data[[f_col]], levels = f_levels)
              l_obj$fill_scale$drop <- FALSE # don't drop unused levels (would break unified legend)
              l_obj$fill_scale$na.translate <- FALSE
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

        ggbrain_panel$new(
          layers = slc_layers,
          region_labels = slc_labels,
          #title = "Testing",
          bg_color = "black",
          text_color = "white",
          draw_border = FALSE,
          annotations = all_annotations[[i]]$annotate_settings, # get slice-relevant annotations as list of tibbles
          #xlab = "X lab",
          #ylab = "Y lab"
        )
      })

      return(self)
    },
    #' @description return a plot of all panels as a patchwork object
    plot = function() {
      # extract ggplot objects from panels and plot with patchwork wrap_plots
      patchwork::wrap_plots(lapply(private$pvt_ggbrain_panels, function(x) x$gg))
    }
    # generate_panel = function(r) {
    #   
    # }
  )
)

#' S3 method to allow for plot() syntax with ggbrain_panel objects
#' @export
plot.ggbrain_plot <- function(object) {
  object$plot()
}

#' ggbrain_r6 <- R6::R6Class(
#'   classname = "ggbrain",
#'   private = list(
#'     layer_imgs = list(), # keep original data?
#'     pvt_panels = list(),
#'     composite_plot = NULL,
#'     set_panels = function(panels) {
#'       if (checkmate::assert_class(panels, "gg")) {
#' 
#'       }
#'       checkmate::assert_list(panels)
#'       sapply(panels, function(x) { checkmate::assert_class(x, "ggbrain_panel") })
#'       private$pvt_panels <- panels
#'     }
#'   ),
#'   active = list(
#'     #' @field panels The ggplot panels
#'     panels = function(val) {
#'       if (missing(val)) {
#'         return(private$pvt_panels)
#'       } else {
#'         private$set_panels(val)
#'       }
#'     }
#'   ),
#'   public = list(
#'     #' @description generate empty ggbrain object
#'     initialize = function(panels = NULL) {
#'       if (is.null(panels)) {
#'         stop("Cannot create a ggbrain object without panels!")
#'       } else {
#'         self$set_panels(panels)
#'       }
#'     },
#'     
#'     #' @description plot the composite plot
#'     plot = function() {
#'       plot(private$composite_plot)
#'     },
#'     
#'     #' @description return the composite plot
#'     get_composite_plot = function() {
#'       private$composite_plot
#'     },
#' 
#'     #' @description future idea? -- multiple views based on cached data
#'     create_view = function(slices) {
#'       private$views <- c(private$views, "VIEW HERE")
#'     }
#'   )
#' )
#' 
#' # allow for gg + theme() type stuff at panel level
#' `+.ggbrain` <- function(gg, args) {
#'   gg_new <- gg$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
#'   gg_new$panels <- lapply(gg_new$get_panels(), function(gg) { gg + args }) # add to each panel
#'   return(gg_new)
#' }
#' 
#' # allow for gg %+% theme() stuff at composite level
#' # this does not create a new ggbrain object... just adds to the composite plot.
#' `%+%.ggbrain` <- function(gg, args) {
#'   # gg_new <- gg$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
#'   gg$get_composite_plot() + args
#'   #return(gg_new)
#' }
# 
# plot.ggbrain <- function(obj) {
#   obj$plot()
# }
# 

