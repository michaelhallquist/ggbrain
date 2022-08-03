#' Generic R6 base class that is used to support + semantics
#' @details this object becomes a simple storage class that contains all relevant objects
#'   (e.g., ggbrain_images) required to generate a brain plot
#' @importFrom checkmate assert_class
#' @keywords internal
ggb <- R6::R6Class(
  "ggb",
  private=list(),
  public=list(
    #' @field ggb_images ggbrain_images object for this plot
    ggb_images = NULL,

    #' @field ggb_image_labels a named list of data.frames that label corresponding images
    ggb_image_labels = NULL,

    #' @field ggb_slices list slices to extract for this plot
    ggb_slices = NULL,

    #' @field ggb_layers a list of ggbrain_layer objects containing the bottom-to-top layers to be plotted
    ggb_layers = NULL,

    #' @field ggb_plot a ggbrain_plot object containing the specification of the plot
    ggb_plot = NULL,

    #' @field ggb_annotations a list of annotation objects
    ggb_annotations = NULL,

    #' @field ggb_region_labels a list of ggbrain_label objects to be added as text to label regions
    ggb_region_labels = NULL,

    #' @field action what should this ggb object contribute to another when added with it?
    action = NULL,

    #' @description create a new ggb object. Note that inputs are always cloned to avoid
    #'   unintended modify-in-place behaviors of R6 classes.
    #' @param images a ggbrain_images object containing relevant images
    #' @param slices a character vector of slices to extract
    #' @param layers a list of ggbrain_layer objects
    #' @param labels a list of data.frames with labels that align with one or more images
    #' @param annotations a list of data.frames with annotations that will be added to specific slices
    #' @param region_labels a list of ggbrain_label objects with text-based labels to be drawn on the plot
    #' @param title overall title of the plot
    #' @param bg_color the background color of the overall plot
    #' @param text_color the text color of the overall plot
    #' @param base_size the base size of text on the plot
    #' @param action the action to be taken when adding this object to an existing ggb
    initialize=function(images=NULL, slices=NULL, layers = NULL, labels = NULL, annotations=NULL, region_labels = NULL,
      title = NULL, bg_color=NULL, text_color=NULL, base_size = NULL, action=NULL) {
      if (!is.null(images)) {
        checkmate::assert_class(images, "ggbrain_images")
        self$ggb_images <- images$clone(deep=TRUE)
      }

      if (!is.null(slices)) self$add_slices(slices)

      if (!is.null(layers)) {
        # form one-element list if a ggbrain_object is passed
        if (checkmate::test_class(layers, "ggbrain_layer")) layers <- list(layers)
        self$add_layers(layers)
      }

      if (!is.null(labels)) {
        checkmate::assert_list(labels, names = "unique")
        do.call(self$add_image_labels, labels)
      }

      if (!is.null(annotations)) {
        self$add_annotations(annotations)
      }

      if (!is.null(region_labels)) {
        self$add_region_labels(region_labels)
      }

      # initialize empty ggbrain_plot object for future population
      self$ggb_plot <- ggbrain_plot$new(title, bg_color, text_color, base_size)

      # for tracking addition actions
      if (!is.null(action)) {
        self$action <- action
      }
    },

    #' @description add layers from another ggb object to this one
    #' @param ilist a list of ggbrain_layer objects. If a ggb object is passed, we
    #'   will get this list from obj$ggb_layers
    add_layers = function(ilist) {
      if (checkmate::test_class(ilist, "ggb")) {
        ilist <- ilist$ggb_layers
      }
      checkmate::assert_list(ilist)
      sapply(ilist, function(x) checkmate::assert_class(x, "ggbrain_layer"))

      self$ggb_layers <- c(self$ggb_layers, ilist)

      names(self$ggb_layers) <- sapply(self$ggb_layers, "[[", "name")
      return(self)
    },

    #' @description add slices to the existing vector of slices
    #' @param slices a character vector of slices to be appended to the existing slices
    add_slices = function(slices=NULL) {
      if (is.null(slices)) return(self) # nothing to do
      
      if (checkmate::test_character(slices)) {
        # if slices is a simple character vector, store as list with coordinate element
        slices <- lapply(slices, function(x) list(coordinate=x))
      } else if (checkmate::test_list(slices)) {
        sapply(slices, function(x) stopifnot("coordinate" %in% names(x)))
      } else {
        stop("Cannot determine how to add slices with the input provided")
      }

      self$ggb_slices <- c(self$ggb_slices, slices)
      return(self)
    },

    #' @description add annotations to panels
    #' @param annotations a list or data.frame containing the annotations to add to each panel. Minimally,
    #'   the list or data.frame must contain \code{position} and \code{label} columns that define the position
    #'   and text to be added. Other arguments that pass through to ggplot2::annotate() can be provided as columns/elements
    #'   in \code{annotations} and these will be passed through to annotate
    add_annotations = function(annotations = NULL) {
      if (is.null(annotations)) {
        return(self) # no change
      }

      # wrap data.frame input as single-element list for type consistency
      if (checkmate::test_data_frame(annotations)) annotations <- list(annotations)

      checkmate::assert_list(annotations)

      # convert each element to a tibble
      ann <- lapply(annotations, function(aa) {
        checkmate::assert_multi_class(aa, c("data.frame", "list"))

        if (!rlang::has_name(aa, "geom")) aa$geom <- "text" # default to text geom
        if (aa$geom %in% c("label", "text")) {
          # enforce that label and position are present for text/label
          checkmate::assert_subset(c("label", "x", "y"), names(aa))
        }
        tibble::as_tibble(aa) # convert lists to tibbles so that singular values in one field are replicated if other fields are many
      })

      # append annotations
      self$ggb_annotations <- c(self$ggb_annotations, ann)
    },

    #' @description add labels to a given image
    #' @param labels a named list of data.frame objects where the names denote corresponding images
    add_image_labels = function(...) {
      label_args <- list(...)

      # return unchanged object if no input labels found
      if (is.null(label_args) || length(label_args) == 0L) {
        return(self)
      }

      label_names <- names(label_args)
      if (is.null(label_names) || any(label_names == "")) {
        stop("All arguments must be named, with the name referring to the image to be labeled.")
      }

      sapply(label_args, function(x) checkmate::assert_data_frame(x) )
      sapply(label_args, function(x) checkmate::assert_subset(c("value"), names(x)))

      self$ggb_image_labels <- c(self$ggb_image_labels, label_args)
      return(self)
    },

    #' @description add a list of ggbrain_label objects to the overall ggb for compiling a plot
    #' @param labels a list of data.frames with region labels that should be plotted on each slice. This is generated
    #'   internally by ggbrain_images$get_slices() in the $slice_labels field.
    add_region_labels = function(labels = NULL) {
      if (checkmate::test_class(labels, "ggbrain_label")) {
        labels <- list(labels) # make into one-element list for consistency
      }

      sapply(labels, function(x) checkmate::assert_class(x, "ggbrain_label"))
      self$ggb_region_labels <- c(self$ggb_region_labels, labels)
    },

    #' @description this method converts the ggb object into a compiled ggplot2 object that can then be passed to other
    #'   functions from cowplot, ggplot2, and patchwork. Once the object is rendered, it no longer retains the underlying ggb
    #'   fields that contain the elemental data.
    render = function() {
      if (is.null(self$ggb_layers) || length(self$ggb_layers) == 0L) {
        stop("No brain layers added to this object yet. Use + geom_brain() to add.")
      }

      img <- self$ggb_images$clone(deep = TRUE)
      img$add_slices(sapply(self$ggb_slices, "[[", "coordinate"))

      if (!is.null(self$ggb_image_labels))  do.call(img$add_labels, self$ggb_image_labels)
      slc <- img$get_slices()

      # image/contrast definitions for each layer
      layer_defs <- trimws(sapply(self$ggb_layers, "[[", "definition"))
      is_contrast <- !layer_defs %in% img$get_image_names() # if definition is just an image name, it's not a contrast to be computed
      layer_sources <- rep(NA_character_, length(layer_defs))

      # data sources for simple image layers
      layer_sources[!is_contrast] <- layer_defs[!is_contrast]

      # compute any contrasts requested
      if (any(is_contrast)) {
        # allow for definitions of the form "con1 := overlay*2"
        contrast_list <- lapply(layer_defs[is_contrast], function(x) {
          if (grepl("^\\s*\\w+\\s*:=.*$", x, perl = TRUE)) {
            con_name <- sub("^\\s*(\\w+)\\s*:=.*$", "\\1", x, perl = TRUE) # parse name
            con_val <- trimws(sub("^\\s*\\w+\\s*:=\\s*(.*)$", "\\1", x, perl = TRUE)) # parse contrast
          } else {
            con_name <- ""
            con_val <- x
          }

          return(c(name = con_name, value = con_val))
        })

        con_names <- sapply(contrast_list, "[[", "name")
        con_names[con_names == ""] <- make.unique(rep("con", sum(con_names == "")))
        contrast_list <- lapply(contrast_list, "[[", "value")
        names(contrast_list) <- con_names

        # fill in layer_sources with appropriate contrast names
        layer_sources[is_contrast] <- con_names

        slc$compute_contrasts(contrast_list)
      }

      # populate source fields in layers so that the appropriate layer can be looked up from slices
      for (ii in seq_along(layer_sources)) {
        self$ggb_layers[[ii]]$source <- layer_sources[ii]
      }

      # need to line up layer names with data name in slice_data
      # populate ggbrain_plot object with compiled data
      self$ggb_plot$slices <- slc
      self$ggb_plot$layers <- self$ggb_layers
      self$ggb_plot$panel_settings <- self$ggb_slices
      self$ggb_plot$annotations <- self$ggb_annotations # pass through annotations
      self$ggb_plot$region_labels <- self$ggb_region_labels # pass through region labels
      self$ggb_plot$generate_plot()

      return(self$ggb_plot$plot())
    },

    #' @description plot this ggb object
    #' @details requires that required elements are in place already.
    plot = function() {
      # returns a ggbrain_plot object
      obj <- self$render()
      return(obj)
    }
  )
)

#' S3 method to allow for plot() syntax with ggbrain (ggb) objects
#' @export
plot.ggb <- function(object) {
  object$plot()
}

#' addition operator for ggb object to support ggplot-like syntax
#' @param o1 the first object inheriting the ggb class
#' @param o2 the second object inheriting the ggb class
#' @return a modified version of the o1 object with o2 added to it
#' @details Note that the addition operator always clones the underlying o1 object
#'   rather than modifying it in place
#' @export
`+.ggb` <- function(o1, o2) {
  if (is.null(o2$action)) {
    # nothing to do
    return(o1)
  } else {
    oc <- o1$clone(deep = TRUE)
    oc$action <- NULL # always make sure no action is needed in combined object
    if (o2$action == "add_slices") {
      oc$add_slices(o2$ggb_slices)
    } else if (o2$action == "add_images") {
      # use direct addition approach (by reference) -- yields single ggbrain_images object
      oc$ggb_images$add(o2$ggb_images)
    } else if (o2$action == "add_layers") {
      oc$add_layers(o2$ggb_layers)
    } else if (o2$action == "add_image_labels") {
      do.call(oc$add_image_labels, o2$ggb_image_labels)
    } else if (o2$action == "add_annotations") {
      oc$add_annotations(o2$ggb_annotations)
    } else if (o2$action == "add_region_labels") {
      oc$add_region_labels(o2$ggb_region_labels)
    } else if (o2$action == "render") {
      # transform in to patchwork object
      oc <- oc$render()
    }
  }

  return(oc)
}
