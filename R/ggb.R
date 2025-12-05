#' Generic R6 base class that is used to support + semantics
#' @details this object becomes a simple storage class that contains all relevant objects
#'   (e.g., ggbrain_images) required to generate a brain plot
#' @importFrom checkmate assert_class test_character
#' @importFrom R6 R6Class
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

    #' @field ggb_cluster_slices a list of cluster_slices_spec objects for deferred slice computation
    ggb_cluster_slices = NULL,

    #' @field ggb_cluster_data a list of data.frames containing cluster information from resolved cluster_slices
    ggb_cluster_data = NULL,

    #' @field ggb_contrasts a character vector of contrasts to be computed as part of this plot
    ggb_contrasts = NULL,

    #' @field ggb_layers a list of ggbrain_layer objects containing the bottom-to-top layers to be plotted
    ggb_layers = NULL,

    #' @field ggb_plot a ggbrain_plot object containing the specification of the plot
    ggb_plot = NULL,

    #' @field ggb_annotations a list of annotation objects
    ggb_annotations = NULL,

    #' @field ggb_region_labels a list of ggbrain_label objects to be added as text to label regions
    ggb_region_labels = NULL,

    #' @field ggb_target_resolution a list with target resolution settings (voxel_size, interpolation, interpolation_value)
    ggb_target_resolution = NULL,

    #' @field action what should this ggb object contribute to another when added with it?
    action = NULL,

    #' @description create a new ggb object. Note that inputs are always cloned to avoid
    #'   unintended modify-in-place behaviors of R6 classes.
    #' @param images a ggbrain_images object containing relevant images
    #' @param slices a character vector of slices to extract
    #' @param contrasts a character vector of contrasts to define and compute
    #' @param layers a list of ggbrain_layer objects
    #' @param labels a list of data.frames with labels that align with one or more images
    #' @param annotations a list of data.frames with annotations that will be added to specific slices
    #' @param region_labels a list of ggbrain_label objects with text-based labels to be drawn on the plot
    #' @param title overall title of the plot
    #' @param bg_color the background color of the overall plot
    #' @param text_color the text color of the overall plot
    #' @param base_size the base size of text on the plot
    #' @param action the action to be taken when adding this object to an existing ggb
    initialize=function(images=NULL, slices=NULL, contrasts = NULL, layers = NULL, labels = NULL, annotations=NULL, region_labels = NULL,
      title = NULL, bg_color=NULL, text_color=NULL, base_size = NULL, action=NULL) {
      if (!is.null(images)) {
        checkmate::assert_class(images, "ggbrain_images")
        self$ggb_images <- images$clone(deep=TRUE)
      }

      if (!is.null(slices)) self$add_slices(slices)

      if (!is.null(contrasts)) self$add_contrasts(contrasts)

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
        slices <- lapply(slices, function(x) list(coordinate = x))
      } else if (checkmate::test_list(slices)) {
        sapply(slices, function(x) stopifnot("coordinate" %in% names(x)))
      } else {
        stop("Cannot determine how to add slices with the input provided")
      }

      self$ggb_slices <- c(self$ggb_slices, slices)
      return(self)
    },

    #' @description add a cluster_slices_spec for deferred slice computation
    #' @param spec a cluster_slices_spec object from cluster_slices()
    add_cluster_slices = function(spec = NULL) {
      if (is.null(spec)) return(self) # nothing to do

      if (!is_cluster_slices_spec(spec)) {
        stop("spec must be a cluster_slices_spec object from cluster_slices()")
      }

      self$ggb_cluster_slices <- c(self$ggb_cluster_slices, list(spec))
      return(self)
    },

    #' @description retrieve cluster data from resolved cluster_slices specifications
    #' @details This method returns the cluster information computed during rendering.
    #'   It must be called after plot() or render() has been invoked, otherwise it returns NULL.
    #'   The returned data.frame contains columns: cluster_id, size (in voxels), com_i, com_j, com_k
    #'   (center of mass in voxel coordinates), com_x, com_y, com_z (center of mass in world/mm coordinates),
    #'   and slice_coord (the slice coordinate string used for plotting).
    #' @return A data.frame with cluster information, or NULL if no cluster_slices were used or render() hasn't been called
    get_cluster_data = function() {
      if (is.null(self$ggb_cluster_data) || length(self$ggb_cluster_data) == 0L) {
        return(NULL)
      }
      # Combine all cluster data into a single data.frame
      do.call(rbind, self$ggb_cluster_data)
    },

    #' @description add contrast definitions to the plot object
    #' @param contrasts a character vector of contrasts to compute as part of the plot generation
    add_contrasts = function(contrasts) {

      if (checkmate::test_character(contrasts)) {
        contrasts <- as.list(contrasts) # convert to list for type consistency
      }

      # for testing
      #contrasts <- c(x="a - b", "diff := a*b", c="x := y + z", "t + j")
      #contrasts <- c(x="a - b", "diff := a*b") #, c="x := y + z")
      has_names <- sapply(seq_along(contrasts), function(i) checkmate::test_named(contrasts[i]) )
      if (any(has_names)) {
        # verify that the named arguments don't use := operator
        nm <- contrasts[has_names]
        sapply(seq_along(nm), function(i) {
          if (grepl(":=", nm[i], fixed=TRUE)) {
            stop(glue::glue(
              "Contrasts must either use the named form c(nm='con def')",
              "or the := operator: 'nm := con def'.",
              " Problem with {names(nm)[i]}='{nm[i]}'"
            ))
          }
        })
      }

      if (any(!has_names)) {
        un <- contrasts[!has_names]
        un <- lapply(seq_along(un), function(i) {
          if (!grepl("^\\s*[\\w.]+\\s*:=.*$", un[[i]], perl = TRUE)) {
            stop(glue::glue(
              "Contrasts must either use the named form c(nm='con def')",
              "or the := operator: 'nm := con def'.",
              " Problem with '{un[[i]]}'"
            ))
          } else {
            return(contrast_split(un[[i]]))
          }
        })

        contrasts[!has_names] <- lapply(un, "[[", 2) # retain contrast definitions as value (named list)
        names(contrasts)[!has_names] <- sapply(un, "[[", 1) # use first element (contrast name) as list names
      }

      self$ggb_contrasts <- c(self$ggb_contrasts, contrasts)
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

        # orientation annotations carry params and will be expanded later
        if (rlang::has_name(aa, "orientation_params")) {
          return(tibble::tibble(orientation_params = list(aa$orientation_params)))
        }

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
    #' @param ... a named list of arguments where each is a data.frame with labels denoting corresponding images
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
      sapply(label_args, function(x) {
        if (!checkmate::test_subset(c("value"), names(x))) {
          stop("Labels data.frame must contain a 'value' column corresponding to the numeric image values to be labeled")
        }
      })

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
    #' @param guides Passes through to patchwork::plot_layout to control how legends are combined across plots. The default
    #'   is "collect", which collects legends within a given nesting level (removes duplicates).
    render = function(guides = "collect") {
      if (is.null(self$ggb_layers) || length(self$ggb_layers) == 0L) {
        warning("No brain layers added to this object yet. Use + geom_brain() or + geom_outline() to add.")
        return(NULL)
      }

      # reset cluster data and slices when render is called again
      base_slices <- self$ggb_slices
      self$ggb_cluster_data <- list()
      self$ggb_slices <- base_slices

      img <- self$ggb_images$clone(deep = TRUE)

      # Resolve any deferred cluster_slices specifications before adding slices
      outline_imgs <- character(0) # track any auto-generated outline images to refresh slices later
      if (!is.null(self$ggb_cluster_slices) && length(self$ggb_cluster_slices) > 0L) {
        for (spec in self$ggb_cluster_slices) {
          cluster_result <- resolve_cluster_slices(spec, img, self$ggb_layers)
          if (length(cluster_result$coordinates) > 0 && !identical(spec$action, "clusterized")) {
            # Convert to the list format expected by ggb_slices
            cluster_slice_list <- lapply(cluster_result$coordinates, function(coord) list(coordinate = coord))
            self$ggb_slices <- c(self$ggb_slices, cluster_slice_list)
          }
          # Annotate cluster provenance and store for later retrieval
          cluster_df <- cluster_result$cluster_data
          cluster_df$type <- if (identical(spec$action, "clusterized")) "display" else "slicing"
          cluster_df$cluster_source <- if (!is.null(spec$layer)) spec$layer else spec$definition
          cluster_df$cluster_layer <- if (!is.null(spec$cluster_layer_name)) spec$cluster_layer_name else NA_character_
          self$ggb_cluster_data <- c(self$ggb_cluster_data, list(cluster_df))

          # If this spec is for a clusterized fill layer, add the labeled volume and wire fill scale
          if (identical(spec$action, "clusterized") && !is.null(cluster_result$labeled_volume)) {
            cluster_img_name <- spec$cluster_layer_name %||% "clusterized"
            existing_imgs <- img$get_image_names()
            counter <- 1L
            while (cluster_img_name %in% existing_imgs) {
              counter <- counter + 1L
              cluster_img_name <- paste0("clusterized_", counter)
            }

            img$add_array_as_image(cluster_result$labeled_volume, name = cluster_img_name)

            # Update the placeholder layer to point to the new cluster image and scale
            if (!is.null(self$ggb_layers) && length(self$ggb_layers) > 0L) {
              # identify the clusterized layer by name
              cl_idx <- which(vapply(self$ggb_layers, function(ll) identical(ll$name, spec$cluster_layer_name), logical(1)))
              if (length(cl_idx) == 0L) cl_idx <- length(self$ggb_layers) # fallback
              cl_idx <- cl_idx[[1L]]
              # keep layer name/definition/source aligned with the actual image name
              self$ggb_layers[[cl_idx]]$name <- cluster_img_name
              self$ggb_layers[[cl_idx]]$definition <- cluster_img_name
              self$ggb_layers[[cl_idx]]$source <- cluster_img_name

              # If user supplied a fill_scale, use it; otherwise build a discrete scale from palette
              if (!is.null(spec$cluster_fill_scale)) {
                self$ggb_layers[[cl_idx]]$fill_scale <- spec$cluster_fill_scale
              } else {
                cluster_vals <- cluster_result$labeled_volume[!is.na(cluster_result$labeled_volume)]
                uids <- sort(unique(cluster_vals))
                pal <- grDevices::hcl.colors(length(uids), palette = "Dark 3")
                names(pal) <- as.character(uids)
                self$ggb_layers[[cl_idx]]$fill_scale <- ggplot2::scale_fill_manual(
                  values = pal, limits = names(pal), na.translate = FALSE, drop = FALSE
                )
              }

              # attach custom labels if requested and scale has no labels yet
              if (!is.null(spec$cluster_label_fields) && length(spec$cluster_label_fields) > 0L) {
                lab_fields <- spec$cluster_label_fields
                scale_obj <- self$ggb_layers[[cl_idx]]$fill_scale
                has_labels <- !is.null(scale_obj$labels) && !inherits(scale_obj$labels, "waiver")
                if (!has_labels) {
                  hdr <- img$get_headers(img_names = spec$cluster_source, drop = TRUE)
                  pixdim <- as.numeric(hdr$pixdim[2:4])
                  lims <- scale_obj$limits
                  ids <- if (!is.null(lims) && !is.function(lims) && length(lims) > 0L) {
                    as.character(lims)
                  } else {
                    as.character(sort(unique(cluster_result$cluster_data$cluster_index)))
                  }
                  # only label ids that exist in cluster_data
                  valid_ids <- intersect(ids, as.character(cluster_result$cluster_data$cluster_index))
                  label_fun <- function(id) {
                    parts <- character(0)
                    if ("number" %in% lab_fields) parts <- c(parts, id)
                    if ("voxels" %in% lab_fields) {
                      vox <- cluster_result$cluster_data$size[cluster_result$cluster_data$cluster_index == as.integer(id)]
                      parts <- c(parts, paste0(vox, " voxels"))
                    }
                    if ("size" %in% lab_fields) {
                      vox <- cluster_result$cluster_data$size[cluster_result$cluster_data$cluster_index == as.integer(id)]
                      vol_mm3 <- vox * prod(pixdim)
                      parts <- c(parts, paste0(round(vol_mm3), " mm^3"))
                    }
                    if (length(parts) == 0L) return(as.character(id))
                    if (length(parts) == 1L) return(parts)
                    paste0(parts[1L], ": ", paste(parts[-1L], collapse = ", "))
                  }
                  lab_vals <- vapply(valid_ids, label_fun, character(1))
                  self$ggb_layers[[cl_idx]]$fill_scale$labels <- lab_vals
                  if (is.null(self$ggb_layers[[cl_idx]]$fill_scale$limits)) {
                    self$ggb_layers[[cl_idx]]$fill_scale$limits <- valid_ids
                  }
                }
              }

              # Ensure categorical treatment
              self$ggb_layers[[cl_idx]]$mapping <- ggplot2::aes(fill = factor(value))
              self$ggb_layers[[cl_idx]]$show_legend <- isTRUE(spec$cluster_show_legend)
              self$ggb_layers[[cl_idx]]$unify_scales <- FALSE
            }
          }

          # Optionally add an outline layer derived from the identified clusters
          if (isTRUE(spec$outline) && !is.null(cluster_result$labeled_volume)) {
            # Generate a unique name for the outline image to avoid collisions
            outline_img_name <- "cluster_outline"
            existing_imgs <- img$get_image_names()
            counter <- 1L
            while (outline_img_name %in% existing_imgs) {
              counter <- counter + 1L
              outline_img_name <- paste0("cluster_outline_", counter)
            }

            img$add_array_as_image(cluster_result$labeled_volume, name = outline_img_name)

            cluster_vals <- cluster_result$labeled_volume[!is.na(cluster_result$labeled_volume)]
            if (length(cluster_vals) == 0L) next
            n_outline <- length(unique(cluster_vals))
            outline_ids <- as.character(sort(unique(cluster_vals)))
            outline_palette <- cluster_result$outline_palette
            outline_scale_user <- spec$outline_scale
            if (is.null(outline_scale_user)) outline_scale_user <- cluster_result$outline_scale

            use_mapping <- n_outline > 1L || !is.null(outline_scale_user)
            outline_levels <- names(outline_palette)
            outline_map <- if (use_mapping) ggplot2::aes(outline = outline_id, fill = NULL) else ggplot2::aes(outline = NULL, fill = NULL)
            outline_scale <- NULL
            outline_color <- NULL

            if (!is.null(outline_scale_user)) {
              outline_scale <- outline_scale_user
              # keep all cluster ids across slices even when custom scales omit limits
              if (is.null(outline_scale$limits) || inherits(outline_scale$limits, "waiver") || is.function(outline_scale$limits)) {
                outline_scale$limits <- outline_ids
              }
              outline_scale$drop <- FALSE
              if (is.null(outline_scale$na.translate)) outline_scale$na.translate <- FALSE
            } else if (use_mapping) {
              if (is.null(outline_palette) && n_outline > 0L) {
                outline_palette <- grDevices::hcl.colors(n_outline, palette = "Dark 3")
              }
              if (!is.null(outline_palette) && length(outline_palette) > 0L) {
                names(outline_palette) <- as.character(seq_len(length(outline_palette)))
              }
              outline_levels <- names(outline_palette)
              outline_scale <- ggplot2::scale_fill_manual(values = outline_palette, limits = outline_levels, na.translate = FALSE, drop = FALSE)
            } else if (!is.null(outline_palette) && length(outline_palette) > 0L) {
              outline_color <- outline_palette[[1L]]
            }

            outline_imgs <- c(outline_imgs, outline_img_name)
            show_legend_flag <- if (!is.null(spec$outline_show_legend)) spec$outline_show_legend else use_mapping

            outline_layer <- ggbrain_layer_outline$new(
              name = outline_img_name,
              definition = outline_img_name,
              mapping = outline_map,
              outline = outline_color,
              outline_scale = outline_scale,
              size = spec$outline_size,
              show_legend = show_legend_flag
            )

            self$add_layers(list(outline_layer))
          }
        }
      }

      if (!is.null(self$ggb_slices)) {
        img$add_slices(sapply(self$ggb_slices, "[[", "coordinate"))
      }

      if (!is.null(self$ggb_image_labels))  do.call(img$add_labels, self$ggb_image_labels)

      # Pass target resolution settings to get_slices if specified
      if (!is.null(self$ggb_target_resolution)) {
        slc <- img$get_slices(
          target_resolution = self$ggb_target_resolution$voxel_size,
          resample_interpolation = self$ggb_target_resolution$interpolation_value
        )
      } else {
        slc <- img$get_slices()
      }

      # compute any defined contrasts before looking at inline contrasts
      slc$compute_contrasts(self$ggb_contrasts)

      # image/contrast definitions for each layer
      layer_defs <- trimws(sapply(self$ggb_layers, "[[", "definition"))
      is_contrast <- !layer_defs %in% img$get_image_names() # if definition is just an image name, it's not a contrast to be computed
      # clusterized layers use computed cluster images; treat them as images, not contrasts
      cluster_layer_names <- vapply(self$ggb_cluster_slices, function(s) {
        if (!is.null(s$cluster_layer_name)) s$cluster_layer_name else NA_character_
      }, character(1))
      is_contrast[layer_defs %in% cluster_layer_names] <- FALSE
      layer_sources <- rep(NA_character_, length(layer_defs))

      # data sources for simple image layers
      layer_sources[!is_contrast] <- layer_defs[!is_contrast]

      # compute any contrasts requested
      if (any(is_contrast)) {
        # allow for definitions of the form "con1 := overlay*2" -- split at the :=
        inline_contrasts <- lapply(layer_defs[is_contrast], contrast_split)

        con_names <- sapply(inline_contrasts, "[[", "name")
        con_names[con_names == ""] <- make.unique(rep("con", sum(con_names == "")))
        inline_contrasts <- lapply(inline_contrasts, "[[", "value")
        names(inline_contrasts) <- con_names

        # fill in layer_sources with appropriate contrast names
        layer_sources[is_contrast] <- con_names

        slc$compute_contrasts(inline_contrasts)
      }

      # populate source fields in layers so that the appropriate layer can be looked up from slices
      for (ii in seq_along(layer_sources)) {
        self$ggb_layers[[ii]]$source <- layer_sources[ii]
      }

      # Align panel settings with the actual slices (after deduplication in get_slices)
      panel_settings <- lapply(slc$coord_input, function(ci) {
        idx <- which(vapply(self$ggb_slices, function(x) isTRUE(x$coordinate == ci), logical(1)))
        if (length(idx) == 0L) list() else self$ggb_slices[[idx[1L]]]
      })

      # need to line up layer names with data name in slice_data
      # populate ggbrain_plot object with compiled data
      self$ggb_plot$slices <- slc
      self$ggb_plot$layers <- self$ggb_layers
      self$ggb_plot$panel_settings <- panel_settings
      self$ggb_plot$annotations <- self$ggb_annotations # pass through annotations
      self$ggb_plot$region_labels <- self$ggb_region_labels # pass through region labels
      self$ggb_plot$generate_plot()
      
      # call the ggbrain_plot $plot method to convert to a patchwork object
      g <- self$ggb_plot$plot(guides)

      return(g)
    },

    #' @description get the slice data from the rendered plot for inspection
    #' @param image_name optional character string specifying a single image to extract. If NULL, returns
    #'   all images for each slice.
    #' @param slice_index optional integer vector specifying which slices to return. If NULL, returns all slices.
    #' @param as_matrix if TRUE, convert data.frames to matrices using df2mat(). Default: FALSE
    #' @return A list of slice data. If \code{image_name} is specified, returns a list of data.frames (or matrices)
    #'   for that image across slices. If \code{image_name} is NULL, returns a nested list where each element
    #'   contains all images for that slice.
    #' @details If \code{render()} has not been called yet, this method will call it automatically to
    #'   populate the slice data. This is useful for verifying that resampling/interpolation occurred
    #'   as expected when using \code{target_resolution()}.
    get_slice_data = function(image_name = NULL, slice_index = NULL, as_matrix = FALSE) {
      # Auto-render if slice data is not yet available
      if (is.null(self$ggb_plot) || is.null(self$ggb_plot$slices)) {
        self$render()
      }

      slice_data <- self$ggb_plot$slices$slice_data

      # Subset by slice index if requested
      if (!is.null(slice_index)) {
        checkmate::assert_integerish(slice_index, lower = 1, upper = length(slice_data))
        slice_data <- slice_data[slice_index]
      }

      # Extract specific image if requested
      if (!is.null(image_name)) {
        checkmate::assert_string(image_name)
        available_images <- names(slice_data[[1]])
        if (!image_name %in% available_images) {
          stop(glue::glue(
            "Image '{image_name}' not found. Available images: {paste(available_images, collapse = ', ')}"
          ))
        }
        slice_data <- lapply(slice_data, function(s) s[[image_name]])
      }

      # Convert to matrices if requested
      if (isTRUE(as_matrix)) {
        if (!is.null(image_name)) {
          # slice_data is a flat list of data.frames (one per slice)
          slice_data <- lapply(slice_data, df2mat)
        } else {
          # slice_data is a nested list (slice -> image -> data.frame), so use nested lapply
          slice_data <- lapply(slice_data, function(s) lapply(s, df2mat))
        }
      }

      return(slice_data)
    },

    #' @description plot this ggb object -- just an alias for render
    #' @param guides Passes through to patchwork::plot_layout to control how legends are combined across plots. The default
    #'   is "collect", which collects legends within a given nesting level (removes duplicates).
    #' @details requires that required elements are in place already.
    plot = function(guides="collect") {
      # returns a ggbrain_plot object
      obj <- self$render(guides)
      return(obj)
    }
  )
)

#' S3 method to allow for plot(x) syntax with ggbrain (ggb) objects
#' 
#' @param x the \code{ggb} object to be plotted
#' @param ... additional arguments passed to the plot method
#' @details
#'   This will plot the ggbrain object to the current graphics device
#' @return NULL, invisibly
#' @export
plot.ggb <- function(x, ...) {
  p <- x$plot(...) # convert to ggbrain_patchwork object
  if (!is.null(p)) plot(p, ...) # pass through to ggbrain_patchwork plotting function (that handles background color)
  invisible(p) # return the ggbrain_patchwork object, for further modification -- matches ggplot2 approach
}

#' S3 method to allow for render(x) syntax with ggbrain (ggb) objects
#' 
#' @param x the \code{ggb} object to be rendered to a \code{ggbrain_patchwork} object
#' @param ... additional arguments passed to the render method
#' @return the ggbrain_patchwork object that can be handed off to patchwork and ggplot2
#'   functions.
#' @export
render.ggb <- function(x, ...) {
  p <- x$render(...)
  invisible(p)
}

#' S3 method to allow for plot() syntax with rendered ggbrain patchwork objects
#' 
#' @param x the \code{ggbrain_patchwork} object to be plotted
#' @param ... additional arguments. Not currently used
#' @return \code{patchworkGrob} object, invisibly
#' @importFrom grid grid.newpage grid.rect grid.draw gpar
#' @importFrom patchwork patchworkGrob
#' @export
plot.ggbrain_patchwork <- function(x, ...) {
  grid.newpage()
  grid.rect(gp = gpar(fill = x$theme$plot.background$fill, col = NA))  # use plot background from object
  grob_obj <- patchworkGrob(x)
  grid.draw(grob_obj)  # Draws the plot on top of the background rectangle
  invisible(grob_obj) # return the patchwork grob in case it's of interest
}

#' default S3 method for ggbrain_patchwork objects (post-render)
#' @rdname plot.ggbrain_patchwork
#' @export
print.ggbrain_patchwork <- plot.ggbrain_patchwork

#' addition operator for ggb object to support ggplot-like syntax
#' @param o1 the first object inheriting the ggb class
#' @param o2 the second object inheriting the ggb class
#' @return a modified version of the o1 object with o2 added to it
#' @details Note that the addition operator always clones the underlying o1 object
#'   rather than modifying it in place
#' @export
`+.ggb` <- function(o1, o2) {
  actions <- c()
  if (is.null(o2$action)) {
    # nothing to do
    # actually, combine o2 with o1 to the extent possible
    if (!is.null(o2$ggb_slices)) actions <- c(actions, "add_slices")
    if (!is.null(o2$ggb_cluster_slices)) actions <- c(actions, "add_cluster_slices")
    if (!is.null(o2$ggb_contrasts)) actions <- c(actions, "add_contrasts")
    if (!is.null(o2$ggb_images$get_image_names())) actions <- c(actions, "add_images")
    if (!is.null(o2$ggb_layers)) actions <- c(actions, "add_layers")
    if (!is.null(o2$ggb_image_labels)) actions <- c(actions, "add_image_labels")
    if (!is.null(o2$ggb_annotations)) actions <- c(actions, "add_annotations")
    if (!is.null(o2$ggb_region_labels)) actions <- c(actions, "add_region_labels")
    if (!is.null(o2$ggb_target_resolution)) actions <- c(actions, "set_target_resolution")
  } else {
    # single action in an add step
    actions <- o2$action
  }

  if (is.null(actions)) return(o1) #nothing to do

  oc <- o1$clone(deep = TRUE)
  oc$action <- NULL # always make sure no action is needed in combined object

  for (aa in actions) {
    if (aa == "add_slices") {
      oc$add_slices(o2$ggb_slices)
    } else if (aa == "add_cluster_slices") {
      # Handle cluster_slices_spec from slices(cluster_slices(...))
      for (spec in o2$ggb_cluster_slices) {
        oc$add_cluster_slices(spec)
      }
    } else if (aa == "add_clusterized_layer") {
      for (spec in o2$ggb_cluster_slices) {
        oc$add_cluster_slices(spec)
      }
      oc$add_layers(o2$ggb_layers)
    } else if (aa == "add_contrasts") {
      oc$add_contrasts(o2$ggb_contrasts)
    } else if (aa == "add_images") {
      # use direct addition approach (by reference) -- yields single ggbrain_images object
      oc$ggb_images$add(o2$ggb_images)
    } else if (aa == "add_layers") {
      oc$add_layers(o2$ggb_layers)
    } else if (aa == "add_image_labels") {
      do.call(oc$add_image_labels, o2$ggb_image_labels)
    } else if (aa == "add_annotations") {
      oc$add_annotations(o2$ggb_annotations)
    } else if (aa == "add_region_labels") {
      oc$add_region_labels(o2$ggb_region_labels)
    } else if (aa == "set_target_resolution") {
      oc$ggb_target_resolution <- o2$ggb_target_resolution
    } else if (aa == "render") {
      # transform in to patchwork object
      oc <- oc$render()
    }
  }
  
  return(oc)
}
