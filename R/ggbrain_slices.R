#' R6 class for managing slice data for ggbrain plots
#' @importFrom dplyr bind_rows full_join group_by across mutate summarize filter
#' @importFrom tidyr pivot_wider pivot_longer unnest
#' @importFrom tibble tibble
#' @importFrom tidyselect across
#' @author Michael Hallquist
#' @keywords internal
ggbrain_slices <- R6::R6Class(
  classname="ggbrain_slices",
  private = list(
    pvt_slice_index=NULL,
    pvt_coord_input=NULL,
    pvt_coord_label=NULL,
    pvt_plane=NULL,
    pvt_slice_number=NULL,
    pvt_slice_data=list(),
    pvt_slice_matrix=list(),
    pvt_slice_labels=list(),
    pvt_contrast_data=list(),

    # helper function to combine slice image and contrast data
    # TODO: sort out how to avoid calculating this twice for labels and numeric values
    get_combined_data = function(slice_indices = NULL, only_labeled=FALSE) {
      if (is.null(slice_indices)) {
        slice_indices <- private$pvt_slice_index
      } else {
        checkmate::assert_integerish(slice_indices, lower = 1, upper = max(private$pvt_slice_index), unique = TRUE)
      }

      # calculate overall ranges across slices for unified scales
      img_slice <- private$pvt_slice_data[slice_indices]
      if (isTRUE(only_labeled)) {
        # which layers are labeled
        has_labels <- sapply(private$pvt_slice_data[[1]], function(x) "label" %in% names(x))
        stopifnot(sum(has_labels) > 0L) # would be a problem

        # subset img_slice to only labeled layers
        img_slice <- lapply(img_slice, function(ll) ll[has_labels])
      }

      if (all(sapply(img_slice, length) > 0L)) {
        # img_data <- purrr::transpose(img_slice) %>% bind_rows(.id = "slice_index")
        # since this is a nested list, bind_rows will generate list-columns for each layer with corresponding layer data
        img_data <- img_slice %>% bind_rows(.id = "slice_index")
      } else {
        img_data <- NULL
      }

      img_contrast <- private$pvt_contrast_data[slice_indices]
      if (all(sapply(img_contrast, length) > 0L)) {
        # con_data <- purrr::transpose(img_contrast) %>% bind_rows(.id = "slice_index")
        con_data <- img_contrast %>% bind_rows(.id = "slice_index")
      } else {
        con_data <- NULL
      }

      if (!is.null(img_data) && !is.null(con_data)) {
        img_all <- img_data %>% inner_join(con_data, by = "slice_index")
      } else if (is.null(img_data)) {
        img_all <- con_data
      } else if (is.null(con_data)) {
        img_all <- img_data
      } else {
        stop("In $get_ranges, both $slice_data and $contrast_data are empty!")
      }

      # get names of all columns (layers) except the index
      layer_names <- grep("slice_index", names(img_all), invert = TRUE, value = TRUE)

      # convert into a long data frame with layer and slice_index as keys, drop NAs
      img_all <- img_all %>%
        tidyr::pivot_longer(cols = all_of(layer_names), names_to = "layer", values_to = "slice_data") %>%
        tidyr::unnest(slice_data) %>%
        dplyr::mutate(slice_index = as.integer(slice_index)) %>%
        dplyr::filter(!is.na(value)) # no point in keeping all the empty/NA data

      return(img_all)

    }
  ),

  # these active bindings create read-only access to class properties
  active = list(
    #' @field slice_index read-only access to the slice_index containing the slice numbers
    slice_index = function(value) {
      if (missing(value)) private$pvt_slice_index
      else stop("Cannot assign slice_index")
    },
    #' @field coord_input the input string used to lookup the slices
    coord_input = function(value) {
      if (missing(value)) private$pvt_coord_input
      else stop("Cannot assign coord_input")
    },
    #' @field coord_label the calculated x, y, or z coordinate of the relevant slice
    coord_label = function(value) {
      if (missing(value)) private$pvt_coord_label
      else stop("Cannot assign coord_label")
    },
    #' @field slice_number the slice number along the relevant axis of the 3D image matrix
    slice_number = function(value) {
      if (missing(value)) private$pvt_slice_number
      else stop("Cannot assign slice_number")
    },
    #' @field slice_data a nested list of data.frames where each element contains all data relevant to
    #'   that slice and the list elements within are each a given image
    slice_data = function(value) {
      if (missing(value)) private$pvt_slice_data
      else stop("Cannot assign slice_data")
    },
    #' @field slice_matrix the slice data in matrix form
    slice_matrix = function(value) {
      if (missing(value)) private$pvt_slice_matrix
      else stop("Cannot assign slice_matrix")
    },
    slice_labels = function(value) {
      if (missing(value)) private$pvt_slice_labels
      else stop("Cannot assign slice_labels")
    },
    contrast_data = function(value) {
      if (missing(value)) private$pvt_contrast_data
      else stop("Cannot assign contrast_data")
    }
  ),
  public = list(
    #' @description create a ggbrain_slices object based
    #' @param slice_df a data.frame generated by ggbrain_images$get_slices()
    #' @details If this becomes a user-facing/exported class, we may want a more friendly constructor
    initialize = function(slice_df = NULL) {
      checkmate::assert_data_frame(slice_df)

      # empty lists for populating unused fields -- match length of slice_df for consistency
      empty_list <- lapply(seq_len(nrow(slice_df)), function(i) list())

      df_names <- names(slice_df)
      private$pvt_coord_input <- if ("coord_input" %in% df_names) slice_df$coord_input else empty_list
      private$pvt_coord_label <- if ("coord_label" %in% df_names) slice_df$coord_label else empty_list
      private$pvt_plane <- if ("plane" %in% df_names) slice_df$plane else empty_list
      private$pvt_slice_index <- if ("slice_index" %in% df_names) slice_df$slice_index else seq_along(df_names)
      private$pvt_slice_number <- if ("slice_number" %in% df_names) slice_df$slice_number else empty_list
      private$pvt_slice_data <- if ("slice_data" %in% df_names) slice_df$slice_data else empty_list
      private$pvt_slice_labels <- if ("slice_labels" %in% df_names) slice_df$slice_labels else empty_list
      private$pvt_slice_matrix <- if ("slice_matrix" %in% df_names) slice_df$slice_matrix else empty_list
      private$pvt_contrast_data <- if ("contrast_data" %in% df_names) slice_df$contrast_data else empty_list
    },

    #' @description computes contrasts of the sliced image data
    #' @param contrast_list a named list or character vector containing contrasts to be computed.
    #'   The names of the list form the contrast names, while the values should be character strings
    #'   that use standard R syntax for logical tests, subsetting, and arithmetic
    #' @examples
    #' \dontrun{
    #'   slc <- ggbrain_slices$new(slice_df=my_data)
    #'   slc$compute_contrasts(list(pos_vals="overlay[overlay> 0]"))
    #' }
    compute_contrasts = function(contrast_list=NULL) {
      if (checkmate::test_class(contrast_list, "character")) {
        contrast_list <- as.list(contrast_list) # tolerate named character vector input
      }

      # force unique names of input contrats
      checkmate::assert_list(contrast_list, names = "unique")
      if (length(private$pvt_slice_data) == 0L) {
        stop("Cannot use $compute_contrasts() if there are no slice_data in the object")
      }

      # convert slice data to wide format to allow contrasts to be parsed
      wide <- lapply(private$pvt_slice_data, function(slc_xx) {
        ss <- slc_xx %>% dplyr::bind_rows()
        if ("label" %in% names(ss)) { # only pivot label if it is present
          vcols <- c("value", "label")
        } else {
          vcols <- c("value")
        }
        ss <- ss %>%
          tidyr::pivot_wider(
            id_cols = c(dim1, dim2),
            names_from = "image",
            names_glue = "{image}_{.value}",
            values_from = all_of(vcols)
          )

        names(ss) <- sub("_value$", "", names(ss)) # remove _value suffix to make evaluation of contrasts easier
        return(ss)
      })

      if (any(names(contrast_list) %in% names(private$pvt_contrast_data))) {
        overlap <- intersect(names(contrast_list), names(private$pvt_contrast_data))
        warning("Existing contrast data will be replaced for the following contrasts: ", paste(overlap, collapse=", "))
      }

      private$pvt_contrast_data <- lapply(seq_along(wide), function(ww) {
        c_data <- lapply(seq_along(contrast_list), function(cc) {
          contrast_parser(contrast_list[[cc]], data = wide[[ww]]) %>%
            mutate(image = names(contrast_list)[cc]) # tag contrasts with a label column
        }) %>% setNames(names(contrast_list))

        if (length(private$pvt_contrast_data) > 0L) {
          e_data <- private$pvt_contrast_data[[ww]]
        } else {
          e_data <- list()
        }
        e_data[names(c_data)] <- c_data # update relevant elements of extant data
        return(e_data)
      })

    },

    #' @description convert the slices object into a data.frame with list-columns for slice data elements
    as_tibble = function() {
      tibble::tibble(
        coord_input=private$pvt_coord_input,
        coord_label=private$pvt_coord_label,
        plane=private$pvt_plane,
        slice_index=private$pvt_slice_index,
        slice_number=private$pvt_plane,
        slice_data=private$pvt_slice_data,
        slice_labels=private$pvt_slice_labels,
        slice_matrix=private$pvt_slice_matrix,
        contrast_data=private$pvt_contrast_data
      )
    },

    #' @description returns a vector of the names of all image and contrast data available in this
    #'   ggbrain_slices object.
    get_image_names = function() {
      if (length(private$pvt_contrast_data) > 0L) {
        nmc <- names(private$pvt_contrast_data[[1]]) # first slice should be representative
      } else {
        nmc <- NULL
      }

      if (length(private$pvt_slice_data) > 0L) {
        nms <- names(private$pvt_slice_data[[1]]) # first slice should be representative
      } else {
        nms <- NULL
      }

      return(c(nms, nmc))
    },

    #' @description calculates the numeric ranges of each image/contrast in this object, across all
    #'   constituent slices. This is useful for setting scale limits that are shared across panels
    #' @param slice_indices an optional integer vector of slice indices to be used as a subset in the calculation
    #' @return a tibble keyed by 'layer' with overall low and high values, as well as split by pos/neg
    get_ranges = function(slice_indices = NULL) {
      img_all <- private$get_combined_data(slice_indices)

      img_ranges <- img_all %>%
        dplyr::group_by(layer) %>%
        dplyr::summarize(low = min(value, na.rm = TRUE), high = max(value, na.rm = TRUE), .groups = "drop")

      # for bisided layers, we need pos and neg ranges -- N.B. this does not support arbitrary cutpoints!
      img_ranges_posneg <- img_all %>%
        dplyr::filter(value > 2 * .Machine$double.eps | value < -2 * .Machine$double.eps) %>% # filter exact zeros so that we get true > and <
        dplyr::mutate(above_zero = factor(value > 0, levels = c(TRUE, FALSE), labels = c("pos", "neg"))) %>%
        dplyr::group_by(layer, above_zero, .drop=FALSE) %>%
        dplyr::summarize(
          low = suppressWarnings(min(value, na.rm = TRUE)),
          high = suppressWarnings(max(value, na.rm = TRUE)), .groups = "drop") %>%
        pivot_wider(id_cols="layer", names_from="above_zero", values_from=c(low, high))

      # join the overall ranges with the pos/neg split
      img_ranges <- img_ranges %>%
        dplyr::full_join(img_ranges_posneg, by = "layer") %>%
        dplyr::mutate(across(matches("low|high"), ~ if_else(is.infinite(.x), NA_real_, .x))) # set Inf to NA

      return(img_ranges)

    },

    #' @description returns a data.frame with the unique values for each label layer, across all
    #'   constituent slices
    #' @param slice_indices an optional integer vector of slice indices to be used as a subset in the calculation
    get_uvals = function(slice_indices = NULL) {
      # examine first slice to see if any layers have labels (reasonably assumes all slices have same layers)
      has_labels <- sapply(private$pvt_slice_data[[1]], function(x) "label" %in% names(x))
      if (!any(has_labels)) {
        return(data.frame()) # return empty data.frame
      } else {
        img_all <- private$get_combined_data(slice_indices, only_labeled = TRUE)
        img_uvals <- img_all %>%
          group_by(layer) %>%
          dplyr::summarize(uvals = unique(label), .groups = "drop") %>%
          na.omit()
      }

      return(img_uvals)

    }
  )
)
