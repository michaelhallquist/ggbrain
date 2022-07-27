#' @title R6 class for compiling images to render in ggplot
#' @importFrom RNifti voxelToWorld readNifti niftiHeader
#' @importFrom rlang flatten
#' @importFrom dplyr bind_rows group_by group_split distinct mutate select n anti_join
#' @importFrom checkmate assert_character assert_file_exists assert_logical assert_subset
#' @importFrom reshape2 melt
#' @importFrom tidyr unnest
#' @importFrom tibble remove_rownames
#' @importFrom tidyselect everything
#'
#' @export
ggbrain_images <- R6::R6Class(
  classname = "ggbrain_images",
  private = list(
    pvt_imgs = list(), # image data
    pvt_img_labels = list(), # list of data.frames containing labels for a label image
    pvt_img_names = NULL, # names of images
    pvt_dims = NULL, # x, y, z extent
    pvt_zero_tol = 1e-6, # threshold for what constitutes a non-zero voxel
    pvt_nz_range = NULL, # the range of slices in x, y, and z that contain non-zero voxels
    pvt_slices = NULL, # allows caching of slices for + approach
    pvt_contrasts = NULL, # allows caching of contrasts for + approach
    pvt_fill_holes = NULL, # vector of settings for each img indicating whether to run slices through the imager::fill() function
    pvt_clean_specks = NULL, # vector of settings for each img indicating whether to run slices through the imager::clean() function

    # TODO: make this not extrapolate beyond original
    # Best approach: use flood fill from boundary of image to find pixels that cannot be reached when filling the background.

    fill_img_holes = function(img, size=NULL) {
      browser()
      slc_cimg <- as.cimg(img)
      orig_px <- as.pixset(slc_cimg) # pixels before interp
      f <- fill(slc_cimg, 5, boundary=FALSE)
      f <- fill(slc_cimg, 5, boundary = TRUE)
      to_fill <- as.pixset(f - orig_px)
      filled <- slc_cimg
      filled[to_fill] <- NA
      filled2 <- inpaint(filled, 2)
      plot(slc_cimg)
      plot(filled)
            plot(filled2)


      locations <- which(to_fill, arr.ind = TRUE) %>%
        as.data.frame() %>%
        setNames(c("x", "y", "z", "c"))
      x <- slc_cimg
      x[as.matrix(locations)] <- NA
      filled <- interp(x, locations)


    },

    determine_fill_clean = function(val, img_names = NULL) {
      if (is.null(val)) {
        val <- rep(0L, length(img_names)) %>% setNames(img_names) # all zeros
      } else {
        if (length(val) > 1L && length(val) != length(img_names)) {
          stop("Length of fill_holes/clean_specks doesn't match length of images")
        }

        # if user passes in TRUE/FALSE, then use default numeric hole size of 2 (2x2 holes)
        if (checkmate::test_logical(val)) {
          if (length(val) == 1L) val <- rep(val, length(img_names)) # replicate T/F for all images
          val <- ifelse(val == TRUE, 2, 0)
        } else if (checkmate::test_integerish(val)) {
          val <- as.integer(val)
          checkmate::assert_integer(val, lower = 0L)
        } else {
          stop("Cannot interpret fill_holes/clean_specks")
        }

        names(val) <- img_names
      }
      return(val)
    },

    set_images = function(images = NULL, fill_holes = NULL, clean_specks = NULL) {
      if (is.null(images)) return(NULL) # skip out

      if (checkmate::test_character(images)) {
        checkmate::assert_file_exists(images)
        if (is.null(names(images))) {
          warning(
            "The images vector does not contain any names. ",
            "This may lead to weird behaviors downstream if 'underlay' and 'overlay' are requested."
          )
          names(images) <- make.unique(basename(images))
        } else if (any(names(images) == "")) {
          which_empty <- names(images) == ""
          names(images)[which_empty] <- make.unique(basename(images[which_empty]))
        }

        img_list <- sapply(images, function(ff) {
          img <- RNifti::readNifti(ff)

          # round very small values to zero
          if (!is.null(private$pvt_zero_tol) && private$pvt_zero_tol > 0) {
            img[img > -1 * private$pvt_zero_tol & img < private$pvt_zero_tol] <- 0
          }

          return(img)
        }, simplify = FALSE)
      } else if (checkmate::test_list(images)) {
        checkmate::assert_named(images, type = "unique") # unique names
        sapply(images, function(x) checkmate::assert_class(x, "niftiImage") ) # enforce RNifti objects
        img_list <- images
      }

      # determine how to handle specks and holes for these images
      fill_holes <- determine_fill_clean(fill_holes, names(img_list))
      clean_specks <- determine_fill_clean(clean_specks, names(img_list))

      img_dims <- cbind(sapply(img_list, dim), extant=private$pvt_dims) # xyz x images matrix augmented by stored dims
      dim_match <- apply(img_dims, 1, function(row) {
        length(unique(row)) == 1L
      })

      if (!all(dim_match)) {
        print(img_dims)
        stop("Image dimensions do not match one another")
      } else {
        private$pvt_dims <- img_dims[, 1]
      }

      private$pvt_imgs[names(img_list)] <- img_list
      private$pvt_fill_holes[names(fill_holes)] <- fill_holes
      private$pvt_clean_specks[names(clean_specks)] <- clean_specks
      private$pvt_img_names <- names(private$pvt_imgs)
      private$pvt_nz_range <- self$get_nz_indices()
    }
  ),
  active = list(
    #' @field zero_tol the (positive) numeric value that should be treated as indistinguishable from zero.
    #'   This value is used to set small values in the images to exactly zero for proper masking. Default 1e-6
    zero_tol = function(value) {
      if (missing(value)) {
        private$pvt_zero_tol
      } else {
        checkmate::assert_number(value, lower=0) # force positive number
        private$pvt_zero_tol <- value
      }
    },
    #' @field slices a character vector of cached slice specifications to be used in $get_slices()
    slices = function(value) {
      if (missing(value)) {
        private$pvt_slices
      } else {
        checkmate::assert_character(value) # probably need better validation...
        private$pvt_slices <- value
      }
    },
    #' @field contrasts a character vector of cached contrast specifications to be used in $get_slices()
    contrasts = function(value) {
      if (missing(value)) {
        private$pvt_contrasts
      } else {
        checkmate::assert_character(value) # probably need better validation...
        private$pvt_contrasts <- value
      }
    },
    clean_specks = function(value) {
      if (missing(value)) {
        private$pvt_clean_specks
      } else {
        private$pvt_clean_specks <- determine_fill_clean(value, private$pvt_img_names)
      }
    },
    fill_holes = function(value) {
      if (missing(value)) {
        private$pvt_fill_holes
      } else {
        private$pvt_fill_holes <- determine_fill_clean(value, private$pvt_img_names)
      }
    }
  ),
  public = list(
    #' @description create ggbrain_images object consisting of one or more NIfTI images
    #' @param images a character vector of file names containing NIfTI images to read
    initialize = function(images = NULL, fill_holes = NULL, clean_specks = NULL, labels=NULL) {
      private$set_images(images, fill_holes, clean_specks)
      if (!is.null(labels)) {
        # if user provides a data.frame as label input, this works in the case of a single image, which is assumed to correspond
        if (checkmate::test_data_frame(labels) && length(images) == 1L) {
          labels <- list(labels) %>% setNames(self$get_image_names())
        }
        checkmate::assert_list(labels, names = "unique")
        do.call(self$add_labels, labels)
      }
    },

    #' @description method to add another ggbrain_images object to this one
    #' @param obj the ggbrain_images object to combine with this one
    add = function(obj) {
      checkmate::assert_class(obj, "ggbrain_images")
      # nothing to add
      if (is.null(obj$get_image_names())) {
        return(self)
      } else if (!is.null(self$get_image_names()) && !identical(obj$dim(), self$dim())) {
        stop(glue::glue("Dimensions of existing object ({paste(self$dim(), collapse=',')})",
                  "do not match object to add ({paste(obj$dim(), collapse=',')})"))
      }

      if (!identical(obj$zero_tol, self$zero_tol)) {
        new_tol <- min(obj$zero_tol, self$zero_tol)
        message(glue("Using lesser of zero tolerances ({new_tol}) in combined object"))
        self$zero_tol <- new_tol
      }

      # add any slice specifications from other object
      self$add_slices(obj$slices)

      # get image list (list of Niftis) of object to be added
      self$add_images(obj$get_images(drop=FALSE), obj$fill_holes, obj$clean_specks)

      # use do.call to build a named ... list of arguments
      do.call(self$add_labels, obj$get_labels())

      # pvt_img_labels = list(), # list of data.frames containing labels for a label image
      return(self)
    },

    #' @description add a labels data.frame that connects an integer-valued image with a set of labels
    #' @param ... named arguments containing data.frame objects for each image to be labeled. The argument name should
    #'   match the image name to be labeled and the value should be a data.frame containing \code{value} and \code{label}. 
    #' @details
    #'
    #'   As a result of $add_labels, the $get_slices method will always remap the numeric values for label images to the corresponding
    #'   text-based labels in the label data. In addition, a new attribute will be returned called "slice_labels" that contains
    #'   a row for each region represented in each slice.
    add_labels = function(...) {
      label_args <- list(...)

      # return unchanged object if no input labels found
      if (is.null(label_args) || length(label_args) == 0L) return(self)
      label_names <- names(label_args)
      if (is.null(label_names) || any(label_names == "")) {
        stop("All arguments must be named, with the name referring to the image to be labeled.")
      }

      # all label arguments must match an image name
      checkmate::assert_subset(label_names, private$pvt_img_names)
      sapply(label_args, function(x) checkmate::assert_data_frame(x) )
      sapply(label_args, function(x) checkmate::assert_subset("value", names(x)))

      for (x in seq_along(label_args)) {
        cur_vals <- private$pvt_img_labels[[ label_names[x] ]]
        if (!is.null(cur_vals)) {
          message(glue("Image {label_names[x]} has labels, which will replaced"))
        }

        # encode label columns for each input data.frame -- only character and factor/ordered allowed
        col_classes <- sapply(label_args[[x]], function(v) inherits(v, c("character", "ordered", "factor")))
        attr(label_args[[x]], "label_columns") <- names(label_args[[x]][col_classes])

        private$pvt_img_labels[[ label_names[x] ]] <- label_args[[x]]
      }

      return(self)
    },

    #' @description add one or more images to this ggbrain_images object
    #' @param images a character vector of file names containing NIfTI images to read 
    add_images = function(images = NULL, fill_holes = NULL, clean_specks = NULL) {
      private$set_images(images, fill_holes, clean_specks)
      return(self)
    },

    #' @description return the 3D dimensions of the images contained in this object
    dim = function() {
      private$pvt_dims
    },

    #' @description return the names of the images contained in this object
    get_image_names = function() {
      private$pvt_img_names
    },

    #' @description return the RNifti objects of one or more images contained in this object
    #' @param img_names The names of images to return. Use \code{$get_image_names()} if you're uncertain
    #'   about what is available.
    #' @param drop If TRUE, a single image is returned as an RNifti object, rather than a single-element list 
    #'   containing that object.
    get_images = function(img_names = NULL, drop = TRUE) {
      checkmate::assert_logical(drop, len=1L)
      if (is.null(img_names)) {
        ret <- private$pvt_imgs
      } else {
        checkmate::assert_subset(img_names, private$pvt_img_names)
        ret <- private$pvt_imgs[img_names]
      }

      if (length(ret) == 1L && isTRUE(drop)) {
        ret <- ret[[1L]] # unlist
      }

      return(ret)
    },

    #' @description return the NIfTI headers for one or more images contained in this object
    #' @param img_names The names of images whose header are returned. Use \code{$get_image_names()} if you're uncertain
    #'   about what is available.
    #' @param drop If TRUE, a single header is returned as an niftiHeader object, rather than a single-element list 
    #'   containing that object.
    get_headers = function(img_names = NULL, drop = TRUE) {
      checkmate::assert_logical(drop, len=1L)
      if (is.null(img_names)) {
        ret <- private$pvt_imgs
      } else {
        checkmate::assert_subset(img_names, private$pvt_img_names)
        ret <- private$pvt_imgs[img_names]
      }

      ret <- sapply(ret, niftiHeader, simplify=FALSE)

      if (length(ret) == 1L && isTRUE(drop)) {
        ret <- ret[[1L]] # unlist
      }

      return(ret)
    },

    #' @description method for removing one or more images from the ggbrain_images object
    #' @param img_names 
    remove_images = function(img_names) {
      checkmate::assert_character(img_names)
      good_imgs <- intersect(private$pvt_img_names, img_names)
      bad_imgs <- setdiff(img_names, private$pvt_img_names)

      if (length(good_imgs) > 0L) {
        message(glue("Removing images: {paste(good_imgs, collapse=', ')}"))
        private$pvt_imgs[good_imgs] <- NULL
      }

      if (length(bad_imgs) > 0L) {
        warning(glue("Could not find these images to remove: {paste(bad_imgs, collapse=', ')}"))
      }

    },

    #' @description winsorize the tails of a set of images to pull in extreme values
    #' @param img_names The names of images in the ggbrain_images object to be winsorized
    #' @param quantiles The lower and upper quantiles used to define the thresholds for winsorizing.
    winsorize_images = function(img_names, quantiles = c(.001, .999)) {
      checkmate::assert_numeric(quantiles, lower = 0, upper = 1, len = 2)
      stopifnot(quantiles[1] < quantiles[2])
      checkmate::assert_character(img_names)
      checkmate::assert_subset(img_names, private$pvt_img_names)
      private$pvt_imgs[img_names] <- lapply(private$pvt_imgs[img_names], function(img) {
        if (quantiles[1] > 0) {
          lthresh <- quantile(img[img > 0], quantiles[1])
          img[img < lthresh & img > 0] <- lthresh
        }

        if (quantiles[2] < 1) {
          uthresh <- quantile(img[img > 0], quantiles[2])
          img[img > uthresh] <- uthresh
        }

        return(img)
      })
      return(self)
    },

    #' @description method to set values less than \code{threshold} to NA
    #' @param img_names The names of images in the ggbrain_images object whose values should be set to NA
    #' @param threshold The threshold value whose absolute value used to determine which voxels to set to NA.
    #'   If \code{NULL}, use the pvt_zero_tol field (default 1e-6).
    na_images = function(img_names, threshold = NULL) {
      if (is.null(threshold)) {
        threshold <- private$pvt_zero_tol
      }

      private$pvt_imgs[img_names] <- lapply(private$pvt_imgs[img_names], function(img) {
        img[abs(img) < threshold] <- NA
        return(img)
      })

    },

    #' @description print a summary of the ggbrain_images object
    summary = function() {
      cat("\nImage dimensions:\n")
      print(private$pvt_dims)
      cat("\nImages in object:\n")
      print(private$pvt_imgs)
    },

    #' @description return the indices of non-zero voxels
    #' @param img_names The names of images in the ggbrain_images object whose non-zero indices should be looked up
    #' @details Note that this function looks for non-zero voxels in any of the images specified by \code{img_names}.
    get_nz_indices = function(img_names = NULL) {
      if (is.null(img_names)) {
        if (!is.null(private$pvt_nz_range)) {
          return(private$pvt_nz_range) # return pre-cached dims (reflects all images), if available
        } else {
          img_names <- private$pvt_img_names
        }
      } else {
        checkmate::assert_subset(img_names, private$pvt_img_names)
      }

      # find voxels in each image that are different from zero
      img_nz <- lapply(private$pvt_imgs[img_names], function(img) {
        abs(img) > private$pvt_zero_tol
      })

      # elementwise logical and of nz voxels in images list
      # img_all <- Reduce("&", img_nz)
      # sum(img_all)

      img_any <- Reduce("|", img_nz)

      nz_pos <- which(img_any == TRUE, arr.ind = TRUE)

      # get indices in i, j, k that are non-zero across the images of interest
      lapply(1:3, function(j) {
        range(nz_pos[, j])
      }) %>% setNames(c("i", "j", "k"))
    },

    #' @description adds one or more slices to the cached slices that will be retrieved by
    #'   $get_slices() when no \code{slices} argument is passed.
    #' @param slices a character vector containing one or more slices to be extracted by \code{$get_slices}.
    #'   Uses the syntax `"<xyz>=<number>"`. Example: `c("x=10", "y=50%")`
    add_slices = function(slices = NULL) {
      if (!is.null(slices)) {
        checkmate::assert_character(slices)
        private$pvt_slices <- c(private$pvt_slices, slices)
      }
      return(self)
    },

    #' @description adds one or more contrasts to the cached contrasts that will be retrieved by
    #'   $get_slices() when no \code{contrasts} argument is passed.
    #' @param contrasts a character vector containing one or more contrasts to be extracted by \code{$get_slices}.
    #'   Uses the syntax `"<img_name>[subset_expression] + <img_name>"`.
    add_contrasts = function(contrasts = NULL) {
      if (!is.null(contrasts)) {
        checkmate::assert_character(contrasts)
        private$pvt_contrasts <- c(private$pvt_contrasts, contrasts)
      }
      return(self)
    },

    #' @description remove all cached slice settings
    reset_slices = function() {
      private$pvt_slices <- NULL
      return(self)
    },

    #' @description get slice data for one or more slices based on their coordinates
    #' @param slices a vector of slice positions
    #' @param img_names a character vector of images contained in the ggbrain_images object to be sliced
    #' @param contrasts a named character vector of contrasts to be calculated for each slice
    #' @param make_square If TRUE, make all images square and of the same size
    #' @param remove_null_space If TRUE, remove slices where all values are approximately zero
    #' @param fill_holes An integer. If > 0, slice data will be passed through a hole-filling algorithm
    #'   that dilates the mask of slice data by this amount, then shrinks back to the original size. For
    #'   example if `fill_holes = 3` then any hole that is 3x3 or larger (along the slice extents)
    #'   will be filled in.
    #' @param clean_specks An integer. If > 0L, any object greater than this size will be removed from the slice
    #'   for visualization (since it would be hard to see.) For example, if `clean_specks = 3`, then
    #'   any object 3x3 or larger (along the slice extents) will be removed.
    #' @details This function always returns a data.frame where each row represents a slice requested
    #'   by the user. The $slice_data element is a list-column where each element is itself a list
    #'   of slice data for a given layer/image (e.g., underlay or overlay) . The $slice_matrix
    #'   is a list-column where each element is a list of 2-D matrices, one per layer/image.
    #'  @return a ggbrain_slices object containing the requested slices and contrasts
    get_slices = function(slices = NULL, img_names = NULL, contrasts = NULL, fill_labels = FALSE,
      make_square = TRUE, remove_null_space = TRUE, fill_holes = NULL, clean_specks = NULL) {

      if (is.null(slices)) {
        if (!is.null(private$pvt_slices)) {
          slices <- private$pvt_slices # use cached slice settings
        } else {
          stop("No slices have been provided and none are in the $slices field. Cannot determine what to extract.")
        }
      }

      if (is.null(contrasts) && !is.null(private$pvt_contrasts)) {
          contrasts <- private$pvt_contrasts # use cached contrast settings
      }

      slice_df <- self$lookup_slices(slices) # defaults to ignoring null space
      all_img_names <- self$get_image_names()
      if (!is.null(img_names)) {
        checkmate::assert_subset(img_names, all_img_names)
      } else {
        img_names <- all_img_names # all in the set
      }
      checkmate::assert_character(contrasts, names="unique", null.ok = TRUE)
      checkmate::assert_logical(make_square, len=1L)
      checkmate::assert_logical(remove_null_space, len = 1L)

      coords <- slice_df %>%
        group_by(slice_index) %>%
        group_split()

      # populate fill and clean from parent object
      if (is.null(fill_holes)) fill_holes <- self$fill_holes[img_names]
      if (is.null(clean_specks)) clean_specks <- self$clean_specks[img_names]

      slc <- lapply(coords, function(slc) {
        self$get_slices_inplane(img_names, slc$slice_number, slc$plane, drop = TRUE)
      })

      # WIP
      # if (any(fill_holes > 0L)) {
      #   which_fill <- which(fill_holes > 0L)
      #   slc <- lapply(slc, function(ss) {
      #     ss[which_fill] <- lapply(which_fill, function(i) {
      #       private$fill_img_holes(ss[[i]], fill_holes[i])
      #     })

      #   })
      # }

      # remove blank space from matrices if requested (this must come before making the slices square)
      if (isTRUE(remove_null_space)) {
        # find voxels in each image that are different from zero
        slc <- lapply(slc, function(ilist) {
          img_nz <- lapply(rlang::flatten(ilist), function(img) {
            abs(img) > private$pvt_zero_tol
          })

          img_any <- Reduce("|", img_nz)
          good_rows <- rowSums(img_any, na.rm = TRUE) > 0L
          good_cols <- colSums(img_any, na.rm = TRUE) > 0L

          lapply(ilist, function(mat) {
            mat[good_rows, good_cols]
          })
        })
      }

      # whether to make all images have the same square dimensions
      if (isTRUE(make_square)) {
        slc_dims <- sapply(rlang::flatten(slc), dim)
        square_dims <- apply(slc_dims, 1, max)
        # for each slice and image within slice, center the matrix in the target output dims
        slc <- lapply(slc, function(ilist) {
          lapply(ilist, function(mat) {
            center_matrix(square_dims, mat, drop_zeros = FALSE) # at present, drop_zeros = TRUE will lead to offsets across images...
          })
        })
      }

      # look up labels for each slice
      if (any(img_names %in% names(private$pvt_img_labels))) {
        # which images contain integer-valued data that should be labeled?
        label_imgs <- img_names[img_names %in% names(private$pvt_img_labels)]

        # compute CoM statistics for label images based on unique numeric values
        label_slc <- lapply(slc, "[", label_imgs)
        com_stats <- lapply(seq_along(label_slc), function(dd) {
          lapply(label_slc[[dd]], function(xx) {
            uvals <- unique(as.vector(xx))
            uvals <- uvals[!uvals %in% c(NA, 0)]
            if (length(uvals) == 0L) return(NULL) # no matching positions on this slice
            sapply(uvals, function(u) { colMeans(which(xx == u, arr.ind=TRUE)) }) %>%
              t() %>% data.frame() %>% setNames(c("dim1", "dim2")) %>% 
              dplyr::bind_cols(value = uvals, slice_index = dd) %>% dplyr::arrange(uvals)
          })
        })
      } else {
        com_stats <- NULL
        label_imgs <- NULL
      }

      # create a list of image data.frames for each slice
      slc_nestlist <- lapply(slc, function(dd) {
        # each element of dd is a square matrix for a given image
        sapply(names(dd), function(lname) {
          df <- reshape2::melt(dd[[lname]], varnames = c("dim1", "dim2"))
          df$image <- lname
          return(df)
        }, USE.NAMES = TRUE, simplify = FALSE)
      })

      # generate a labeled copy of the data using the number -> label conversion
      if (!is.null(label_imgs)) {
        for (ii in seq_along(slc_nestlist)) {
          this_slc <- slc_nestlist[[ii]]
          which_lab <- intersect(names(this_slc), label_imgs)

          for (label_name in which_lab) {
            this_img <- this_slc[[label_name]]
            this_com <- com_stats[[ii]][[label_name]]

            # always set 0 to NA in labeled image
            this_img$value[this_img$value == 0] <- NA

            # unique values represented in this image
            all_vals <- sort(unique(this_img$value)) # note that sort drops NA by default

            # get labels data.frame
            lb <- private$pvt_img_labels[[label_name]]

            # which columns in the data.frame are labels
            l_cols <- attr(lb, "label_columns")

            # in the fill_labels == TRUE case, fill in labels that are present in the label data.frame, but
            # add a default label (the value) for any values in the image that lack a label
            if (isTRUE(fill_labels)) {
              # fill in missing labels, keeping the numeric values in string form
              all_df <- data.frame(value = all_vals)
              all_labs <- as.character(all_vals)
              all_df <- data.frame(value = all_vals, label = all_labs)

              # replace numeric value column with labeled character column
              for (ll in l_cols) {
                # keep the non-matching rows from all_df as defaults, then bind the hand-labeled areas
                lb_ll <- lb %>%
                  select(all_of(c("value", ll))) %>%
                  dplyr::rename(label = !!ll)

                comb_df <- all_df %>%
                  dplyr::anti_join(lb_ll, by = "value") %>%
                  dplyr::bind_rows(lb_ll) %>%
                  dplyr::arrange(value)

                this_img <- this_img %>%
                  dplyr::mutate(!!ll := comb_df$label[match(value, comb_df$value)])
              }

              this_img <- this_img %>%
                dplyr::select(dim1, dim2, value, image, everything())
            } else {
              this_img <- this_img %>% left_join(lb, by = "value") # this will have NA labels for any values that lack a match in lb
            }

            # also label CoM data.frame
            if (!is.null(this_com)) this_com <- this_com %>% left_join(lb, by="value")

            attr(this_img, "label_cols") <- l_cols

            slc_nestlist[[ii]][[label_name]] <- this_img
            com_stats[[ii]][[label_name]] <- this_com # update com stats
          }
        }
      }

      # can use unnest_longer to get a slices and images/layers on the rows
      slice_df$slice_data <- slc_nestlist
      #xx <- slice_df %>% tidyr::unnest_longer(slice_data)

      # always keep slices as a list of 2D matrices (one per layer/image)
      slice_df$slice_matrix <- slc
      slice_df$slice_labels <- com_stats

      slice_obj <- ggbrain_slices$new(slice_df)
      if (!is.null(contrasts)) { # compute contrasts, if requested
        slice_obj$compute_contrasts(contrasts)
      }

      return(slice_obj)
    },

    #' @description get_slices_inplane is mostly an internal funciton for getting one or more slices from a given plane
    #' @param imgs The names of images to slice
    #' @param slice_numbers The numbers of slices in the specified plant to grab
    #' @param plane The image plane to slice. Must be "coronal", "sagittal", or "axial"
    #' @param drop if TRUE, a single slice is returned as a 2D matrix instead of a 3D matrix with a singleton first dimension
    #' @return A 3D matrix of slices x dim1 x dim2
    get_slices_inplane = function(imgs = NULL, slice_numbers, plane, drop=FALSE) {
      if (is.null(imgs)) {
        imgs <- private$pvt_img_names
      } else if (!checkmate::test_subset(imgs, private$pvt_img_names)) {
        stop(glue("The img input to $get_slice() must be one of: {paste(private$pvt_img_names, collapse=', ')}"))
      }

      checkmate::assert_integerish(slice_numbers, lower = 1)
      checkmate::assert_subset(plane, c("sagittal", "coronal", "axial"))
      #return named list of slices for the images requested
      sapply(imgs, function(iname) {
        dat <- private$pvt_imgs[[iname]]
        if (plane == "sagittal") {
          slc_mat <- aperm(dat[slice_numbers, , , drop = FALSE], c(1, 2, 3))
        } else if (plane == "coronal") {
          slc_mat <- aperm(dat[, slice_numbers, , drop = FALSE], c(2, 1, 3))
        } else if (plane == "axial") {
          slc_mat <- aperm(dat[, , slice_numbers, drop = FALSE], c(3, 1, 2))
        }

        attr(slc_mat, "slice_numbers") <- slice_numbers
        attr(slc_mat, "plane") <- plane

        if (isTRUE(drop)) slc_mat <- drop(slc_mat)
        return(slc_mat)
      }, simplify = FALSE)
    },
    
    #' @description return a list of data.frames containing labels for a given image
    #' @details the names of the list correspond directly with the names of the images
    get_labels = function() {
      return(private$pvt_img_labels)
    },
    
    #' @description internal function to lookup which slices to display along each axis based on their quantile,
    #'   xyz coordinate, or ijk coordinate
    #' @param slices A character vector of coordinates for slices to display
    #' @param ignore_null_space If TRUE, any coordinates specified as quantiles (e.g., x = 50%)
    #'   use the quantiles of only the non-zero slices (ignoring blank sliaces)
    lookup_slices = function(slices, ignore_null_space = TRUE) {
      checkmate::assert_character(slices)
      img_dims <- self$dim()
      
      slc_range_full <- list(i = seq_len(img_dims[1]), j = seq_len(img_dims[2]), k = seq_len(img_dims[2]))
      
      if (isTRUE(ignore_null_space)) {
        slc_range <- self$get_nz_indices()
      } else {
        slc_range <- slc_range_full
      }
      
      # get nifti header for first image for voxel -> world transformations
      nii_head <- self$get_headers(drop=FALSE)[[1L]]
      
      # translate ijk to xyz for each axis
      xcoords <- RNifti::voxelToWorld(cbind(slc_range_full$i, 1, 1), nii_head)[, 1]
      ycoords <- RNifti::voxelToWorld(cbind(1, slc_range_full$j, 1), nii_head)[, 2]
      zcoords <- RNifti::voxelToWorld(cbind(1, 1, slc_range_full$k), nii_head)[, 3]
      
      # helper subfunction to lookup slice number, plane, and label for any ijk, xyz, or % input
      get_slice_num <- function(coord_str) {
        res <- tolower(trimws(strsplit(coord_str, "\\s*=\\s*", perl = TRUE)[[1]]))
        axis <- res[1]
        number <- res[2]
        is_pct <- grepl("[\\d.]+%", number, perl = TRUE)
        if (isTRUE(is_pct)) {
          number <- as.numeric(sub("%", "", number, fixed=TRUE))
          checkmate::assert_number(number, lower = 0, upper = 100)
          number <- number/100 # convert to quantile
        } else {
          number <- as.numeric(number)
        }
        
        # determine plane of slice to display
        plane <- switch(
          axis,
          i = "sagittal",
          j = "coronal",
          k = "axial",
          x = "sagittal",
          y = "coronal",
          z = "axial",
          stop(sprintf("Cannot interpret input: %s", coord_str))
        )
        
        # determine world or voxel coordinate system
        coord <- switch(
          axis,
          i = "voxel", j = "voxel", k = "voxel",
          x = "world", y = "world", z = "world",
          stop(sprintf("Cannot interpret input: %s", coord_str))
        )
        
        axis_label <- switch(plane, sagittal = "x", coronal = "y", axial = "z")
        
        # validate input and lookup slice
        if (isTRUE(is_pct)) {
          rr <- switch(plane, sagittal = slc_range$i, coronal = slc_range$j, axial = slc_range$k)
          slc_num <- round(quantile(rr, number))
        } else {
          if (coord == "world") { # xyz
            coords <- switch(plane, sagittal = xcoords, coronal = ycoords, axial = zcoords)
            checkmate::assert_number(number, lower=min(coords), upper=max(coords))
            slc_num <- which.min(abs(number - coords))
          } else if (coord == "voxel") { #ijk
            coords <- switch(plane, sagittal = slc_range_full$i, coronal = slc_range_full$j, axial = lc_range_full$k)
            checkmate::assert_integerish(number, lower=min(coords), upper=max(coords), len=1L)
            slc_num <- number
          }
        }
        
        # slc_num is the slice number in the plane of interest
        if (axis_label == "x") {
          slc_coords <- xcoords[slc_num]
        } else if (axis_label == "y") {
          slc_coords <- ycoords[slc_num]
        } else if (axis_label == "z") {
          slc_coords <- zcoords[slc_num]
        }
        
        slc_coords <- round(slc_coords, 1) # for display
        df <- data.frame(coord_label = paste(axis_label, "=", slc_coords), plane = plane, slice_number = slc_num)
        
        return(df)
      }
      
      slice_df <- lapply(slices, get_slice_num) %>%
        bind_rows() %>%
        distinct() %>% # remove any dupes
        tibble::remove_rownames() %>% # unneeded labels
        mutate(slice_index = 1:n(), coord_input = slices) %>%
        select(slice_index, coord_input, coord_label, everything())
      
      return(slice_df)
    }
  )

)

summary.ggbrain_images <- function(gg, args) {
  gg$summary()
}


#'addition operator for combining ggbrain_images objects
#' @param o1 first ggbrain_images object
#' @param o2 second ggbrain_images object
#' @return combined ggbrain_images object
#' @details note that the addition does not modify either existing object. Rather,
#'   the first object is cloned and the second is added to it. If you want to add one
#'   ggbrain_images object to another in place (i.e., modifying the extant object), use
#'   the $add() method.
#' @export
`+.ggbrain_images` <- function(o1, o2) {
  if (!identical(o1$dim(), o2$dim())) {
    stop("ggbrain_images objects must have the same dimensions to be added together")
  }
  
  # always work from copy
  oc <- o1$clone(deep = TRUE)
  
  # add objects using add method
  oc$add(o2)
}


# testing
# test <- data.frame(value=100, label="hello")
# 
# i1 <- ggbrain_images$new(images=c(underlay = "template_brain.nii.gz"))
# i1$add_slices("x=10")
# i1$add_labels(underlay=test)
# 
# i2 <- ggbrain_images$new(images=c(atlas = "template_brain.nii.gz"))
# i2$add_slices("y=10")
# i2$add_labels(atlas=test)
# 
# ic <- i1+i2
# ic$slices
# ic$get_labels()
# ic$get_images()
# ic$get_nz_indices()

#i1$add(i2) # add by reference