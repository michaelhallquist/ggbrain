#' R6 class for compiling images to render in ggplot
#' @importFrom RNifti voxelToWorld readNifti
#' @importFrom rlang flatten
#' @export
ggbrain_images <- R6::R6Class(
  classname = "ggbrain_images",
  private = list(
    pvt_imgs = list(), # image data
    pvt_img_names = NULL, # names of images
    pvt_dims = NULL, # x, y, z extent
    pvt_zero_tol = 1e-6, # threshold for what constitutes a non-zero voxel
    pvt_nz_range = NULL, # the range of slices in x, y, and z that contain non-zero voxels
    set_images = function(images) {
      checkmate::assert_character(images)
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

      if (!"underlay" %in% c(private$pvt_img_names, names(images))) {
        warning("'underlay' is not among the images provided. This may lead to weirdness downstream.")
      }

      img_list <- sapply(images, function(ff) {
        img <- RNifti::readNifti(ff)

        # round very small values to zero
        if (!is.null(private$pvt_zero_tol) && private$pvt_zero_tol > 0) {
          img[img > -1 * private$pvt_zero_tol & img < private$pvt_zero_tol] <- 0
        }

        return(img)
      }, simplify = FALSE)

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
      private$pvt_img_names <- names(private$pvt_imgs)
      private$pvt_nz_range <- self$get_nz_indices()
    }
  ),
  public = list(
    #' @description create ggbrain_images object consisting of one or more NIfTI images
    #' @param images a character vector of file names containing NIfTI images to read 
    initialize = function(images) {
      private$set_images(images)
    },
    
    #' @description add one or more images to this ggbrain_images object
    #' @param images a character vector of file names containing NIfTI images to read 
    add_images = function(images) {
      private$set_images(images)
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
    #'   Or in more technical terms, 
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

    #' @description get slice data for one or more slices based on their coordinates
    #' @param slices a vector of slice positions
    #' @param img_names a character vector of images contained in the ggbrain_images object to be sliced
    #' @param make_square If TRUE, make all images square and of the same size
    #' @param remove_null_space If TRUE, remove slices where all values are approximately zero
    #' @param as_data_frame If TRUE, return slices as a data.frame indexed by slice, image layer, and the image matrix
    get_slices = function(slices, img_names = NULL, make_square = TRUE, remove_null_space = TRUE, as_data_frame=TRUE) {
      slice_df <- self$lookup_slices(slices) # defaults to ignoring null space
      coords <- slice_df %>%
        group_by(slice_index) %>%
        group_split()

      slc <- lapply(coords, function(slc) {
        self$get_slices_inplane(img_names, slc$slice_number, slc$plane, drop = TRUE)
      })

      if (isTRUE(make_square)) {
        slc_dims <- sapply(flatten(slc), dim)
        square_dims <- apply(slc_dims, 1, max)
        # for each slice and image within slice, center the matrix in the target output dims
        slc <- lapply(slc, function(ilist) {
          lapply(ilist, function(mat) {
            center_matrix(square_dims, mat)
          })
        })
      }

      # remove blank space from matrices if requested
      # this approach 
      if (isTRUE(remove_null_space)) {
        # find voxels in each image that are different from zero
        img_nz <- lapply(rlang::flatten(slc), function(img) {
          abs(img) > private$pvt_zero_tol
        })

        img_any <- Reduce("|", img_nz)
        good_rows <- rowSums(img_any, na.rm = T) > 0L
        good_cols <- colSums(img_any, na.rm = T) > 0L

        # subset all images to the non-zero slices
        slc <- lapply(slc, function(ilist) {
          lapply(ilist, function(mat) {
            mat[good_rows, good_cols]
          })
        })
      }

      if (isTRUE(as_data_frame)) {
        slc <- lapply(slc, function(dd) {
          # each element of dd is a sqaure matrix for a given image
          lapply(names(dd), function(lname) {
            df <- reshape2::melt(dd[[lname]], varnames = c("dim1", "dim2"))
            df$image <- lname
            return(df)
          }) %>% bind_rows()
        })

        slice_df$slice_data <- slc
        slice_df <- slice_df %>% unnest(slice_data)
      } else {
        # add list column for slice data
        slice_df$slice_data <- slc
      }

      return(slice_df)

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

    }
  )

)


ggbrain_r6 <- R6::R6Class(
  classname = "ggbrain",
  private = list(
    layer_imgs = list(), # keep original data?
    pvt_panels = list(),
    composite_plot = NULL,
    set_panels = function(panels) {
      if (checkmate::assert_class(panels, "gg")) {

      }
      checkmate::assert_list(panels)
      sapply(panels, function(x) { checkmate::assert_class(x, "ggbrain_panel") })
      private$pvt_panels <- panels
    }
  ),
  active = list(
    #' @field panels The ggplot panels
    panels = function(val) {
      if (missing(val)) {
        return(private$pvt_panels)
      } else {
        private$set_panels(val)
      }
    }
  ),
  public = list(
    #' @description generate empty ggbrain object
    initialize = function(panels = NULL) {
      if (is.null(panels)) {
        stop("Cannot create a ggbrain object without panels!")
      } else {
        self$set_panels(panels)
      }
    },
    
    #' @description plot the composite plot
    plot = function() {
      plot(private$composite_plot)
    },
    
    #' @description return the composite plot
    get_composite_plot = function() {
      private$composite_plot
    },

    #' @description future idea? -- multiple views based on cached data
    create_view = function(slices) {
      private$views <- c(private$views, "VIEW HERE")
    }
  )
)

# allow for gg + theme() type stuff at panel level
`+.ggbrain` <- function(gg, args) {
  gg_new <- gg$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
  gg_new$panels <- lapply(gg_new$get_panels(), function(gg) { gg + args }) # add to each panel
  return(gg_new)
}

# allow for gg %+% theme() stuff at composite level
# this does not create a new ggbrain object... just adds to the composite plot.
`%+%.ggbrain` <- function(gg, args) {
  # gg_new <- gg$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
  gg$get_composite_plot() + args
  #return(gg_new)
}


# allow for gg + theme() type stuff
`+.ggbrain_panel` <-  function(gg, args) {
  gg_new <- gg$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
  gg_new$ggobj <- gg_new$get() + args # add args to panel
  return(gg_new)
}

summary.ggbrain_images <- function(gg, args) {
  gg$summary()
}


plot.ggbrain <- function(obj) {
  obj$plot()
}

