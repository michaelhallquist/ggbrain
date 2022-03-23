#' R6 class for compiling images to render in ggplot
#' @importFrom RNifti voxelToWorld readNifti
#' @importFrom abind abind
#' @export
ggbrain_images <- R6::R6Class(
  classname = "ggbrain_images",
  private = list(
    pvt_imgs = list(), # image data
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

      if (!"underlay" %in% c(names(private$pvt_imgs), names(images))) {
        warning("'underlay' is not among the images provided. This may lead to weirdness downstream.")
      }

      img_list <- sapply(images, RNifti::readNifti, simplify = FALSE)
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
      private$pvt_nz_range <- self$get_nz_indices()
    }
  ),
  public = list(
    initialize = function(images) {
      private$set_images(images)
    },
    add_images = function(images) {
      private$set_images(images)
    },
    dim = function() {
      private$pvt_dims
    },
    get_images = function(img_names = NULL, drop = TRUE) {
      checkmate::assert_logical(drop, len=1L)
      if (is.null(img_names)) {
        ret <- private$pvt_imgs
      } else {
        checkmate::assert_subset(img_names, names(private$pvt_imgs))
        ret <- private$pvt_imgs[img_names]
      }

      if (length(ret) == 1L && isTRUE(drop)) {
        ret <- ret[[1L]] # unlist
      }

      return(ret)
    },
    get_headers = function(img_names = NULL, drop = TRUE) {
      checkmate::assert_logical(drop, len=1L)
      if (is.null(img_names)) {
        ret <- private$pvt_imgs
      } else {
        checkmate::assert_subset(img_names, names(private$pvt_imgs))
        ret <- private$pvt_imgs[img_names]
      }

      ret <- sapply(ret, niftiHeader, simplify=FALSE)

      if (length(ret) == 1L && isTRUE(drop)) {
        ret <- ret[[1L]] # unlist
      }

      return(ret)
    },
    remove_images = function(img_names) {
      checkmate::assert_character(img_names)
      good_imgs <- intersect(names(private$pvt_imgs), img_names)
      bad_imgs <- setdiff(img_names, names(private$pvt_imgs))

      if (length(good_imgs) > 0L) {
        message(glue("Removing images: {paste(good_imgs, collapse=', ')}"))
        private$pvt_imgs[good_imgs] <- NULL
      }

      if (length(bad_imgs) > 0L) {
        warning(glue("Could not find these images to remove: {paste(bad_imgs, collapse=', ')}"))
      }

    },
    summary = function() {
      cat("\nImage dimensions:\n")
      print(private$pvt_dims)
      cat("\nImages in object:\n")
      print(private$pvt_imgs)

    },
    get_nz_indices = function(img_names = NULL) {
      if (is.null(img_names)) {
        if (!is.null(private$pvt_nz_range)) {
          return(private$pvt_nz_range) # return pre-cached dims (reflects all images), if available
        } else {
          img_names <- names(private$pvt_imgs)
        }
      } else {
        checkmate::assert_subset(img_names, names(private$pvt_imgs))
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
    get_slices = function(slices, img_names = NULL, make_square = TRUE, remove_null_space = TRUE, as_data_frame=TRUE) {
      slice_df <- lookup_slices(slices, self) # defaults to ignoring null space
      coords <- slice_df %>%
        group_by(slice_index) %>%
        group_split()

      slc <- lapply(coords, function(slc) {
        self$get_slices_inplane(img_names, slc$slice_number, slc$plane, drop = TRUE)
      })

      if (isTRUE(make_square)) {
        browser()
        slc_dims <- sapply(flatten(slc), dim)
        square_dims <- apply(slc_dims, 1, max)
        square_mat <- array(NA_real_, dim = square_dims)
        square_melt <- reshape2::melt(square_mat, varnames = c("dim1", "dim2"), value.name = "dummy")

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
    get_slices_inplane = function(imgs = NULL, slice_numbers, plane, drop=FALSE) {
      if (is.null(imgs)) {
        imgs <- names(private$pvt_imgs)
      } else if (!checkmate::test_subset(imgs, names(private$pvt_imgs))) {
        stop(glue("The img input to $get_slice() must be one of: {paste(names(private$pvt_imgs), collapse=', ')}"))
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


ggbrain <- R6::R6Class(
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
    #' @field sided Whether to clusterize 1-sided ('one'), two-sided ('two'), or bi-sided ('bi')
    panels = function(val) {
      if (missing(val)) {
        return(private$pvt_panels)
      } else {
        private$set_panels(val)
      }
    }
  ),
  public = list(
    initialize = function(panels = NULL) {
      if (is.null(panels)) {
        stop("Cannot create a ggbrain object without panels!")
      } else {
        self$set_panels(panels)
      }
    },
    plot = function() {
      plot(private$composite_plot)
    },
    get_composite_plot = function() {
      private$composite_plot
    },

    # future idea? -- multiple views based on cached data
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

