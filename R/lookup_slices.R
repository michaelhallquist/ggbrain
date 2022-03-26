#' @description internal function to lookup which slices to display along each axis based on their quantile,
#'   xyz coordinate, or ijk coordinate
#' @param slices A character vector of coordinates for slices to display
#' @param ignore_null_space If TRUE, any coordinates specified as quantiles (e.g., x = 50%)
#'   use the quantiles of only the non-zero slices (ignoring blank sliaces)
lookup_slices <- function(slices, ignore_null_space = TRUE) {
  checkmate::assert_character(slices)
  img_dims <- self$dim()

  slc_range_full <- list(
    i = seq_len(img_dims[1]),
    j = seq_len(img_dims[2]),
    k = seq_len(img_dims[2])
  )

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
      i = "voxel",
      j = "voxel",
      k = "voxel",
      x = "world",
      y = "world",
      z = "world",
      stop(sprintf("Cannot interpret input: %s", coord_str))
    )
    
    axis_label <- switch(
      plane,
      sagittal = "x",
      coronal = "y",
      axial = "z"
    )

    # validate input and lookup slice
    if (isTRUE(is_pct)) {
      if (plane == "sagittal") {
        rr <- slc_range$i
      } else if (plane == "coronal") {
        rr <- slc_range$j
      } else if (plane == "axial") {
        rr <- slc_range$k
      }

      slc_num <- round(quantile(rr, number))
    } else {
      if (coord == "world") { # xyz
        if (plane == "sagittal") {
          coords <- xcoords
        } else if (plane == "coronal") {
          coords <- ycoords
        } else if (plane == "axial") {
          coords <- zcoords
        }

        checkmate::assert_number(number, lower=min(coords), upper=max(coords))
        slc_num <- which.min(abs(number - coords))
      } else if (coord == "voxel") { #ijk
        if (plane == "sagittal") {
          coords <- slc_range_full$i
        } else if (plane == "coronal") {
          coords <- slc_range_full$j
        } else if (plane == "axial") {
          coords <- slc_range_full$k
        }

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

# add lookup_slices as method
ggbrain_images$set("public", "lookup_slices", lookup_slices)