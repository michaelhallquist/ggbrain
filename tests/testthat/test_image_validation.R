test_that("images with matching dimensions, pixdim, and affine can be combined", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # Test combining in a single images() call
  expect_no_error({
    ggb_obj <- images(c(underlay = underlay, overlay = overlay))
  })
  # images() returns a ggb object, access image names via ggb_images

  expect_equal(length(ggb_obj$ggb_images$get_image_names()), 2)

  # Test combining via sequential images() calls (using + operator)
  expect_no_error({
    gg_obj <- ggbrain() +
      images(c(underlay = underlay)) +
      images(c(overlay = overlay))
  })
  expect_equal(length(gg_obj$ggb_images$get_image_names()), 2)
})

test_that("images with matching properties combine correctly with add() method", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")
  echange <- system.file("extdata", "echange_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # Create separate image objects
  img1 <- ggbrain_images$new(c(underlay = underlay))
  img2 <- ggbrain_images$new(c(overlay = overlay))
  img3 <- ggbrain_images$new(c(echange = echange))

  # Add them together
  expect_no_error({
    img1$add(img2)
    img1$add(img3)
  })

  expect_equal(length(img1$get_image_names()), 3)
  expect_setequal(img1$get_image_names(), c("underlay", "overlay", "echange"))
})

test_that("dimension mismatch between images is detected", {
  skip_on_cran()

  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # Create a temporary image with different dimensions
  img <- RNifti::readNifti(underlay)
  # Subset to create different dimensions
  small_img <- img[1:50, 1:50, 1:50]

  temp_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(temp_file), add = TRUE)
  RNifti::writeNifti(small_img, temp_file)

  expect_error(
    images(c(underlay = underlay, small = temp_file)),
    "dimensions do not match"
  )
})

test_that("dimension mismatch is detected when combining via add() method", {
  skip_on_cran()

  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # Create a temporary image with different dimensions
  img <- RNifti::readNifti(underlay)
  small_img <- img[1:50, 1:50, 1:50]

  temp_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(temp_file), add = TRUE)
  RNifti::writeNifti(small_img, temp_file)

  img1 <- ggbrain_images$new(c(underlay = underlay))
  img2 <- ggbrain_images$new(c(small = temp_file))

  expect_error(
    img1$add(img2),
    "Dimensions.*do not match"
  )
})

test_that("voxel size (pixdim) mismatch between images is detected", {
  skip_on_cran()

  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # Create a temporary image with different voxel sizes
  img <- RNifti::readNifti(underlay)
  # Change the pixdim (voxel sizes)
  RNifti::pixdim(img) <- c(3, 3, 3) # 3mm instead of 2mm

  temp_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(temp_file), add = TRUE)
  RNifti::writeNifti(img, temp_file)

  expect_error(
    images(c(underlay = underlay, different_res = temp_file)),
    "Voxel sizes do not match"
  )
})

test_that("voxel size mismatch is detected when combining via add() method", {
  skip_on_cran()

  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # Create a temporary image with different voxel sizes
  img <- RNifti::readNifti(underlay)
  RNifti::pixdim(img) <- c(3, 3, 3)

  temp_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(temp_file), add = TRUE)
  RNifti::writeNifti(img, temp_file)

  img1 <- ggbrain_images$new(c(underlay = underlay))
  img2 <- ggbrain_images$new(c(different_res = temp_file))

  expect_error(
    img1$add(img2),
    "Voxel sizes.*do not match"
  )
})

test_that("translated affines are rejected on import, addition, and derived images", {
  original <- RNifti::asNifti(array(1, c(4, 5, 6)))
  shifted <- RNifti::asNifti(array(2, c(4, 5, 6)))
  affine <- diag(4)
  affine[1, 4] <- 10
  RNifti::sform(shifted) <- structure(affine, code = 2L)
  paths <- c(original = tempfile(fileext = ".nii"), shifted = tempfile(fileext = ".nii"))
  on.exit(unlink(paths), add = TRUE)
  RNifti::writeNifti(original, paths[[1]])
  RNifti::writeNifti(shifted, paths[[2]])

  expect_error(images(paths), "Affine matrices do not match")
  expect_error(images(paths[1]) + images(paths[2]), "Affine matrices do not match")
  collection <- ggbrain_images$new(paths[1])
  expect_error(collection$add_array_as_image(array(1, c(4, 5, 6)), "bad", shifted),
               "Affine matrices do not match")
  expect_identical(collection$get_image_names(), "original")
})

test_that("empty ggbrain_images object can be added without error", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  img1 <- ggbrain_images$new(c(underlay = underlay))
  empty_img <- ggbrain_images$new()

  # Adding empty object should not error and should return unchanged

  expect_no_error({
    result <- img1$add(empty_img)
  })
  expect_equal(length(img1$get_image_names()), 1)
})
