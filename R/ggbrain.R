#' Plot fMRI data on an underlay image
#' 
#' @param underlay a 3D nifti image used for the image underlay (default b/w)
#' @param overlay a 4D nifti image used for plotting stats on underlay (color)
#' @param color_col a position in the 4th dim of overlay use to color plots
#' @param alpha_col a position in the 4th dim of overlay use to set alpha transparency of plots
#' @param underlay_colorscale A ggplot scale_fill_* function call used for coloration of underlay
#' @param positive_colorscale A ggplot scale_fill_* function call used for coloration of positive overlay values
#' @param negative_colorscale A ggplot scale_fill_* function call used for coloration of negative overlay values
#' @param remove_null_space If TRUE, quantiles are computed on the non-zero slices and plots are trimmed for
#'   empty space.
#' @param pos_thresh The positive threshold to be applied to the \code{overlay} image. Any positive voxel less than
#'   this threshold will be removed from the map.
#' @param neg_thresh The negative threshold to be applied to the \code{overlay} image. Any negative voxel greater than
#'   this threshold will be removed from the map.
#' @param zero_underlay Any voxels in the underlay image whose absolute values are less than \code{zero_underlay}
#'   are set to precisely zero. This helps with small color variation in black/empty space.
#' @param trim_underlay Winsorize the extreme values of the underlay based on the low and high quantiles provided
#' @param nrow Passed to cowplot::plot_grid, controls number of rows in multi-panel plot
#' @param ncol Passed to cowplot::plot_grid, controls number of columns in multi-panel plot
#' @importFrom checkmate assert_numeric
#' @importFrom cowplot add_sub get_legend plot_grid
#' @importFrom dplyr bind_rows filter mutate if_else group_by group_split %>%
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 aes coord_fixed element_blank element_rect element_text
#'   geom_raster ggplot guide_colorbar labs scale_fill_gradient scale_fill_distiller
#'   theme_void xlab ylab
#' @importFrom stats sd
#' @export
ggbrain <- function(underlay=NULL, overlay=NULL, 
                    color_col=NULL, alpha_col=NULL,
                    underlay_colorscale = scale_fill_gradient(low="grey8", high="grey92"),
                    negative_colorscale = scale_fill_distiller(palette="Blues", direction = 1),
                    positive_colorscale = scale_fill_distiller(palette="Reds"),
                    slices = data.frame(
                      coord = c("x = 25%", "x = 50%", "x = 75%",
                                "y = 25%", "y = 50%", "y = 75%",
                                "z = 25%", "z = 50%", "z = 75%")
                    ),
                    remove_null_space=TRUE,
                    pos_thresh = 1,
                    neg_thresh = -1, legend_label = expression(italic("z")),
                    background_color = "gray10", text_color = "white",
                    trim_underlay = c(.01, .99), zero_underlay = 1e-3, symmetric_legend = TRUE,
                    panel_labels = NULL, underlay_contrast = "none", panel_borders = TRUE,
                    theme_custom = NULL, base_size = 14,
                    nrow = NULL, ncol = NULL
) {
  
  checkmate::assert_class(underlay_colorscale, "ScaleContinuous")
  checkmate::assert_class(negative_colorscale, "ScaleContinuous")
  checkmate::assert_class(positive_colorscale, "ScaleContinuous")
  
  # force transparency for NA values on each layer to make things visible
  if (underlay_colorscale$na.value != "transparent") { 
    underlay_colorscale$na.value <- "transparent"
  }
  
  if (positive_colorscale$na.value != "transparent") { 
    positive_colorscale$na.value <- "transparent"
  }
  
  if (negative_colorscale$na.value != "transparent") { 
    negative_colorscale$na.value <- "transparent"
  }
  
  # handle data.frame
  if (is.list(slices)) {
    slices <- bind_rows(slices)
  }
  
  if (is.null(slices$coord_label)) {
    slices$coord_label <- TRUE # default to labeling slices
  }
  
  max_bg <- "gray95" # brightest value on underlay
  
  checkmate::assert_file_exists(underlay)
  checkmate::assert_file_exists(overlay)
  checkmate::assert_class(theme_custom, "theme", null.ok = TRUE)
  checkmate::assert_integerish(nrow, lower = 1, len = 1L, null.ok = TRUE)
  checkmate::assert_integerish(ncol, lower = 1, len = 1L, null.ok = TRUE)
  
  stat_decimals <- 2 # rounding for overlay

  gg_imgs <- ggbrain_images$new(c(underlay = underlay, overlay = overlay))

  # round very small values to zero
  # if (!is.null(zero_underlay) && zero_underlay > 0) {
  #   underlay[underlay > -1*zero_underlay & underlay < zero_underlay] <- 0
  # }

  # winsorize extreme values in underlay based on quantiles of non-zero voxels
  gg_imgs$winsorize_images("underlay", trim_underlay)

  gg_imgs$na_images("underlay", 1e-8)
  
  # sigmoid transform
  #underlay2 <- underlay + underlay*underlay_contrast*(1/(1+exp(-underlay)))
  #underlay <- underlay*5*(1/(1+exp(-underlay)))
  #browser()

  underlay <- gg_imgs$get_images("underlay")

  # Beta of 1 is very weak contrast enhancement, 10 is very steep
  sigmoid <- function(x, beta=1) {
    # make beta (slope) scale invariant by standardizing (note that scale is very slow for array-level normalization)
    m_x <- mean(x, na.rm=T)
    s_x <- sd(x, na.rm=T)
    z_x <- (x - m_x)/s_x

    y <- 1/(1+exp(-beta*(z_x)))
    return(y*x)
  }
  
  if (underlay_contrast == "low") {
    beta <- .1
  } else if (underlay_contrast == "medium") {
    beta <- .8
  } else if (underlay_contrast == "high") {
    beta <- 1.6
  }

  if (underlay_contrast != "none") {
    underlay <- sigmoid(underlay, beta)
  }


  # verify that i,j,k (1,2,3) dimensions of underlay match dimensions of overlay
  # stopifnot(identical(dim(underlay)[1:3], dim(overlay)[1:3]))

  # need grouping variables to be factors to maintain order in group_by %>% group_split
  # coords_df <- lookup_slices(slices$coord, underlay, remove_null_space)
  
  # split into row-wise list for lapply inside slice lookup
  # coords <- coords_df %>% group_by(slice_index) %>% group_split()
  
  slice_data <- gg_imgs$get_slices(slices$coord, make_square = TRUE, remove_null_space = remove_null_space)
  
  anat_slices <- slice_data %>%
    filter(image == "underlay")

  # set voxels below threshold to NA to keep data square (avoid geom_raster complaints)
  pos_plot <- slice_data %>%
    filter(image == "overlay") %>%
    mutate(value = if_else(value < !!pos_thresh, NA_real_, value))
  
  neg_plot <- slice_data %>% 
    filter(image == "overlay") %>%
    mutate(value = if_else(value > !!neg_thresh, NA_real_, value))
  
  has_pos <- sum(!is.na(pos_plot$value)) > 0L
  has_neg <- sum(!is.na(neg_plot$value)) > 0L

  h_stat <- ifelse(has_pos, max(pos_plot$value, na.rm=TRUE), 0)
  l_stat <- ifelse(has_neg, min(neg_plot$value, na.rm=TRUE), 0)
  
  if (isTRUE(symmetric_legend)) {
    biggest <- max(abs(c(h_stat, l_stat)))
    h_stat <- biggest
    l_stat <- -1*biggest
  }

  # legend color scale limits
  pos_limits <- round(c(pos_thresh, h_stat), stat_decimals)
  neg_limits <- round(c(l_stat, neg_thresh), stat_decimals)

  # go to split approach, rather than dealing with facet_wrap issues -- need drop = FALSE to keep lengths the same if some slices blank
  anat_slices <- anat_slices %>% group_by(slice_index, .drop = FALSE) %>% group_split()
  pos_plot <- pos_plot %>% group_by(slice_index, .drop = FALSE) %>% group_split()
  neg_plot <- neg_plot %>% group_by(slice_index, .drop = FALSE) %>% group_split()

  make_slice_plot <- function(i) {
    a_df <- anat_slices[[i]]
    p_df <- pos_plot[[i]]
    n_df <- neg_plot[[i]]

    a_layer <- ggbrain_layer$new(layer_df = a_df, layer_scale = underlay_colorscale, show_scale=FALSE)
    p_layer <- ggbrain_layer$new(layer_df = p_df, layer_scale = positive_colorscale, show_scale=TRUE)
    n_layer <- ggbrain_layer$new(layer_df = n_df, layer_scale = negative_colorscale, show_scale=TRUE)
    
    browser()
    # add layers to plot (bottom to top)
    g <- ggplot(mapping = aes(x=dim1, y=dim2)) +
      a_layer + n_layer + p_layer
    
    # rename columns to ensure that the multiple fill scales in the plot have unique variable names
    a_df <- a_df %>% dplyr::rename(uval = value)
    p_df <- p_df %>% dplyr::rename(pos = value)
    n_df <- n_df %>% dplyr::rename(neg = value)
    
    
    #g <- ggplot(mapping = aes(x=dim1, y=dim2)) +
    #  #geom_tile(data = anat_slices, mapping = aes(fill=uval), show.legend = FALSE, color = NA, height = 1.01, width = 1.01) +
    #  #geom_tile(data = anat_slices, mapping = aes(fill=uval, color=uval), show.legend = FALSE) +
    #  geom_raster(data = a_df, mapping = aes(fill=uval), show.legend = FALSE, interpolate = FALSE) +
    #  underlay_colorscale # scales[[1]]
    
    # negative overlay values
    if (has_neg) {
      g <- g +
        new_scale_fill() +
        geom_raster(data = n_df, mapping = aes(fill=neg), interpolate = FALSE) +
        negative_colorscale # scales [[2]]
    
      # examples
      # scale_fill_viridis_c(begin = .48, end=0) +
      # scale_fill_distiller("", palette="Blues", direction = 1,  breaks = integer_breaks(), limits = neg_limits,
      #                      guide = guide_colorbar(order = 2))
    }
    
    #browser()
    # positive overlay values
    if (has_pos) {
      g <- g +
        new_scale_fill() + 
        geom_raster(data = p_df, mapping = aes(fill=pos), interpolate = FALSE) +
        positive_colorscale # scales[[3]]

      #scale_fill_viridis_c(begin = 0.52, end=1) +
      # scale_fill_distiller(legend_label, palette="Reds", breaks = integer_breaks(), limits = pos_limits, 
      #                       guide = guide_colorbar(order = 1))  
    }
    
    # modify scale breaks, limits, and guide order -- need to hack this from the existing scale since adding a new scale
    # overrides all of the information, rather than keeping what we want
    if (has_neg) {
      # g$scales$scales[[2]]$breaks <- integer_breaks()
      # g$scales$scales[[2]]$breaks <- pretty_breaks()
      g$scales$scales[[2]]$breaks <- range_breaks(digits = stat_decimals)
      g$scales$scales[[2]]$limits <- neg_limits
      g$scales$scales[[2]]$name <- ifelse(has_pos, "", legend_label) # only add label to neg scale if there are no positive values
      g$scales$scales[[2]]$guide <- guide_colorbar(order = 2, available_aes = c("fill", "fill_new"), ticks.colour = text_color)
    }

    # position of positive and negative color scales in g$scales$scales list
    ppos <- ifelse(has_pos && has_neg, 3, 2)

    if (has_pos) {
      #g$scales$scales[[ppos]]$breaks <- integer_breaks()
      #g$scales$scales[[ppos]]$breaks <- pretty_breaks() # ggplot default for now
      g$scales$scales[[ppos]]$breaks <- range_breaks(digits = stat_decimals) # ggplot default for now
      g$scales$scales[[ppos]]$limits <- pos_limits
      g$scales$scales[[ppos]]$name <- legend_label # label above positive extent
      g$scales$scales[[ppos]]$guide <- guide_colorbar(order = 1, available_aes = c("fill", "fill_new"), ticks.colour = text_color)
    }

    # annotation
    # g <- g +
    #   annotate(geom="text", x = Inf, y = -Inf, label=coords_df$coord_label[i], color = text_color, hjust = 1.5, vjust = -1.5)

    # theme refinements
    g <- g +
      # geom_text(data = coords_df, mapping = aes(label = coord_label, x = Inf, y = -Inf), 
      #           color="white", vjust = 0.5, hjust = 1, size=1) +
      theme_void(base_size = base_size) + coord_fixed() + #+ facet_wrap(~slice, drop=T) + 
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(), 
        plot.background = element_rect(fill=background_color, color=ifelse(isTRUE(panel_borders), text_color, NA)),
        panel.background = element_rect(fill=background_color, color=ifelse(isTRUE(panel_borders), text_color, NA)),
        text = element_text(color = text_color),
        legend.spacing.y = unit(0.1, "lines"),
        legend.position = "right",
        plot.margin = unit(c(0.0, 0.5, 0.0, 0.5), "lines") # space on L and R, but not T and B
      )
    
    # add subcaption with cowplot -- note that this makes the object a gtable, not a ggplot object
    # g <- g %>% add_sub(coords_df$coord_label[i], x = 0.9, hjust = 1, color = text_color) # right justified
    
    slice_info <- attr(slice_data, "slice_info")
    
    if (isTRUE(slices$coord_label[i])) {
      # new approach in ggplot 3+: use caption for label since that maintains this as a ggplot object
      #g <- g + theme(plot.caption = element_text(hjust = 0.8, vjust = 8)) + labs(caption = slice_info$coord_label[i])  
      #g <- g + annotate(geom = "text", x = Inf, y = -Inf, label = slice_info$coord_label[i], hjust = 1, vjust = 0)
      
      # final decision: annotate gives us fine control over positioning and doesn't expand panel into plot margin areas
      # label_x_pos <- quantile(a_df$dim1, .95, na.rm=TRUE) # 90% right
      # label_y_pos <- quantile(a_df$dim2, 0, na.rm=TRUE) # 10% off the bottom
      
      # annotate will expand the coordinates of the plot, if needed
      label_x_pos <- max(a_df$dim1, na.rm=T) + 2  # place slightly to the right of the furthest point
      label_y_pos <- min(a_df$dim2, na.rm=T) - 6  # place slightly below the lowest point
      
      g <- g + annotate(geom = "text", x = label_x_pos, y = label_y_pos, label = slice_info$coord_label[i], hjust = 1, vjust = 0)
    }

    # add x axis label if requested
    if (!is.null(slices$xlab[i]))  {
      g <- g + theme(axis.title.x = element_text()) + xlab(slices$xlab[i])
    }

    # add y axis label if requested
    if (!is.null(slices$ylab[i]))  {
      g <- g + theme(axis.title.y = element_text()) + ylab(slices$ylab[i])
    }

    # add title if requested
    if (!is.null(slices$panel_title[i]))  {
      g <- g + ggtitle(slices$panel_title[i])
    }

    # add custom theme elements to each panel, if requested
    if (!is.null(theme_custom)) {
      g <- g + theme_custom
    }

    # cache legend
    leg <- get_legend(g)

    # remove legend from subplots
    g <- g + theme(legend.position = "none")

    # cache legend for extraction
    attr(g, "legend") <- leg

    # ggp <- ggbrain_panel_r6$new(g)

    return(g)
  }
  
  # geom_raster is grumpy about blank space
  #glist <- suppressWarnings(lapply(seq_along(anat_slices), make_slice_plot))
  glist <- lapply(seq_along(anat_slices), make_slice_plot)
  #leg <- get_legend(glist[[1]] + theme(legend.position="right"))
  
  g_all <- plot_grid(plotlist = glist, labels = panel_labels, nrow = nrow, ncol = ncol) #, labels = "AUTO", label_colour = text_color)
  g_all <- plot_grid(g_all, attr(glist[[1]], "legend"), rel_widths=c(1, 0.2)) +
    theme(plot.background = element_rect(fill=background_color, color = NA))
  #+theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  plot(g_all)
}
