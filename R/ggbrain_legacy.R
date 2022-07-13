#' Plot fMRI data on an underlay image
#' 
#' @param underlay a 3D nifti image used for the image underlay (default b/w)
#' @param overlay a 4D nifti image used for plotting stats on underlay (color)
#' @param underlay_colorscale A ggplot scale_fill_* function call used for coloration of underlay
#' @param positive_colorscale A ggplot scale_fill_* function call used for coloration of positive overlay values
#' @param negative_colorscale A ggplot scale_fill_* function call used for coloration of negative overlay values
#' @param slices A data.frame consisting of slices to display. Minimally, this should contain a column called coord that
#'   the x (sagittal), y (coronal), and z (axial) slices to display. Percentages can be used (e.g., "x = 25%").
#' @param remove_null_space If TRUE, plots are trimmed of empty space that was in the NIfTI.
#' @param pos_thresh The positive threshold to be applied to the \code{overlay} image. Any positive voxel less than
#'   this threshold will be removed from the map.
#' @param neg_thresh The negative threshold to be applied to the \code{overlay} image. Any negative voxel greater than
#'   this threshold will be removed from the map.
#' @param legend_label An expression or character string to display over the color scale.
#' @param background_color A character string specifying what color should be used for the plot background. Default: "gray10"
#' @param text_color A character string specifying what color should be used for the text on the plot. Default: "white"
#' @param trim_underlay Winsorize the extreme values of the underlay based on the low and high quantiles provided
#' @param zero_underlay Any voxels in the underlay image whose absolute values are less than \code{zero_underlay}
#'   are set to precisely zero. This helps with small color variation in black/empty space.
#' @param theme_custom A ggplot2 theme object that will be added to each panel.
#' @param base_size The base_size argument passed to the theme, controlling overall font sizes
#' @param nrow Passed to cowplot::plot_grid, controls number of rows in multi-panel plot
#' @param ncol Passed to cowplot::plot_grid, controls number of columns in multi-panel plot
#' @param title Passed to patchwork::plot_annotation and adds overall title
#' 
#' @importFrom checkmate assert_numeric
#' @importFrom cowplot add_sub get_legend plot_grid
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @importFrom dplyr bind_rows filter mutate if_else group_by group_split %>%
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 aes coord_fixed element_blank element_rect element_text
#'   geom_raster ggplot guide_colorbar labs scale_fill_gradient scale_fill_distiller
#'   theme_void xlab ylab
#' @importFrom stats sd
#' @export
ggbrain_legacy <- function(underlay=NULL, overlay=NULL, 
                    underlay_colorscale = scale_fill_gradient(low="grey8", high="grey92"),
                    negative_colorscale = scale_fill_distiller(palette="Blues", direction = 1),
                    positive_colorscale = scale_fill_distiller(palette="Reds"),
                    slices = data.frame(
                      coord = c("x = 25%", "x = 50%", "x = 75%",
                                "y = 25%", "y = 50%", "y = 75%",
                                "z = 25%", "z = 50%", "z = 75%")
                    ),
                    remove_null_space=TRUE,
                    pos_thresh = 1, neg_thresh = -1, legend_label = expression(italic("z")),
                    background_color = "gray10", text_color = "white",
                    trim_underlay = c(.01, .99), zero_underlay = 1e-3, symmetric_legend = TRUE,
                    panel_labels = NULL, underlay_contrast = "none", panel_borders = TRUE,
                    theme_custom = NULL, base_size = 14,
                    nrow = NULL, ncol = NULL, title = NULL
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

  # winsorize extreme values in underlay based on quantiles of non-zero voxels
  gg_imgs$winsorize_images("underlay", trim_underlay)

  # set any tiny value in underlay to NA
  gg_imgs$na_images("underlay", 1e-8)
  
  # sigmoid transform
  #underlay2 <- underlay + underlay*underlay_contrast*(1/(1+exp(-underlay)))
  #underlay <- underlay*5*(1/(1+exp(-underlay)))

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
    
    # redefine these on a per-panel basis since some panels may lack a pos or neg component even if others have it
    has_pos <- sum(!is.na(p_df$value)) > 0L
    has_neg <- sum(!is.na(n_df$value)) > 0L

    a_layer <- ggbrain_layer$new(name = "underlay", data = a_df, color_scale = underlay_colorscale, show_legend=FALSE)
    n_layer <- ggbrain_layer$new(name = "negative", data = n_df, color_scale = negative_colorscale, limits = neg_limits, breaks = range_breaks(digits = stat_decimals), show_legend=TRUE)
    p_layer <- ggbrain_layer$new(name = "positive", data = p_df, color_scale = positive_colorscale, limits = pos_limits, breaks = range_breaks(digits = stat_decimals), show_legend=TRUE)
    panel <- ggbrain_panel$new(
      layers = list(a_layer, n_layer, p_layer),
      title = slices$panel_title[i],
      bg_color = background_color,
      text_color = text_color,
      draw_border = panel_borders,
      xlab = slices$xlab[i],
      ylab = slices$ylab[i],
      theme_custom = theme_custom
    )
    
    #panel$add_to_gg(theme_bw() + ggtitle("hello"))
    
    # modify scale breaks, limits, and guide order -- need to hack this from the existing scale since adding a new scale
    # overrides all of the information, rather than keeping what we want
    if (has_neg) {
      panel$gg$scales$scales[[2]]$name <- ifelse(has_pos, "", legend_label) # only add label to neg scale if there are no positive values
      panel$gg$scales$scales[[2]]$guide <- guide_colorbar(order = 2, available_aes = c("fill", "fill_new"), ticks.colour = text_color)
    }

    # position of positive and negative color scales in g$scales$scales list
    ppos <- ifelse(has_pos && has_neg, 3, 2)

    if (has_pos) {
      panel$gg$scales$scales[[ppos]]$name <- legend_label # label above positive extent
      panel$gg$scales$scales[[ppos]]$guide <- guide_colorbar(order = 1, available_aes = c("fill", "fill_new"), ticks.colour = text_color)
    }

    slice_info <- attr(slice_data, "slice_info")
    
    if (isTRUE(slices$coord_label[i])) {
      # annotate will expand the coordinates of the plot, if needed
      xrange <- diff(range(a_df$dim1, na.rm=T))
      yrange <- diff(range(a_df$dim2, na.rm=T))
      label_x_pos <- max(a_df$dim1, na.rm=T) + .01*xrange  # place slightly to the right of the furthest point
      label_y_pos <- min(a_df$dim2, na.rm=T) - .07*yrange  # place slightly below the lowest point
      
      panel$gg <- panel$gg + annotate(geom = "text", x = label_x_pos, y = label_y_pos, label = slice_info$coord_label[i], 
                        hjust = 1, vjust = 0, color=text_color, size = (base_size*.6)/ggplot2::.pt)
    }
    
    # cache legend
    leg <- get_legend(panel$gg)

    # remove legend from subplots (only needed for cowplot)
    # g <- g + theme(legend.position = "none")

    # cache legend for extraction
    attr(panel$gg, "legend") <- leg

    return(panel$gg)
  }

  glist <- lapply(seq_along(anat_slices), make_slice_plot)
  #leg <- get_legend(glist[[1]] + theme(legend.position="right"))
  
  # panel_height <- (unit(1,"npc") - sum(ggplotGrob(glist[[1]])[["heights"]][-3]) - unit(1,"line"))/5
  # 
  
  g_all <- wrap_plots(glist) + plot_layout(guides = 'collect', ncol = ncol, nrow = nrow) +
    plot_annotation(
      theme = theme(
        plot.background = element_rect(fill=background_color, color = NA),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = base_size)
      ),
      title = title
    )
  
  #plot.margin=grid::unit(c(0,0,0,0), "mm")
  
  # & guides(
  #   fill = guide_colorbar(barheight=panel_height),
  #   fill_new = guide_colorbar(barheight = panel_height, available_aes = "fill_new"),
  #   #fill_new_new = guide_colorbar(barheight = unit(0.1, "cm"), available_aes = "fill_new_new")
  # )
  
  # g_all <- glist[[1]] + guides(
  #   fill = guide_colorbar(barheight=panel_height),
  #   fill_new = guide_colorbar(barheight = panel_height, available_aes = "fill_new"),
  #   #fill_new_new = guide_colorbar(barheight = unit(0.1, "cm"), available_aes = "fill_new_new")
  # )
  
  
  # g_all <- plot_grid(plotlist = glist, labels = panel_labels, nrow = nrow, ncol = ncol) #, labels = "AUTO", label_colour = text_color)
  # g_all <- plot_grid(g_all, attr(glist[[1]], "legend"), rel_widths=c(1, 0.3)) +
  #   theme(plot.background = element_rect(fill=background_color, color = NA))

  # +theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  return(g_all)
}
