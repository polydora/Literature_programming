# Colours for spbu online courses
# reworked after https://raw.githubusercontent.com/drsimonj/blogR/master/Rmd/creating_corporate_colors_ggplot2.Rmd

# spbu corporate colors ####
spbu_colors <- c(
  `lightgrey`  = "#BFBFBF",
  `darkgrey`   = "#8C8C8C",
  `black`      = "#1E1E1E",
  `violet`     = "#D4CFE8",
  `darkviolet` = "#6576B9",
  `blue`       = "#627AEB",
  `darkblue`   = "#1870B8",
  `steelblue`  = "#4682B4",
  `lightteal`  = "#DEECEE",
  `teal`       = "#BCD8DD",
  `green`      = "#A8CFA6",
  `darkgreen`  = "#609D83",
  `yellow`     = "#FEC00F",
  `orange`     = "#F37735",
  `red`        = "#CB3757")


#' Function to extract colors as hex codes
#'
#' @param ... Character names of spbu_colors
#'
spbu_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (spbu_colors)
  spbu_colors[cols]
}

# Palettes
spbu_palettes <- list(
  `main`  = spbu_cols("steelblue", "yellow", "darkgreen", "red", "green", "orange", "darkviolet", "darkgrey", "black"),
  `caegorical` = spbu_cols('red', 'blue', 'green'),
  `contrast` = spbu_cols("steelblue", "red"),
  `cold`  = spbu_cols("steelblue", "green", "darkgreen", "blue", "teal"),
  `hot`   = spbu_cols("red", "yellow", "orange"),
  `grey`  = spbu_cols("black", "lightgrey", "darkgrey")
)

#' Return function to interpolate a spbu color palette
#'
#' @param palette Character name of palette in spbu_palettes
#' @param reverse Boolean indicating whether the palette should be reversed

spbu_pal <- function (palette = "main", reverse = FALSE, names = FALSE)
{
  pal <- spbu_palettes[[palette]]
  function(n) {
    if (n <= length(pal)) {
      pal <- pal[seq_len(n)]
    }
    if (n > length(pal)) {
      warning(paste0("n too large, allowed maximum for palette '",
                     palette, "' is ", length(pal)),
              "\nReturning a gradient palette from 2 first colors\n")
      pal <- colorRampPalette(pal[1:2])(n)
    }
    if (reverse) {
      pal <- rev(pal)
    }
    if (names == TRUE) {
      return(pal)
    } else {
      return(as.character(pal))
    }
  }
}


#' Color scale constructor for spbu colors
#'
#' @param palette Character name of palette in spbu_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_spbu <- scale_color_spbu <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spbu_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("spbu_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = colorRampPalette(pal(2))(3), ...)
  }
}




#' Fill scale constructor for spbu colors
#'
#' @param palette Character name of palette in spbu_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_spbu <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spbu_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("spbu_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = colorRampPalette(pal(2))(3), ...)
  }
}


