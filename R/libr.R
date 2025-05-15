if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("x", "color"))
}

libr_themes <- c(
  "LIBR_logo_sunset", list(c("#F4EB2A", "#F15C27")),
  "LIBR_logo_sky", list(c("#D6EEEF", "#1B4685")),
  "LIBR_logo_purple", list(c("#91509F", "#242468")),
  "LIBR_logo_pink", list(c("#EA118C", "#BD202C")),
  
  "LIBR_theme_dark", list(c("#242467", "#F15C27", "#BD202C", "#F68826", "#1B4685")),
  "LIBR_theme_bright", list(c("#60408F", "#F68826", "#ED2D87", "#FFC920", "#97D8EB")),
  "LIBR_theme_pastel", list(c("#D5CDE2", "#FFEFE2", "#FDEAEF", "#FFECBE", "#DEF2F7")),
  
  "LIBR_theme_bright_expanded", list(c("#60408F", "#F68826", "#ED2D87", "#FFC920", 
                                       "#97D8EB", "#1B263B", "#F4E1D2", "#56C596", 
                                       "#343A40", "#E1E8ED", "#FF7F50", "#37474F", 
                                       "#B2DFDB", "#FFAB40")),
  "LIBR_theme_dark_expanded", list(c("#242467", "#F15C27", "#BD202C", "#F68826", 
                                     "#1B4685", "#F0F0F0", "#343A40", "#FFCC00", 
                                     "#89CFF0", "#7C4DFF", "#6D9886", "#E57373", 
                                     "#D9D9D9", "#0D7377")),
  "LIBR_theme_pastel_expanded", list(c("#D5CDE2", "#FFEFE2", "#FDEAEF", "#FFECBE", 
                                       "#DEF2F7", "#E6E2F2", "#FFF5E5", "#F7E5EB", 
                                       "#FFF6D8", "#E8F7FA", "#D7EBD4", "#F4E9FF", 
                                       "#FFDFD3", "#E0F7EF")),
  
  "LIBR_gradient_purple", list(c("#D5CDE2", "#60408F", "#242467")),
  "LIBR_gradient_orange", list(c("#FFEFE2", "#F68826", "#F15C27")),
  "LIBR_gradient_pink", list(c("#FDEAEF", "#ED2D87", "#BD202C")),
  "LIBR_gradient_yellow", list(c("#FFECBE", "#FFC920", "#F68826")),
  "LIBR_gradient_blue", list(c("#DEF2F7", "#97D8EB", "#1B4685"))
) %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("palette", "colors")) %>%
  mutate(palette = as.character(palette)) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(n = length(colors))

#' Retrieve a LIBR color palette
#'
#' @param n Integer; number of colors to return.
#' @param name Character; one of the named palettes in \code{libr_themes$palette}. Continuous gradient options include: \code{"LIBR_logo_sunset"}, \code{"LIBR_logo_sky"}, \code{"LIBR_logo_purple"}, \code{"LIBR_logo_pink"}, \code{"LIBR_gradient_purple"}, \code{"LIBR_gradient_orange"}, \code{"LIBR_gradient_pink"}, \code{"LIBR_gradient_yellow"}, and \code{"LIBR_gradient_blue"}. Discrete palette options include: \code{"LIBR_theme_dark"}, \code{"LIBR_theme_bright"}, \code{"LIBR_theme_pastel"}, \code{"LIBR_theme_dark_expanded"}, \code{"LIBR_theme_bright_expanded"}, and \code{"LIBR_theme_pastel_expanded"}.
#' @return A character vector of HEX color codes.
#' @examples
#' libr.pal(3, "LIBR_theme_dark")
#' @export
libr.pal <- function(n, name){
  if(!(name %in% libr_themes$palette)){
    stop(paste0("\"", name,"\" is not a valid palette name for libr.pal\n"))
  }   
  n_max <- libr_themes$n[libr_themes$palette == name]
  these_colors <- libr_themes$colors[libr_themes$palette == name] %>% unlist
  if(n>n_max){
    warning(paste("n too large, maximum for palette",name,"is", n_max,
                  "\nInterpolating more colors using colorRampPalette().\n"))
    return(colorRampPalette(these_colors)(n))
  }else{
    return(these_colors[1:n])
  }
}

# Function to create a plot for each palette
create_palette_plot <- function(my_palette, my_palette_name, is_continuous = TRUE) {
  if(is_continuous){
    my_colors <- colorRampPalette(my_palette)(100)
  }else{
    my_colors <- my_palette
  }
  
  data <- data.frame(
    color = my_colors,
    x = 1:length(my_colors)
  )
  
  palette_name <- my_palette_name
  
  ggplot(data, aes(x = x, y = 1, fill = color)) + 
    geom_tile(show.legend = FALSE) + 
    scale_fill_identity() + 
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, size = 15, family = 'Poppins')) +
    ggtitle(palette_name)
}


#' Display all LIBR palettes in a stacked plot
#'
#' @return A \pkg{patchwork} plot showing every palette.
#' @examples
#' display.libr.all()
#' @export
display.libr.all <- function(){
  p <- plot_spacer() / 
    create_palette_plot(libr_themes$colors[[1]], libr_themes$palette[1]) / 
    create_palette_plot(libr_themes$colors[[2]], libr_themes$palette[2]) / 
    create_palette_plot(libr_themes$colors[[3]], libr_themes$palette[3]) / 
    create_palette_plot(libr_themes$colors[[4]], libr_themes$palette[4]) / 
    plot_spacer() / 
    create_palette_plot(libr_themes$colors[[11]], libr_themes$palette[11]) /
    create_palette_plot(libr_themes$colors[[12]], libr_themes$palette[12]) / 
    create_palette_plot(libr_themes$colors[[13]], libr_themes$palette[13]) / 
    create_palette_plot(libr_themes$colors[[14]], libr_themes$palette[14]) / 
    create_palette_plot(libr_themes$colors[[15]], libr_themes$palette[15]) / 
    plot_spacer() / 
    create_palette_plot(libr_themes$colors[[6]], libr_themes$palette[6], is_continuous = F) / 
    create_palette_plot(libr_themes$colors[[7]], libr_themes$palette[7], is_continuous = F) /
    create_palette_plot(libr_themes$colors[[5]], libr_themes$palette[5], is_continuous = F) / 
    plot_spacer() / 
    create_palette_plot(libr_themes$colors[[8]], libr_themes$palette[8], is_continuous = F) / 
    create_palette_plot(libr_themes$colors[[10]], libr_themes$palette[10], is_continuous = F) / 
    create_palette_plot(libr_themes$colors[[9]], libr_themes$palette[9], is_continuous = F) /
    plot_spacer()
  p
}