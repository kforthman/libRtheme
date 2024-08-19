library(showtext)
library(ggplot2)
library(scales)
library(patchwork)

font_add("Poppins", "fonts/poppins-v21-latin-500.ttf")
showtext_auto()


# Define your palettes
LIBR_logo_sunset <- c("#F4EB2A", "#F15C27")
LIBR_logo_sky    <- c("#D6EEEF", "#1B4685")
LIBR_logo_purple <- c("#91509F", "#242468")
LIBR_logo_pink   <- c("#EA118C", "#BD202C")

LIBR_theme_dark   <- c("#242467", "#F15C27", "#BD202C", "#F68826", "#1B4685")
LIBR_theme_bright <- c("#60408F", "#F68826", "#ED2D87", "#FFC920", "#97D8EB")
LIBR_theme_pastel <- c("#D5CDE2", "#FFEFE2", "#FDEAEF", "#FFECBE", "#DEF2F7")


LIBR_theme_bright_expanded <- c("#60408F", "#F68826", "#ED2D87", "#FFC920", 
                                "#97D8EB", "#1B263B", "#F4E1D2", "#56C596", 
                                "#343A40", "#E1E8ED", "#FF7F50", "#37474F", 
                                "#B2DFDB", "#FFAB40")

LIBR_theme_dark_expanded   <- c("#242467", "#F15C27", "#BD202C", "#F68826", 
                                "#1B4685", "#F0F0F0", "#343A40", "#FFCC00", 
                                "#89CFF0", "#7C4DFF", "#6D9886", "#E57373", 
                                "#D9D9D9", "#0D7377")

LIBR_theme_pastel_expanded <- c("#D5CDE2", "#FFEFE2", "#FDEAEF", "#FFECBE", 
                                "#DEF2F7", "#E6E2F2", "#FFF5E5", "#F7E5EB", 
                                "#FFF6D8", "#E8F7FA", "#D7EBD4", "#F4E9FF", 
                                "#FFDFD3", "#E0F7EF")

LIBR_gradient_purple <- c("#D5CDE2", "#60408F", "#242467")
LIBR_gradient_orange <- c("#FFEFE2", "#F68826", "#F15C27")
LIBR_gradient_pink   <- c("#FDEAEF", "#ED2D87", "#BD202C")
LIBR_gradient_yellow <- c("#FFECBE", "#FFC920", "#F68826")
LIBR_gradient_blue   <- c("#DEF2F7", "#97D8EB", "#1B4685")

# Function to create a plot for each palette
create_palette_plot <- function(my_palette, is_continuous = TRUE) {
  if(is_continuous){
    my_colors <- colorRampPalette(my_palette)(100)
  }else{
    my_colors <- my_palette
  }
  
  data <- data.frame(
    color = my_colors,
    x = 1:length(my_colors)
  )
  
  palette_name <- deparse(substitute(my_palette))
  
  ggplot(data, aes(x = x, y = 1, fill = color)) + 
    geom_tile(show.legend = FALSE) + 
    scale_fill_identity() + 
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, size = 15, family = 'Poppins')) +
    ggtitle(palette_name)
}



# Display the final stacked plot
p <- plot_spacer() / create_palette_plot(LIBR_logo_sunset) / create_palette_plot(LIBR_logo_sky) / create_palette_plot(LIBR_logo_purple) / create_palette_plot(LIBR_logo_pink) / plot_spacer() / create_palette_plot(LIBR_gradient_purple) / create_palette_plot(LIBR_gradient_orange) / create_palette_plot(LIBR_gradient_pink) / create_palette_plot(LIBR_gradient_yellow) / create_palette_plot(LIBR_gradient_blue) / plot_spacer() / create_palette_plot(LIBR_theme_bright, is_continuous = F) / create_palette_plot(LIBR_theme_pastel, is_continuous = F) / create_palette_plot(LIBR_theme_dark, is_continuous = F) / plot_spacer() / create_palette_plot(LIBR_theme_bright_expanded, is_continuous = F) / create_palette_plot(LIBR_theme_pastel_expanded, is_continuous = F) / create_palette_plot(LIBR_theme_dark_expanded, is_continuous = F)

p

ggsave('LIBR_Theme.png', p, width = 1200, height = 1200, units = "px")
