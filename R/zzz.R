#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add
#' @import grDevices ggplot2 scales patchwork
.onLoad <- function(libname, pkgname) {
  font_path <- system.file("fonts", "poppins-v21-latin-500.ttf", package = pkgname)
  sysfonts::font_add("Poppins", font_path)
  showtext::showtext_auto()
}