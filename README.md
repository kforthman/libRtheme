# libRtheme

```r
install.packages("devtools")
devtools::install_github("kforthman/libRtheme")
library(libRtheme)
display.libr.all()
```

This package makes it easy to grab color themes inspired by the LIBR logo. To return a vector of colors in Hex form, call the function `libr.pal(n, name)`, where `n` is the number of colors you want returned and `name` is the name of the palette (see below graphic for options). Loading this library also makes available the font "Poppins", one of the fonts used on the LIBR website, which can be added to your plot using the argument `family = "Poppins"`.

Logos can be found in eps, pdf, and png format in the folder 'logos'.

Enjoy! Suggestions welcome, please add an issue or email me at kforthman@laureateinstitute.org.

![Alt text](LIBR_Theme.png)

![Alt text](logos/LIBR_full_color_logo.png)
