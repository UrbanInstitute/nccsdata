library(hexSticker)
library(ggplot2)
library(showtext)
library(sysfonts)

# Load Lato from Google Fonts
sysfonts::font_add_google("Lato", "Lato")
showtext::showtext_auto()

# Empty subplot
p <- ggplot() +
  theme_void() +
  theme(plot.background = element_blank(),
        panel.background = element_blank())

sticker(
  p,
  package = "nccsdata",
  p_size = 8,
  p_color = "#FFFFFF",
  p_family = "Lato",
  p_fontface = "bold",
  p_y = 1,
  s_x = 1,
  s_y = 1,
  s_width = 0.1,
  s_height = 0.1,
  h_fill = "#1696d2",
  h_color = "#FFFFFF",
  h_size = 1.5,
  filename = "hex/nccsdata.svg",
  dpi = 300
)
