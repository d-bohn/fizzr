# devtools::install_github('GuangchuangYu/hexSticker')
library(hexSticker)

img <- "~/Desktop/bubble-512.png"
sticker(img, package="fizzr",
        p_size=8, p_y = 1.6, p_color = 'white',
        s_x=1, s_y=.8, s_width=.6,
        h_fill = '#8A2BE2',
        h_color = c("#424242"),
        url = 'github.com/d-bohn/fizzr',
        filename= "~/Desktop/fizzr_sticker.png")
