create.sticker <- function(){
  hexSticker::sticker("SEQgraphic.png", package = "SEQuential",
                      s_width = .5,
                      s_height = .5,
                      s_x = 1,
                      s_y = .75,
                      p_size = 19,
                      p_color = "maroon",
                      h_fill = "white",
                      h_color = "darkgray")
  return(0)
}
