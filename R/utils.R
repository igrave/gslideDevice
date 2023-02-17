emu_to_pt <- function(emu) {
  emu / 12700
}

pt_to_emu <- function(pt) {
  pt * 12700
}



translate_matrix <- function(x, y){
  matrix(c(1,0,0,
           0,1,0,
           x, y,1),
         nrow=3, ncol=3)
}

rot_matrix <- function(deg) {
  r <- -deg * pi / 180
  scale_x = cos(r)
  scale_y = cos(r)
  shear_x = -sin(r)
  shear_y = sin(r)
  translate_x = 0
  translate_y = 0
  m <- matrix(
    c(
      scale_x, shear_y, 0,
      shear_x, scale_y, 0,
      translate_x, translate_y, 1
    ),
    nrow = 3,
    ncol = 3
  )
  round(m, 3)
}
