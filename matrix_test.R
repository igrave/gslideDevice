rot_matrix <- function(deg) {
  r <- deg * pi / 180
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


rot_matrix(0)
rot_matrix(90)

rot_matrix(0) %*% matrix(c(1, 1, 1), nrow = 3)
rot_matrix(90) %*% matrix(c(1, 1, 1), nrow = 3)
