emu_to_pt <- function(emu) {
  emu / 12700
}

pt_to_emu <- function(pt) {
  pt * 12700
}

cm_to_pt <- function(cm) {
  cm / 2.54 * 72
}

pt_to_cm <- function(pt) {
  pt / 72 * 2.54
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


#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is_null_obj <- function(x) is.null(x) | all(sapply(x, is.null))
#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rm_null_objs <- function(x) {
  class_x <- class(x)
  x <- Filter(Negate(is_null_obj), x)
  x <- lapply(x, function(x) if (is.list(x)) rm_null_objs(x) else x)
  class(x) <- class_x
  x
}



next_req <- function(state) {
  length(state$rdata$batch_requests) + 1
}



text_util <- function(args, state) {
  dpi      <- 1/state$dd$ipr[1]
  fontsize <- state$gc$cex * state$gc$ps * dpi/72

  metrics <- gdtools::str_extents(
    args$str,
    fontname = "sans",
    fontsize = fontsize,
    bold     = FALSE,
    italic   = FALSE,
    fontfile = ""
  )
  metrics / c(1, 0.6)
}

is_rectangle <- function(x, y) {
  length(x) == 4 &&
  length(y) == 4 &&
  x[3] - x[1] == x[4] - x[2] &&
  y[3] - y[1] == y[2] - y[4] &&
  (x[2] - x[1]) * (x[2] - x[3]) + (y[2] - y[1]) * (y[2] - y[3]) == 0
}
