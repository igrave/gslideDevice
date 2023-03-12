# remotes::install_github("coolbutuseless/triangular")
#
# decom <-
# triangular::decompose(data.frame(x=c(1,1,2,2,1), y=c(1,3,4,5,1), group=1, subgroup=1))
#
# by(decom, decom$idx, function(df){
#   triangle_matrix(matrix(c(df$x, df$y), byrow = TRUE, nrow = 2))
#   })
# lapply(decomp$idx)
# triangle_matrix(lapply(decomp) {
#
# })
#
#
#   x0 <- 0
#   y0 <- 0
#   x1 <- 1
#   y1 <- 1
#
#   slide_element <- PageElementProperties(
#     pageObjectId = "SLIDES_API557451271_0",
#     size = Size(width = Dimension(1, "PT"), height = Dimension(1, "PT")),
#     transform = AffineTransform(1, 1, 0, 0, 0, 0)
#   )
#
#   shape_request <- CreateShapeRequest(
#     objectId = new_id("TRI"),
#     shapeType = "RIGHT_TRIANGLE",
#     elementProperties = slide_element
#   )
#
#   send_request(request = shape_request, state = list(rdata = list(slides_id = "1k38RIlrzaKx2ASie9gzLl6zvZ6bnAGnNz6amY_WcS8w")))
#
#
#   # RIGHT_TRIANGLE has points
#   matrix(c(
#     0, 0,
#     0, 1,
#     1, 1
#   ), nrow = 2)

  triangle_matrix <- function(df) {
    x <- df$x
    y <- df$y
    a13 <- x[1]
    a12 <- x[2] - x[1]
    a11 <- x[3] - x[2]
    a21 <- y[3] - y[2]
    a22 <- y[2] - y[1]
    a23 <- y[1]

    matrix(
      c(a11, a21, 0, a12, a22, 0, a13, a23, 1),
      nrow = 3
    )
  }
