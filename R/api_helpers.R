draw_circle <- function(x, y, state) {
  r <- 1
  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(2*r, "PT"), height = Dimension(2*r, "PT")),
    transform = AffineTransform(1, 1, 0, 0, x - r, y - r)
  )

  circle_id <- new_id("CIRCLE")
  shape_request <- CreateShapeRequest(
    objectId = circle_id,
    shapeType = "ELLIPSE",
    elementProperties = slide_element
  )
  add_request(state, shape_request)
}

rectangle_requests <- function(state, id, x0, x1, y0, y1, col, fill) {

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(x1 - x0, "PT"), height = Dimension(y0 - y1, "PT")),
    transform = AffineTransform(1, 1, 0, 0, x0, y1)
  )

  shape_request <- CreateShapeRequest(
    objectId = id,
    shapeType = "RECTANGLE",
    elementProperties = slide_element
  )

  fill <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(fill[1], fill[2], fill[3])),
    alpha = fill[4]
  )
  outline_col <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(col[1], col[2], col[3])),
    alpha = col[4]
  )

  update_style <- UpdateShapePropertiesRequest(
    objectId = id,
    shapeProperties = ShapeProperties(
      shapeBackgroundFill = ShapeBackgroundFill(solidFill = fill),
      outline = Outline(outlineFill = OutlineFill(solidFill = outline_col))
    ),
    fields = paste(
      sep = ",",
      "shapeBackgroundFill.solidFill.color",
      "shapeBackgroundFill.solidFill.alpha",
      "outline.outlineFill.solidFill.color",
      "outline.outlineFill.solidFill.alpha"
    )
  )

  add_request(state, shape_request)
  # add_request(state, update_style)
  state
}
