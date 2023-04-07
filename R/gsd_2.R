new_id <- function(prefix = "ID") {
  paste0(
    prefix, "-",
    paste0(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = "")
  )
}


gsd_circle <- function(args, state) {
  x <- args$x
  y <- args$y
  r <- args$r
  rgba_fill <-  state$gc$fill / 255
  rgba_col <-  state$gc$col / 255


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

  fill <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(rgba_fill[1], rgba_fill[2], rgba_fill[3])),
    alpha = rgba_fill[4]
  )
  outline_col <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(rgba_col[1], rgba_col[2], rgba_col[3])),
    alpha = rgba_col[4]
  )

  update_style <- UpdateShapePropertiesRequest(
    objectId = circle_id,
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
  add_request(state, update_style)
  state
  }

gsd_rect <- function(args, state) {
  x0 <- args$x0
  y0 <- args$y0
  x1 <- args$x1
  y1 <- args$y1
  rgba_fill <-  state$gc$fill / 255
  rgba_col <-  state$gc$col / 255

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(x1 - x0, "PT"), height = Dimension(y0 - y1, "PT")),
    transform = AffineTransform(1, 1, 0, 0, x0, y1)
  )

  rect_id <- new_id("RECT")
  shape_request <- CreateShapeRequest(
    objectId = rect_id,
    shapeType = "RECTANGLE",
    elementProperties = slide_element
  )

  fill <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(rgba_fill[1], rgba_fill[2], rgba_fill[3])),
    alpha = rgba_fill[4]
  )
  outline_col <- SolidFill(
    color = OpaqueColor(rgbColor = RgbColor(rgba_col[1], rgba_col[2], rgba_col[3])),
    alpha = rgba_col[4]
    )

  update_style <- UpdateShapePropertiesRequest(
    objectId = rect_id,
    shapeProperties = ShapeProperties(
      shapeBackgroundFill = ShapeBackgroundFill(solidFill = fill),
      outline = Outline(outlineFill = OutlineFill(solidFill = outline_col))
    ),
    fields = paste(
      sep = ",",
      "shapeBackgroundFill.solidFill.color",
      "shapeBackgroundFill.solidFill.alpha"
    )
  )

  add_request(state, shape_request)
  add_request(state, update_style)
  # update_style$fields <- "outline.outlineFill.solidFill.color,outline.outlineFill.solidFill.alpha"
  # add_request(state, update_style)
  state
}

gsd_line <- function(args, state) {
  x1 <- args$x1
  y1 <- args$y1
  x2 <- args$x2
  y2 <- args$y2
  rgba_col <-  state$gc$col / 255

  if (x1 == x2 && y1 == y2) return()

  height <- abs(y2 - y1)
  width <- abs(x2 - x1)
  scale_x <- sign(x2 - x1)
  scale_y <- sign(y2 - y1)

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(width, "PT"), height = Dimension(height, "PT")),
    transform = AffineTransform(scale_x, scale_y, 0, 0, x1, y1)
  )

  line_id <- new_id("LINE")
  line_request <- CreateLineRequest(
    objectId = line_id,
    category = "LINE_CATEGORY_UNSPECIFIED",
    elementProperties = slide_element
  )

  line_update <- UpdateLinePropertiesRequest(
    objectId = line_id,
    lineProperties = LineProperties(
      lineFill = LineFill(
        solidFill = SolidFill(
          color = OpaqueColor(rgbColor = RgbColor(rgba_col[1], rgba_col[2], rgba_col[3])),
          alpha = rgba_col[4]
        ))
    ),
    # fields = "lineFill.solidFill.color.rgbColor(green,red,blue),lineFill.solidFill.alpha"
    fields = "lineFill.solidFill.color"
  )

  add_request(state, line_request)
  add_request(state, line_update)
  line_update[["fields"]] <- "lineFill.solidFill.alpha"
  add_request(state, line_update)
  state
}




gsd_polyline <- function(args, state) {
  x <- args$x
  y <- args$y
  n <- args$n
  rgba_col <-  state$gc$col / 255

  x = x[1:n]
  y = y[1:n]

  line_ids <- replicate(n-1, new_id("LINE"))
  for(i in seq_len(n-1)){
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[i+1]
    y2 <- y[i+1]

    height <- abs(y2 - y1)
    width <- abs(x2 - x1)
    scale_x <- sign(x2 - x1)
    scale_y <- sign(y2 - y1)


    slide_element <- PageElementProperties(
      pageObjectId = state$rdata$slidepage_id,
      size = Size(width = Dimension(width, "PT"), height = Dimension(height, "PT")),
      transform = AffineTransform(scale_x, scale_y, 0, 0, x1, y1)
    )

    line_request <- CreateLineRequest(
      objectId = line_ids[i],
      category = "LINE_CATEGORY_UNSPECIFIED",
      elementProperties = slide_element
    )

    line_update <- UpdateLinePropertiesRequest(
      objectId = line_ids[i],
      lineProperties = LineProperties(
        lineFill = LineFill(
          solidFill = SolidFill(
            color = OpaqueColor(rgbColor = RgbColor(rgba_col[1], rgba_col[2], rgba_col[3])),
            alpha = rgba_col[4]
          ))
      ),
      fields = "lineFill.solidFill.color"
    )

    add_request(state, line_request)
    add_request(state, line_update)
    line_update[["fields"]] <- "lineFill.solidFill.alpha"
    add_request(state, line_update)
  }

  if (length(line_ids) > 1) {
    group_req <- GroupObjectsRequest(groupObjectId = new_id("GROUP"), line_ids)
    add_request(state, group_req)
  }
  state
}


gsd_polygon <- function(args, state) {
  n <- args$n
  x <- c(args$x, args$x[1])
  y <- c(args$y, args$y[1])
  rgba <-  state$gc$fill / 255
  rgba_col <-  state$gc$col / 255

  # check for rectanges
  if (is_rectangle(args$x, args$y)) {
    state <- gsd_rect(list(x0 = x[1], y0 = y[1], x1 = x[3], y1 = y[3]),
             state)
    return(state)
  }

  triangles <- triangular::decompose(data.frame(x = x, y = y, group=1, subgroup=1))
  transforms <- lapply(split(triangles, triangles$idx), triangle_matrix)
  triangle_ids <- replicate(length(transforms), new_id("RIGHT_TRI"))

  for (i in seq_along(triangle_ids)) {
    affine_mat <- transforms[[i]]

    triangle_request <- CreateShapeRequest(
      objectId = triangle_ids[i],
      shapeType = "RIGHT_TRIANGLE",
      elementProperties = PageElementProperties(
        pageObjectId = state$rdata$slidepage_id,
        size = Size(width = Dimension(1, "PT"), height = Dimension(1, "PT")),
        transform = AffineTransform(
          scaleX = affine_mat[1, 1],
          scaleY = affine_mat[2, 2],
          shearX = affine_mat[1, 2],
          shearY = affine_mat[2, 1],
          translateX = affine_mat[1, 3],
          translateY = affine_mat[2, 3]
        )
      )
    )
    fill <- SolidFill(
      color = OpaqueColor(rgbColor = RgbColor(rgba[1], rgba[2], rgba[3])),
      alpha = rgba[4]
    )
    # outline_col <- Outline(outlineFill = OutlineFill(solidFill = SolidFill(alpha = 0)))
    # outline_col <- Outline(outlineFill = OutlineFill(solidFill = fill),
                           # weight = Dimension(magnitude = 1, unit = "EMU"))
    outline_col <- Outline(propertyState = "NOT_RENDERED")

    update_style <- UpdateShapePropertiesRequest(
      objectId = triangle_ids[i],
      shapeProperties = ShapeProperties(
        shapeBackgroundFill = ShapeBackgroundFill(solidFill = fill),
        outline = outline_col
        ),
      fields = "shapeBackgroundFill.solidFill.color,shapeBackgroundFill.solidFill.alpha,outline.propertyState"

      # fields = "shapeBackgroundFill.solidFill.color,shapeBackgroundFill.solidFill.alpha,outline.outlineFill.solidFill.alpha,outline.outlineFill.solidFill.color,outline.weight"
    )
    add_request(state, triangle_request)
    add_request(state, update_style)
  }
  if (length(triangle_ids) > 1) {
    group_req <- GroupObjectsRequest(groupObjectId = new_id("GROUP"), triangle_ids)
    add_request(state, group_req)
  }

  line_ids <- replicate(n, new_id("LINE"))
  for(i in seq_len(n)){
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[i+1]
    y2 <- y[i+1]

    height <- abs(y2 - y1)
    width <- abs(x2 - x1)
    scale_x <- sign(x2 - x1)
    scale_y <- sign(y2 - y1)
    slide_element <- PageElementProperties(
      pageObjectId = state$rdata$slidepage_id,
      size = Size(width = Dimension(width, "PT"), height = Dimension(height, "PT")),
      transform = AffineTransform(scale_x, scale_y, 0, 0, x1, y1)
    )

    line_request <- CreateLineRequest(
      objectId = line_ids[i],
      category = "LINE_CATEGORY_UNSPECIFIED",
      elementProperties = slide_element
    )

    line_update <- UpdateLinePropertiesRequest(
      objectId = line_ids[i],
      lineProperties = LineProperties(
        lineFill = LineFill(
          solidFill = SolidFill(
            color = OpaqueColor(rgbColor = RgbColor(rgba_col[1], rgba_col[2], rgba_col[3])),
            alpha = rgba_col[4]
        ))
      ),
      fields = "lineFill.solidFill.color"
    )

    add_request(state, line_request)
    add_request(state, line_update)
    line_update[["fields"]] <- "lineFill.solidFill.alpha"
    add_request(state, line_update)

  }

  if (length(line_ids) > 1) {
    group_req <- GroupObjectsRequest(groupObjectId = new_id("GROUP"), line_ids)
    add_request(state, group_req)
  }
  state
}

gsd_textUTF8 <- function(args, state) {
  x <- args$x
  y <- args$y
  str <- args$str
  rot <- args$rot
  hadj <- args$hadj
  extents <- text_util(args, state)
  padding <- 7.25
  width <- extents[1] + 2*padding
  height <- extents[2] + 2*padding

  affine_mat <-
    translate_matrix(x, y) %*%
    rot_matrix(rot) %*%
    translate_matrix(- padding - extents[1] * hadj, - 6 - extents[2])

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(width, "PT"), height = Dimension(height, "PT")),
    transform = AffineTransform(
      scaleX = affine_mat[1, 1],
      scaleY = affine_mat[2, 2],
      shearX = affine_mat[1, 2],
      shearY = affine_mat[2, 1],
      translateX = affine_mat[1, 3],
      translateY = affine_mat[2, 3]
    )
  )

  text_box_id <- new_id("TEXTBOX")
  text_box_request <- CreateShapeRequest(
    objectId = text_box_id,
    shapeType = "TEXT_BOX",
    elementProperties = slide_element
  )
  add_request(state, text_box_request)

  text_request <-
    InsertTextRequest(
      insertionIndex = 0,
      objectId = text_box_id,
      text = str
    )
  add_request(state, text_request)


  text_style_request <- UpdateTextStyleRequest(
    objectId = text_box_id,
    style = TextStyle(fontSize = Dimension(
      magnitude = state$gc$cex * state$gc$ps,
      unit = "PT"
    )),
    textRange = Range(type = "ALL"),
    fields = "fontSize"
  )
  add_request(state, text_style_request)

  if (hadj >= 0.5) {
    text_alignment <- if (hadj == 0.5) {
      "CENTER"
    } else if (hadj > 0.5) {
      "END"
    }
    update_style <- UpdateParagraphStyleRequest(
      objectId = text_box_id,
      style = ParagraphStyle(alignment = text_alignment),
      textRange = Range(type = "ALL"),
      fields = "alignment"
    )
    add_request(state, update_style)
  }
  state
}


gsd_raster <- function(args, state) {
  # raster_matrix <- matrix(rev(args$raster), nrow = args$w, ncol = args$h)
  raster_matrix <- matrix(args$raster, nrow = args$w, ncol = args$h)
  # args$w
  # args$h
  x <- args$x
  y <- args$y
  args$width
  args$height
  args$rot
  raster_ids <- matrix(replicate(args$h * args$w, new_id("RASTER")), nrow = args$h)

  rect_w <- args$width / args$w
  rect_h <- abs(args$height / args$h)
  for (i in seq_len(args$h)) {
    for (j in seq_len(args$w)) {
      rect_x <- x + (j - 1) * rect_w
      # rect_y <- y - (i) * rect_h
      rect_y <- y + args$height + (i-1) * rect_h
      rgba_fill <- col2rgb(paste0("#", as.hexmode(raster_matrix[j, i])), TRUE)[4:1, 1]/255

      slide_element <- PageElementProperties(
        pageObjectId = state$rdata$slidepage_id,
        size = Size(width = Dimension(rect_w, "PT"), height = Dimension(rect_h, "PT")),
        transform = AffineTransform(1, 1, 0, 0, rect_x, rect_y)
      )

      shape_request <- CreateShapeRequest(
        objectId = raster_ids[i, j],
        shapeType = "RECTANGLE",
        elementProperties = slide_element
      )

      fill <- SolidFill(
        color = OpaqueColor(rgbColor = RgbColor(rgba_fill[1], rgba_fill[2], rgba_fill[3])),
        alpha = rgba_fill[4]
      )
      outline_col <- Outline(propertyState = "NOT_RENDERED")

      update_style <- UpdateShapePropertiesRequest(
        objectId = raster_ids[i, j],
        shapeProperties = ShapeProperties(
          shapeBackgroundFill = ShapeBackgroundFill(solidFill = fill),
          outline = outline_col
        ),
        fields = paste(
          sep = ",",
          "shapeBackgroundFill.solidFill.color",
          "shapeBackgroundFill.solidFill.alpha",
          "outline.propertyState"
        )
      )
      add_request(state, shape_request)
      add_request(state, update_style)
    }
  }
  state
}





gsd_holdflush <- function(args, state) {
  print(paste0("level: ", args$level))
  if (args$level <= 0) {
    reqs <- do.call(Request, state$rdata$batch_requests)
    presentations.batchUpdate(
      presentationId = state$rdata$slides_id,
      BatchUpdatePresentationRequest = BatchUpdatePresentationRequest(requests = reqs)
    )
    state$rdata$batch_requests <- NULL
  }
  state
}

gsd_newPage <- function(args, state) {
  layout <- state$rdata$layout
  req <- CreateSlideRequest(slideLayoutReference = LayoutReference(predefinedLayout = layout))
  req <- rm_null_objs(req)
  res <- send_request(req, state)
  # request <- add_create_slide_page_request(predefined_layout = "BLANK")
  # res <- commit_to_slides(state$rdata$slides_id, request)
  state$rdata$slidepage_id <- res$replies[[1]]$createSlide$objectId

  state
}


gsd_open <- function(args, state) {
  if(is.null(state$rdata$slides_id)) {
    state$rdata$slides_id <- create_slides("gslideDevice")
  }
  if(is.null(state$rdata$layout)) {
    state$rdata$layout <- "BLANK"
  }

  state$dd$left <- 0
  state$dd$top <- 0
  state$dd$right <- 720
  state$dd$bottom <- 405
  state$dd$canClip <- FALSE
  state$dd$canHAdj <- 2L
  state$dd$haveRaster <- 1L
  state$dd$ipr <- c(1/72, 1/72)
  state
}

gsd_strWidthUTF8 <- function(args, state) {
   dpi      <- 1/state$dd$ipr[1]
  fontsize <- state$gc$cex * state$gc$ps * dpi/72

  metrics <- gdtools::str_metrics(
    args$str,
    fontname = "sans",
    fontsize = fontsize,
    bold     = FALSE,
    italic   = FALSE,
    fontfile = ""
  )

  state$width <- metrics[['width']]

  state
}


# see https://github.com/coolbutuseless/devoutrgl/blob/a861efc382053144a15e746b5b8e1b3b4b2e8879/R/rgl-text.R#L43
gsd_metricInfo <- function(args, state) {
  dpi      <- 1/state$dd$ipr[1]
  fontsize <- state$gc$cex * state$gc$ps * dpi/72

  cint <- abs(args$c)
  str  <- intToUtf8(cint)

  metrics  <- gdtools::str_metrics(
    str,
    fontname = "sans",
    fontsize = fontsize,
    bold     = FALSE,
    italic   = FALSE,
    fontfile = ""
  )

  state$ascent  <-  metrics[['ascent' ]]
  state$descent <-  metrics[['descent']]
  state$width   <-  metrics[['width'  ]]

  state
}



gsd_function <- function(device_call, args, state) {
  # if (device_call %in% c('mode', 'strWidthUTF8', 'metricInfo')) return()
  if (device_call %in% c('mode', 'strWidthUTF8')) return()

  cat("[", device_call, "]\n")
  # cat(paste(names(args), args, sep = " = ", collapse = ",  "), "\n", sep = "")

  call_string <- paste(
    "[", device_call, "]: ",
    paste(names(args), args, sep = " = ", collapse = ",  ")
  )
  tryCatch({
    state <- switch(
      device_call,
      "strWidthUTF8" = gsd_strWidthUTF8(args, state),
      "metricInfo" = gsd_metricInfo(args, state),
      "line"  = gsd_line (args, state),
      "polyline" = gsd_polyline(args, state),
      "circle" = gsd_circle(args, state),
      "rect" = gsd_rect(args, state),
      "polygon" = gsd_polygon(args, state),
      "open" = gsd_open(args, state),
      "newPage" = gsd_newPage(args, state),
      "textUTF8" = gsd_textUTF8(args, state),
      "raster" = gsd_raster(args, state),
      "holdflush" = gsd_holdflush(args, state),
      state
      )
  },
  error = function(e) {
    cat("Error in call:\n")
    cat(call_string, "\n")
    print(e)
  }
  )
  state
}


#' Title
#'
#' @param device_call
#' @param args
#' @param state
#'
#' @return
#' @export
#' @import devout
#' @examples
gslideDevice <- function(slides_id, layout,  ...) {
  rdevice(gsd_function, slides_id = slides_id, layout = layout, ...)
}
