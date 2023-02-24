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

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(2*r, "PT"), height = Dimension(2*r, "PT")),
    transform = AffineTransform(1, 1, 0, 0, x - r, y - r)
  )

  shape_request <- CreateShapeRequest(
    objectId = new_id("CIRCLE"),
    shapeType = "ELLIPSE",
    elementProperties = slide_element
  )

  i <- length(state$rdata$batch_requests)
  state$rdata$batch_requests[[i + 1]] <- shape_request
  state
}

gsd_rect <- function(args, state) {
  x0 <- args$x0
  y0 <- args$y0
  x1 <- args$x1
  y1 <- args$y1

  slide_element <- PageElementProperties(
    pageObjectId = state$rdata$slidepage_id,
    size = Size(width = Dimension(x1 - x0, "PT"), height = Dimension(y0 - y1, "PT")),
    transform = AffineTransform(1, 1, 0, 0, x0, y1)
  )

  shape_request <- CreateShapeRequest(
    objectId = new_id("RECT"),
    shapeType = "RECTANGLE",
    elementProperties = slide_element
  )

  i <- length(state$rdata$batch_requests)
  state$rdata$batch_requests[[i + 1]] <- shape_request

  state
}

gsd_line <- function(args, state) {
  x1 <- args$x1
  y1 <- args$y1
  x2 <- args$x2
  y2 <- args$y2

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

  shape_request <- CreateLineRequest(
    objectId = new_id("LINE"),
    category = "LINE_CATEGORY_UNSPECIFIED",
    elementProperties = slide_element
  )

  i <- length(state$rdata$batch_requests)
  state$rdata$batch_requests[[i + 1]] <- shape_request

  state
}




gsd_polyline <- function(args, state) {
  x <- args$x
  y <- args$y
  n <- args$n

  x = x[1:n]
  y = y[1:n]
  request <-  NULL

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

    i <- length(state$rdata$batch_requests)
    state$rdata$batch_requests[[i + 1]] <- line_request
  }

  if (length(line_ids) > 1) {
    group_req <- GroupObjectsRequest(groupObjectId = new_id("GROUP"), line_ids)
    i <- length(state$rdata$batch_requests)
    state$rdata$batch_requests[[i + 1]] <- group_req
  }
  state
}


gsd_polygon <- function(args, state) {
  n <- args$n
  x <- c(args$x, args$x[1])
  y <- c(args$y, args$y[1])

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

    i <- length(state$rdata$batch_requests)
    state$rdata$batch_requests[[i + 1]] <- line_request
  }

  if (length(line_ids) > 1) {
    group_req <- GroupObjectsRequest(groupObjectId = new_id("GROUP"), line_ids)
    i <- length(state$rdata$batch_requests)
    state$rdata$batch_requests[[i + 1]] <- group_req
  }
  state
}

gsd_textUTF8 <- function(args, state) {
  x <- args$x
  y <- args$y
  str <- args$str
  rot <- args$rot
  hadj <- args$hadj

  width <- nchar(str) * 15
  height <- 20
  affine_mat <- translate_matrix(x, y) %*%
    rot_matrix(rot) %*%
    translate_matrix(-width * hadj, -height)

  print(paste0("hadj: ", hadj))
  print(paste0("string: ", str))

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
  i <- length(state$rdata$batch_requests)
  state$rdata$batch_requests[[i + 1]] <- text_box_request

  text_request <-
    InsertTextRequest(
      insertionIndex = 0,
      objectId = text_box_id,
      text = str
    )
  state$rdata$batch_requests[[next_req(state)]] <- text_request

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
  req <- CreateSlideRequest(slideLayoutReference = LayoutReference(predefinedLayout = "BLANK"))
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

  state$dd$left <- 0
  state$dd$top <- 0
  state$dd$right <- 720
  state$dd$bottom <- 400
  state$dd$canClip <- FALSE
  state$dd$canHAdj <- 2L

  state
}


debug_gsd_function <- function(device_call, args, state) {
  if (device_call %in% c('mode', 'strWidthUTF8', 'metricInfo')) return()
  cat("[", device_call, "]\n")
  # cat(paste(names(args), args, sep = " = ", collapse = ",  "), "\n", sep = "")

  call_string <- paste(
    "[", device_call, "]: ",
    paste(names(args), args, sep = " = ", collapse = ",  ")
  )
  tryCatch({
    state <- switch(
      device_call,
      "line"  = gsd_line (args, state),
      "polyline" = gsd_polyline(args, state),
      "circle" = gsd_circle(args, state),
      "rect" = gsd_rect(args, state),
      "polygon" = gsd_polygon(args, state),
      "open" = gsd_open(args, state),
      "newPage" = gsd_newPage(args, state),
      "textUTF8" = gsd_textUTF8(args, state),
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

