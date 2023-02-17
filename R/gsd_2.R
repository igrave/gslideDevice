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
  x1 <- x - r
  x2 <- x + r
  y1 <- y - r
  y2 <- y + r

  slide_element <- page_element_property(state$rdata$slidepage_id, x2 - x1, y2 - y1, 1, 1, 0, 0, x1, y1)
  request <- add_create_shape_request(shape_type = "ELLIPSE", page_element_property = slide_element)
  commit_to_slides(state$rdata$slides_id, request)
  state
}

gsd_rect <- function(args, state) {
  x1 <- args$x1
  y1 <- args$y1
  x2 <- args$x2
  y2 <- args$y2
  slide_element <- page_element_property(state$rdata$slidepage_id, x2 - x1, y2 - y1, 1, 1, 0, 0, x1, y1)
  request <- add_create_shape_request(shape_type = "RECTANGLE", page_element_property = slide_element)
  commit_to_slides(state$rdata$slides_id, request)
  state
}

gsd_line <- function(args, state) {
  x1 <- args$x1
  y1 <- args$y1
  x2 <- args$x2
  y2 <- args$y2

  if (x1 == x2 && y1 == y2) return()
  # browser()
  height <- abs(y2 - y1)
  width <- abs(x2 - x1)
  scale_x <- sign(x2 - x1)
  scale_y <- sign(y2 - y1)
  slide_element <- page_element_property(state$rdata$slidepage_id, width, height, scale_x, scale_y, 0, 0, x1, y1)
  request <- add_create_line_request(page_element_property = slide_element)
  commit_to_slides(state$rdata$slides_id, request)
  state
}




gsd_polyline <- function(args, state) {
  x <- args$x
  y <- args$y
  n <- args$n

  x = x[1:n]
  y = y[1:n]
  request <-  NULL

  for(i in seq_len(n-1)){
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[i+1]
    y2 <- y[i+1]

    height <- abs(y2 - y1)
    width <- abs(x2 - x1)
    scale_x <- sign(x2 - x1)
    scale_y <- sign(y2 - y1)
    slide_element <- page_element_property(state$rdata$slidepage_id, width, height, scale_x, scale_y, 0, 0, x1, y1)
    request <- add_create_line_request(request, page_element_property = slide_element)
  }
  # browser()
  response <- commit_to_slides(state$rdata$slides_id, request)
  group_request <- group_objects_request(object_ids = response$replies$createLine$objectId)
  commit_to_slides(state$rdata$slides_id, group_request)
  state
}


gsd_polygon <- function(args, state) {
  n <- args$n
  x <- c(args$x, args$x[1])
  y <- c(args$y, args$y[1])

  request <-  NULL

  for(i in seq_len(n)){
    x1 <- x[i]
    y1 <- y[i]
    x2 <- x[i+1]
    y2 <- y[i+1]

    height <- abs(y2 - y1)
    width <- abs(x2 - x1)
    scale_x <- sign(x2 - x1)
    scale_y <- sign(y2 - y1)
    slide_element <- page_element_property(state$rdata$slidepage_id, width, height, scale_x, scale_y, 0, 0, x1, y1)
    request <- add_create_line_request(request, page_element_property = slide_element)
  }
  # browser()
  response <- commit_to_slides(state$rdata$slides_id, request)
  group_request <- group_objects_request(object_ids = response$replies$createLine$objectId)
  commit_to_slides(state$rdata$slides_id, group_request)
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
  slide_element <- page_element_property(
    state$rdata$slidepage_id,
    width_magnitude = width,
    height_magnitude = height,
    scale_x = affine_mat[1, 1],
    scale_y = affine_mat[2, 2],
    shear_x = affine_mat[1, 2],
    shear_y = affine_mat[2, 1],
    translate_x = affine_mat[1, 3],
    translate_y = affine_mat[2, 3]
    )

  text_box_id <- new_id("TEXTBOX")
  request <- add_create_shape_request(
    shape_type = "TEXT_BOX",
    page_element_property = slide_element,
    object_id = text_box_id)
  request <- add_insert_text_request(
    request,
    object_id = text_box_id,
    text = str,
  )
  commit_to_slides(state$rdata$slides_id, request)

  args$r <- 1
  gsd_circle(args, state)

  state
}

gsd_newPage <- function(args, state) {
  request <- add_create_slide_page_request(predefined_layout = "BLANK")
  res <- commit_to_slides(state$rdata$slides_id, request)
  state$rdata$slidepage_id <- res$replies$createSlide$objectId

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




# invisible(dev.off())
