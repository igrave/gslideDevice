add_to_slides <- function(before, options, envir) {
  if (before) return()

  paths <- unique(opts_knit$get("plot_files"))

  for (path in paths) {
    # get dimensions
    base64_string <- knitr::image_uri(path)
    slide_id <- add_slide()
    add_base64_shape(slide_id, base64_string)
    add_title(slide_id)
  }

}
