
library(devout)
# library(rgoogleslides)
# remotes::install_github("igrave/rgoogleslides")
# authorize(
  # client_id = "1073903696751-gr5k1k39e97ufjl5ssklhqp9pv787rfo.apps.googleusercontent.com",
  # client_secret = "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"
# )

# a <- create_slides("Compare")
# request_rgs <- rgoogleslides::add_create_slide_page_request()
# commit_to_slides(a, request)

gsd_auth("isaac.gravestock@gmail.com")
# "SLIDES_API519334291_15"
# rdevice(debug_gsd_function, slides_id = "1ha1lRjRlKQUH_G7vRnHdaT5MAz4QTWdc7pT_sAeRmKU", layout = "TITLE") #roche

rdevice(debug_gsd_function, slides_id = "1k38RIlrzaKx2ASie9gzLl6zvZ6bnAGnNz6amY_WcS8w")

gsd_auth("isaac.gravestock@gmail.com")
plot(1:4)
plot(1:3, xaxt = 'n', yaxt = 'n')
rect(1, 1, 2, 2, col = 3)
pie(1:4)


slide_element <- page_element_property("p", 4, 4, 1, 1, 0, 0, 5, 5)
request <- add_create_shape_request(shape_type = "RECTANGLE", page_element_property = slide_element)
request <- add_create_shape_request(request, shape_type = "RECTANGLE", page_element_property = slide_element)

commit_to_slides("1k38RIlrzaKx2ASie9gzLl6zvZ6bnAGnNz6amY_WcS8w", request)

req <- add_create_shape_request()

library(ggplot2)

ggplot(iris[1:10,], aes(Sepal.Width, Sepal.Length))  + geom_smooth(color = "blue", fill = "red")+ geom_point()
grDevices::dev.flush()
debug(gsd_polygon)

m <- ggplot(economics[1:20,], aes(unemploy/pop, psavert))
m + geom_path(aes(colour = as.numeric(date)))
grDevices::dev.flush()

plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
image <- as.raster(matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5))
rasterImage(image, 100, 300, 150, 350, interpolate = FALSE)
grDevices::dev.flush()
debug(gsd_raster)
