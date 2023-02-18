
library(devout)
library(rgoogleslides)
# remotes::install_github("igrave/rgoogleslides")
authorize(
  client_id = "1073903696751-gr5k1k39e97ufjl5ssklhqp9pv787rfo.apps.googleusercontent.com",
  client_secret = "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"
)

a <- create_slides("Compare")
request_rgs <- rgoogleslides::add_create_slide_page_request()
commit_to_slides(a, request)


rdevice(debug_gsd_function, slides_id = "1k38RIlrzaKx2ASie9gzLl6zvZ6bnAGnNz6amY_WcS8w")

plot(1:3)


list("createSlide" = CreateSlideRequest(slideLayoutReference = slideLa  "BLANK"))
