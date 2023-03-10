% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_objects.R
\name{CropProperties}
\alias{CropProperties}
\title{CropProperties Object}
\usage{
CropProperties(
  rightOffset = NULL,
  leftOffset = NULL,
  bottomOffset = NULL,
  angle = NULL,
  topOffset = NULL
)
}
\arguments{
\item{rightOffset}{The offset specifies the right edge of the crop rectangle that is located to the left of the original bounding rectangle right edge, relative to the object's original width}

\item{leftOffset}{The offset specifies the left edge of the crop rectangle that is located to the right of the original bounding rectangle left edge, relative to the object's original width}

\item{bottomOffset}{The offset specifies the bottom edge of the crop rectangle that is located above the original bounding rectangle bottom edge, relative to the object's original height}

\item{angle}{The rotation angle of the crop window around its center, in radians}

\item{topOffset}{The offset specifies the top edge of the crop rectangle that is located below the original bounding rectangle top edge, relative to the object's original height}
}
\value{
CropProperties object
}
\description{
CropProperties Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
The crop properties of an object enclosed in a container. For example, an Image. The crop properties is represented by the offsets of four edges which define a crop rectangle. The offsets are measured in percentage from the corresponding edges of the object's original bounding rectangle towards inside, relative to the object's original dimensions. - If the offset is in the interval (0, 1), the corresponding edge of crop rectangle is positioned inside of the object's original bounding rectangle. - If the offset is negative or greater than 1, the corresponding edge of crop rectangle is positioned outside of the object's original bounding rectangle. - If the left edge of the crop rectangle is on the right side of its right edge, the object will be flipped horizontally. - If the top edge of the crop rectangle is below its bottom edge, the object will be flipped vertically. - If all offsets and rotation angle is 0, the object is not cropped. After cropping, the content in the crop rectangle will be stretched to fit its container.
}
\concept{CropProperties functions}
