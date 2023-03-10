% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_objects.R
\name{InsertTextRequest}
\alias{InsertTextRequest}
\title{InsertTextRequest Object}
\usage{
InsertTextRequest(
  insertionIndex = NULL,
  objectId = NULL,
  text = NULL,
  cellLocation = NULL
)
}
\arguments{
\item{insertionIndex}{The index where the text will be inserted, in Unicode code units, based on TextElement indexes}

\item{objectId}{The object ID of the shape or table where the text will be inserted}

\item{text}{The text to be inserted}

\item{cellLocation}{The optional table cell location if the text is to be inserted into a table cell}
}
\value{
InsertTextRequest object
}
\description{
InsertTextRequest Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
Inserts text into a shape or a table cell.
}
\concept{InsertTextRequest functions}
