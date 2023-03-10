% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_objects.R
\name{ReplaceAllTextRequest}
\alias{ReplaceAllTextRequest}
\title{ReplaceAllTextRequest Object}
\usage{
ReplaceAllTextRequest(
  replaceText = NULL,
  containsText = NULL,
  pageObjectIds = NULL
)
}
\arguments{
\item{replaceText}{The text that will replace the matched text}

\item{containsText}{Finds text in a shape matching this substring}

\item{pageObjectIds}{If non-empty, limits the matches to page elements only on the given pages}
}
\value{
ReplaceAllTextRequest object
}
\description{
ReplaceAllTextRequest Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
Replaces all instances of text matching a criteria with replace text.
}
\concept{ReplaceAllTextRequest functions}
