% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_objects.R
\name{TextContent}
\alias{TextContent}
\title{TextContent Object}
\usage{
TextContent(TextContent.lists = NULL, lists = NULL, textElements = NULL)
}
\arguments{
\item{TextContent.lists}{The \link{TextContent.lists} object or list of objects}

\item{lists}{The bulleted lists contained in this text, keyed by list ID}

\item{textElements}{The text contents broken down into its component parts, including styling information}
}
\value{
TextContent object
}
\description{
TextContent Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
The general text content. The text must reside in a compatible shape (e.g. text box or rectangle) or a table cell in a page.
}
\seealso{
Other TextContent functions: 
\code{\link{TextContent.lists}()}
}
\concept{TextContent functions}
