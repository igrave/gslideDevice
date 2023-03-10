% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_objects.R
\name{GroupObjectsRequest}
\alias{GroupObjectsRequest}
\title{GroupObjectsRequest Object}
\usage{
GroupObjectsRequest(groupObjectId = NULL, childrenObjectIds = NULL)
}
\arguments{
\item{groupObjectId}{A user-supplied object ID for the group to be created}

\item{childrenObjectIds}{The object IDs of the objects to group}
}
\value{
GroupObjectsRequest object
}
\description{
GroupObjectsRequest Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
Groups objects to create an object group. For example, groups PageElements to create a Group on the same page as all the children.
}
\concept{GroupObjectsRequest functions}
