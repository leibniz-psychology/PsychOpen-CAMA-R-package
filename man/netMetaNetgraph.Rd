% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netMetaNetgraph.R
\name{netMetaNetgraph}
\alias{netMetaNetgraph}
\title{plot network graph}
\usage{
netMetaNetgraph(model, d)
}
\arguments{
\item{model}{A netmeta model object}

\item{d}{A \code{string} representing the dataset name that is used to extract the sequence and treatment parameter to be used of the netmeta::netgraph function}
}
\value{
returns a network graph for a given netmeta model.
}
\description{
Using the R package netmeta to to create and and return a network graph.
This indirect calling is needed since calling functions other than those from psychOpenCama package is blocked on the opencpu server for security reasons.
}
\author{
Robert Studtrucker
}
