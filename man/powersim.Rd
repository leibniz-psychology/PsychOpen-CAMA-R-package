% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powersim.R
\name{powersim}
\alias{powersim}
\title{Power simulation}
\usage{
powersim(yi, vi, measure, d, n, pval = 0.05)
}
\arguments{
\item{yi}{A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)}

\item{vi}{A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)}

\item{measure}{A character string indicating underlying summary measure. Depending on the type of measure the input data for fitting needs to be transformed and backtransformed later.}

\item{d}{A \code{string} representing the dataset name that should be used for fitting.}

\item{n}{A \code{Integer} representing the sample size which should be used for the power simulation.}

\item{pval}{A \code{Numeric} representing the significance level which should be used for the power simulation.}
}
\value{
returns a power plot
}
\description{
Using the R packages metafor and pwr to calculate a powersimulation for a given dataset and specified parameters. The results are plotted using ggplot2.
}
\author{
Robert Studtrucker
}
