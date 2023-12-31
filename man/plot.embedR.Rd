% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.embedR}
\alias{plot.embedR}
\title{Plot embedding}
\usage{
\method{plot}{embedR}(embedding, ...)
}
\arguments{
\item{embedding}{a \code{numeric} matrix containing a text embedding.}

\item{...}{further arguments passed to \link[ggplot2]{geom_point}.}
}
\value{
The function returns a \code{ggplot2} object.
}
\description{
Generic function \code{plot} shows the points in a two-dimensional embedding or projection space with options for customization.
}
\details{
The function wraps around \link[emebedR]{er_frame} and \link[embedR]{plot.embedR_tbl}.
}
\examples{
\dontrun{
# get embedding plot
neo$text \%>\%
  er_embed() \%>\%
  er_project() \%>\%
  plot()
}
}
\references{
Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
}
