% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame.R
\name{er_frame}
\alias{er_frame}
\title{Build data frame}
\usage{
er_frame(embedding)
}
\arguments{
\item{embedding}{a \code{numeric} matrix containing a text embedding.}
}
\value{
The function returns a \code{tibble} containing the embedding and its attributes.
}
\description{
Function \code{er_frame} transforms the embedding and embedding attributes to a \link[tibble]{tibble} of class \code{embedR_tbl}.
}
\examples{
\dontrun{
# embed, project, and frame
tbl <- er_embed(neo$text) \%>\%
  er_project() \%>\%
  er_frame()
}
}
\references{
Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
}
