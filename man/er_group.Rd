% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{er_group}
\alias{er_group}
\title{Group embedding vectors}
\usage{
er_group(
  embedding,
  method = "identity",
  threshold = 0.99,
  linkage = NULL,
  verbose = FALSE
)
}
\arguments{
\item{embedding}{a \code{numeric} matrix containing a text embedding.}

\item{method}{a \code{character} string specifying the grouping method. One of \code{c("identity","fuzzy")}. Default is \code{"identity"}.}

\item{threshold}{a \code{numeric} specifying the threshold for \code{method = "fuzzy"}. The threshold argument defines the quantile of the arccos similarity distribution that is used as the threshold for grouping embedding objects.}

\item{linkage}{a \code{character} specifying the linkage used for \code{method = "fuzzy"}. The linkage argument defines the criterion used to join clusters.}

\item{verbose}{a \code{logical} specifying whether to show messages.}
}
\value{
The function returns a \code{matrix} containing the grouped embedding. The \code{matrix} still has \code{ncol(embedding)} dimensions, but its rows have been reduced due to the grouping. With \code{method = "identity"}, the \code{matrix} gains the attribute \code{frequency} containing the frequency table of each element in the original \code{embedding}. With \code{method = "fuzzy"}, the \code{matrix} gains the new attributes \code{group_size}, which is analogue to \code{frequency}, \code{group_texts}, which contains the texts assigned to the group, and \code{group_min_sim}, which shows the minimum arccos similarity of texts in a group. Furthermore, with \code{method = "fuzzy"}, the \code{text} column will be replaced with generic group labels.
}
\description{
Function \code{er_group} condenses the embedding by grouping identical or highly similar objects (rows).
}
\examples{
\dontrun{
# get and group embedding vectors
embedding <- er_embed(neo$text) \%>\%
  er_group()
}
}
\references{
Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
}
