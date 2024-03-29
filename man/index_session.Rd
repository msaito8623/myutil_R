% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_session.R
\name{index_session}
\alias{index_session}
\title{Indexing each session in time-series data}
\usage{
index_session(bool.vec)
}
\arguments{
\item{bool.vec}{A boolean vector, which represents the beginnings of each time-series session, e.g. AR.start.}
}
\value{
A numeric vector in the same length as bool.vec, which stands for indices of time-series sessions.
}
\description{
Time-series data usually have a column telling the beginning of each time-series event, e.g. AR.start. Based on this column, the present function gives indices for each time-series event. So, simply, c(TRUE, FALSE, FALSE, TRUE, FALSE) is converted to c(1,1,1,2,2).
}
\examples{
vec = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
ind = index_session(vec)
print(data.frame('AR.start'=vec, 'AR.index'=ind))
}
\author{
Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
}
\keyword{utilities}
