% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selct.R
\name{selct}
\alias{selct}
\title{selct}
\usage{
selct(data, id, day)
}
\arguments{
\item{data}{dataframe containing subject-day level activity count}

\item{id}{id of the subject}

\item{day}{day number of the subject}
}
\value{
a vector containing daily activity record
}
\description{
This function selects a daily activity record given ID and day
}
\examples{
data(act)
selct(data = act ,id = 21034,day = 1)


}
\keyword{selct}

