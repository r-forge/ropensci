\name{familySearch}
\alias{familySearch}
\title{A function to find all fish that are members of a scientific Family}
\usage{
  familySearch(family, fish.data = NULL)
}
\arguments{
  \item{family}{The scientific family name.  Can include
  grep matching, see examples}

  \item{fish.data}{the fishbase database or a subset}
}
\value{
  a logical vector of length(fish.data) indicating the
  matches.
}
\description{
  deprecrated, see \code{which_fish}
}
\details{
  The return value can be summed to give the number of
  matches, can be used as an index, e.g. fish.data[goby],
  to return the matches or to pass to another function.
  See examples.
}
\keyword{utilities}

