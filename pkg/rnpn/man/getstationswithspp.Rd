\name{getstationswithspp}
\alias{getstationswithspp}
\title{Get a list of all stations which have an individual whom is a member of a
   set of species.}
\usage{
  getstationswithspp(speciesid = NA, printdf = TRUE,
    callopts = list())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

  \item{printdf}{print data.frame (default, TRUE) or not
  (FALSE)}

  \item{callopts}{Optional additional curl options
  (debugging tools mostly)}
}
\value{
  Stations' latitude and longitude, names, and ids.
}
\description{
  Get a list of all stations which have an individual whom
  is a member of a set of species.
}
\examples{
\dontrun{
getstationswithspp(c(52,53,54))
getstationswithspp(c(52,53), printdf = FALSE)
}
}

