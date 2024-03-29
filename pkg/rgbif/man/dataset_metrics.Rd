\name{dataset_metrics}
\alias{dataset_metrics}
\title{Geta details on a dataset.}
\usage{
  dataset_metrics(uuid, callopts = list())
}
\arguments{
  \item{uuid}{A dataset UUID.}

  \item{callopts}{Pass on options to GET.}
}
\description{
  You should be able to pass in more than one uuid to this
  function, but I have not yet found more than the one uuid
  below in the example that actually has data available.
}
\examples{
\dontrun{
dataset_metrics(uuid='3f8a1297-3259-4700-91fc-acc4170b27ce')
}
}
\references{
  \url{http://dev.gbif.org/wiki/display/POR/Webservice+API}
}

