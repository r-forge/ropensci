\name{ubio_id}
\alias{ubio_id}
\title{Search uBio by namebank ID.}
\usage{
  ubio_id(namebankID = NULL, keyCode = NULL,
    callopts = list())
}
\arguments{
  \item{searchName}{(string) - term to search within name
  string}

  \item{searchAuth}{(string) - term to search within name
  authorship}

  \item{searchYear}{(string) - term to search within name
  year}

  \item{order}{(string) - (name or namebankID) field by
  which the results will be sorted (default is namebankID)}

  \item{sci}{(int) - (sci, vern, or all) type of results to
  be returned (default is all)}

  \item{vern}{(int) - (limit 1000) maximum number of
  results to be returned (default is 1000)}

  \item{keyCode}{Your uBio API key; loads from .Rprofile.
  If you don't have one, obtain one at
  http://www.ubio.org/index.php?pagename=form.}
}
\value{
  A dataframe.
}
\description{
  Search uBio by namebank ID.
}
\details{
  Can't seem to get json format results along with
  specifiying an API key, so if you use json your key is
  not specified at the moment
}
\examples{
\dontrun{
ubio_id(namebankID = 2483153)
}
}

