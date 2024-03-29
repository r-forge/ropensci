\name{crossref_citation}
\alias{crossref_citation}
\title{Lookup article info via CrossRef with DOI and get a citation.}
\usage{
  crossref_citation(doi, title = FALSE,
    url = "http://www.crossref.org/openurl/",
    key = "cboettig@gmail.com", ...,
    curl = getCurlHandle())
}
\arguments{
  \item{doi}{digital object identifier for an article in
  PLoS Journals}

  \item{title}{return the title of the paper or not
  (defaults to FALSE)}

  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{key}{your PLoS API key, either enter, or loads from
  .Rprofile}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Metadata from DOI in R's bibentry format.
}
\description{
  Options to get formatted citations as bibtext or plain
  text.
}
\details{
  See \url{http://labs.crossref.org/openurl/} for more info
  on this Crossref API service.
}
\examples{
\dontrun{
crossref_citation(doi="10.1371/journal.pone.0042793")
print(crossref_citation("10.3998/3336451.0009.101"), style="Bibtex")
print(crossref_citation("10.3998/3336451.0009.101"), style="text")
}
}
\author{
  Carl Boettiger \email{cboettig@gmail.com}
}
\seealso{
  \code{\link{crossref_search}}, \code{\link{crossref_r}},
  \code{\link{crossref_search_free}}
}

