\name{crossref_search_free}
\alias{crossref_search_free}
\title{Search the CrossRef Metatdata for DOIs using free form references.}
\usage{
  crossref_search_free(query,
    url = "http://search.labs.crossref.org/links")
}
\arguments{
  \item{query}{Reference query; a character vector of
  length 1 or greater, comma-separated of course.}

  \item{url}{Base url for the Crossref metadata API.}
}
\description{
  Search the CrossRef Metatdata for DOIs using free form
  references.
}
\details{
  Have to have at least three terms in each search query.
}
\examples{
\dontrun{
# search with title, author, year, and journal
crossref_search_free(query = "Piwowar Sharing Detailed Research Data Is Associated with Increased Citation Rate PLOS one 2007")

crossref_search_free(query="Renear 2012") # too few words, need at least 3

crossref_search_free(query=c("Renear 2012","Piwowar sharing data PLOS one")) # multiple queries

# Get a DOI and get the citation using crossref_search
doi <- crossref_search_free(query="Piwowar sharing data PLOS one")$doi
crossref_search(doi = doi)

# Queries can be quite complex too
crossref_search_free("M. Henrion, D. J. Mortlock, D. J. Hand, and A. Gandy, \\"A Bayesian approach to star-galaxy classification,\\" Monthly Notices of the Royal Astronomical Society, vol. 412, no. 4, pp. 2286-2302, Apr. 2011.")

# Lots of queries
queries <- c("Piwowar sharing data PLOS one",
						 "Priem Scientometrics 2.0 social web",
						 "William Gunn A Crosstalk Between Myeloma Cells",
						 "karthik ram Metapopulation dynamics override local limits")
crossref_search_free(queries)
}
}
\author{
  Scott Chamberlain \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link{crossref_search}}, \code{\link{crossref_r}},
  \code{\link{crossref_citation}}
}

