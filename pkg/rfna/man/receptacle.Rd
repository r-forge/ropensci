\name{receptacle}
\alias{receptacle}
\title{Get state (paleate or epaleate) of receptacle.}
\usage{
  receptacle(url)
}
\arguments{
  \item{url}{The URL of the page you want to search.}
}
\value{
  paleate, epaleate, or 'not found'.
}
\description{
  Get state (paleate or epaleate) of receptacle.
}
\examples{
\dontrun{
url <- 'http://www.efloras.org/florataxon.aspx?flora_id=1&taxon_id=102552'
receptacle(url)
url <- 'http://www.efloras.org/florataxon.aspx?flora_id=1&taxon_id=250066099'
receptacle(url)
pg1<-'http://www.efloras.org/browse.aspx?flora_id=1&start_taxon_id=10074&page=1'
urls <- getdaughterURLs(pg1)
ldply(urls[1:5], receptacle, .progress='text')
}
}

