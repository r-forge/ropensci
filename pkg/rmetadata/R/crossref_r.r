#' Get a random set of DOI's through CrossRef.
#' 
#' From CrossRef website: "It [this API] might be useful to anybody doing research 
#'    on scholarly publications."
#' 
#' @import RJSONIO httr plyr
#' @param count The number of returned random DOIs. Maximum is 1000, default 20. 
#'    Note that a request for 1000 random DOIs will take a few seconds to 
#'    complete, whereas a request for 20 will take ~1 second.
#' @param to Return only DOIs published before and including the given year.
#' @param from Return only DOIs published after and including the given year. 
#'    Specifiying a long time period with to and from will cause multi-second 
#'    response times since the service will try to provide DOIs distributed 
#'    throughout the time period.
#' @param type Return DOIs of a certain type. Must be one of these unixref 
#'    derived types: journal_article, conference_paper, report_paper, 
#'    journal_issue, journal, book, book_series, book_set, dissertation, 
#'    content_item, series, or standard.
#' @param issn Return only journal article DOIs published in a journal with given 
#' 		ISSN. Provide ISSNs in the form xxxx-xxxx . However, sometimes this form 
#' 		won't match because of how metadata has been submitted to CrossRef. If it 
#' 		doesn't, try the form xxxxxxxx .
#' @return Ten DOI's in R's bibentry format.
#' @details From the Crossref documentation: A random distribution of values, 
#'    0 to 1, has been assigned to our DOI records. We use this as an index to 
#'    look up a random values between 0 and 1 on each request, and take a 
#'    series of DOIs from that point in randomised order. Selection on year, 
#'    title and ISSN filters out records before a lookup on the random index. 
#'    See \url{http://random.labs.crossref.org/} for more info on this 
#' 		Crossref API service.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @seealso \code{\link{crossref_search}}, \code{\link{crossref_citation}}, \code{\link{crossref_search_free}}
#' @export
#' @examples \dontrun{
#' # Default search gets 20 random DOIs
#' crossref_r()
#' 
#' # limit to certain dates
#' crossref_r(from=1990, to=1999)
#'  
#' # Specify you want journal articles only
#' crossref_r(type = 'journal_article')
#' }
crossref_r <- function(count = NULL, to = NULL, from = NULL, type = NULL, issn = NULL)
{
	url = "http://random.labs.crossref.org/dois"
	args <- compact(list(count = count, to = to, from = from, type = type, issn = issn))
	content(GET(url, query = args))
}