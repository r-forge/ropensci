#' Search for ORCID ID's.
#' 
#' @import RCurl XML plyr RJSONIO
#' @param query Search terms. You can do quite complicated queries using the SOLR 
#' 		syntax. See examples below. For all possible fields to query, do data(fields).
#' @param start Result number to start on. Keep in mind that pages start at 0.
#' @param rows Numer of results to return.
#' @param recursive Keep drilling down until all records are retrieved for the 
#' 		given query, default FALSE (logical). If recursive=TRUE, rows and start
#' 		parameters are ignored. 
#' @param defType Query syntax. One of edismax or X. See Details for more.
#' @param qf List of fields and the "boosts" to associate with each of them when 
#' 		building DisMax and queries. 
#' @param q.alt If specified, this query will be used (and parsed by default using 
#' 		standard query parsing syntax) when the main query string is not specified 
#' 		or blank. This comes in handy when you need something like a match-all-docs 
#' 		query (don't forget &rows=0 for that one!) in order to get collection-wise 
#' 		faceting counts.
#' @param qf (Query Fields) List of fields and the "boosts" to associate with each 
#' 		of them when building DisjunctionMaxQueries from the user's query
#' @param mm (Minimum 'Should' Match) See the wiki here \link{http://wiki.apache.org/solr/ExtendedDisMax#mm_.28Minimum_.27Should.27_Match.29}
#' @param qs (Query Phrase Slop) Amount of slop on phrase queries explicitly 
#' 		included in the user's query string (in qf fields; affects matching).
#' @param pf (Phrase Fields) Once the list of matching documents has been identified 
#' 		using the "fq" and "qf" params, the "pf" param can be used to "boost" the 
#' 		score of documents in cases where all of the terms in the "q" param appear 
#' 		in close proximity. Read more here link{http://wiki.apache.org/solr/ExtendedDisMax#pf_.28Phrase_Fields.29}
#' @param ps (Phrase Slop) Default amount of slop on phrase queries built with "pf", 
#' 		"pf2" and/or "pf3" fields (affects boosting).
#' @param pf2 (Phrase bigram fields) As with 'pf' but chops the input into bi-grams, 
#' 		e.g. "the brown fox jumped" is queried as "the brown" "brown fox" "fox jumped"
#' @param ps2 (Phrase bigram slop) As with 'ps' but sets default slop factor for 
#' 		'pf2'. If not specified, 'ps' will be used.
#' @param pf3 (Phrase trigram fields) As with 'pf' but chops the input into tri-grams, 
#' 		e.g. "the brown fox jumped" is queried as "the brown fox" "brown fox jumped"
#' @param ps3 (Phrase trigram slop) As with 'ps' but sets default slop factor for 'pf3'. 
#' 		If not specified, 'ps' will be used.
#' @param tie (Tie breaker) Float value to use as tiebreaker in DisjunctionMaxQueries 
#' 		(should be something much less than 1). Read more here \link{http://wiki.apache.org/solr/ExtendedDisMax#tie_.28Tie_breaker.29}
#' @param bq (Boost Query) A raw query string (in the SolrQuerySyntax) that will 
#' 		be included with the user's query to influence the score. Read more here 
#' 		\link{http://wiki.apache.org/solr/ExtendedDisMax#bq_.28Boost_Query.29}
#' @param bf (Boost Function, additive) Functions (with optional boosts) that will 
#' 		be included in the user's query to influence the score. Any function supported 
#' 		natively by Solr can be used, along with a boost value, e.g.: recip(rord(myfield),1,2,3)^1.5
#' 		Read more here \link{http://wiki.apache.org/solr/ExtendedDisMax#bf_.28Boost_Function.2C_additive.29}
#' @param boost (Boost Function, multiplicative) As for 'bf' but multiplies the boost into the score.
#' @param uf (User Fields) Specifies which schema fields the end user shall be allowed 
#' 		to query for explicitly. This parameter supports wildcards. Read more here
#' 		\link{http://wiki.apache.org/solr/ExtendedDisMax#uf_.28User_Fields.29}
#' @param lowercaseOperators This param controls whether to try to interpret lowercase 
#' 		words as boolean operators such as "and", "not" and "or". Set &lowercaseOperators=true 
#' 		to allow this. Default is "false".
#' @param fuzzy Use fuzzy matching on input DOIs. Defaults to FALSE. If FALSE, 
#' 		we stick "digital-object-ids" before the DOI so that the search sent to 
#' 		ORCID is for that exact DOI. If TRUE, we use some regex to find the DOI.
#' @details You can use any of the following within the query statement: given-names,
#' 		family-name, credit-name, other-names, email, grant-number, patent-number,
#' 		keyword, worktitle, digital-objectids, current-institution, affiliation-name,
#' 		current-primary-institution, text, or past-institution. 
#' 		
#' 		For more complicated queries the ORCID API supports using ExtendedDisMax.
#' 		See the documentation on the web here: \link{http://wiki.apache.org/solr/ExtendedDisMax}.
#' @seealso \code{orcid_doi}
#' @examples \dontrun{
#' # Get a list of names and Orcid IDs matching a name query
#' orcid(query="carl+boettiger")
#' orcid(query="given-names:carl+AND+family-name:boettiger")
#' 
#' # You can string together many search terms
#' orcid(query="johnson+cardiology+houston")
#' 
#' # And use boolean operators
#' orcid("johnson+AND(caltech+OR+'California+Institute+of+Technology')")
#' 
#' # And you can use start and rows arguments to do pagination
#' orcid("johnson+cardiology+houston", start = 2, rows = 3)
#' 
#' # Use search terms, here family name
#' orcid("family-name:Sanchez", start = 4, rows = 6)
#' 
#' # Use search terms, here...
#' orcid(query="Raymond", start=0, rows=10, defType="edismax")
#' 
#' # Search using keywords
#' orcid(query="keyword:ecology")
#' 
#' # Search by DOI
#' orcid(query="10.1087/20120404")
#' 
#' # Note the difference between the first wrt the second and third
#' orcid("10.1087/20120404")
#' orcid("%2210.1087/20120404%22")
#' orcid("digital-object-ids:%2210.1087/20120404%22")
#'  
#' # Search by text type
#' orcid("text:English")
#' 
#' ## Using more complicated SOLR queries
#' 
#' # Use the qf parameter to "boost" query fields so they are ranked higher
#' # 	See how it is different than the second query without using "qf"
#' orcid(defType = "edismax", query = "Raymond", qf = "given-names^1.0 family-name^2.0", start = 0, rows = 10)
#' orcid(query = "Raymond", start = 0, rows = 10)
#' 
#' # Use other SOLR parameters as well, here mm. Using the "mm" param, 1 and 2 word 
#' # 	queries require that all of the optional clauses match, but for queries with 
#' # 	three or more clauses one missing clause is allowed...See for more: 
#' # 	\link{http://lucene.apache.org/solr/4_1_0/solr-core/org/apache/solr/util/doc-files/min-should-match.html}
#' orcid(defType = "edismax", query="keyword:ecology+OR+evolution+OR+conservation", mm = 2, rows = 20)
#' }
#' @export
orcid <- function(query = NULL, start = NULL, rows = NULL, recursive = FALSE,
	defType = NULL, q.alt = NULL, qf = NULL, mm = NULL, qs = NULL, pf = NULL,
	ps = NULL, pf2 = NULL, ps2 = NULL, pf3 = NULL, ps3 = NULL, tie = NULL, 
	bq = NULL, bf = NULL, boost = NULL, uf = NULL, lowercaseOperators = NULL, 
	fuzzy = FALSE)
{
	url = "http://pub.orcid.org/search/orcid-bio"
	url2 <- paste0(url, "/?q=", query)
	args <- compact(list(httpAccept = 'application/orcid+xml',
											 start = start, rows = rows, defType = defType, q.alt = q.alt,
											 qf = qf, mm = mm, qs = qs, pf = pf, ps = ps, pf2 = pf2,
											 ps2 = ps2, pf3 = pf3, ps3 = ps3, tie = tie, bq = bq, bf = bf,
											 boost = boost, uf = uf, lowercaseOperators = lowercaseOperators))
  out <- getForm(url2, .params = args)
  tt <- xmlParse(out)
	toget <- c("relevancy-score","orcid", "creation-method", "completion-date", "submission-date",
						 "claimed", "email-verified", "given-names", "family-name", "external-id-orcid",
						 "external-id-common-name", "external-id-reference", "external-id-url")
	all <- xmlToList(tt)
# 	all <- xmlToList(tt)[[1]]
	out <- llply(all$`orcid-search-results`, function(x) unlist(x, recursive=TRUE))
	namefields <- function(x){
		temp <- sapply(strsplit(names(x), "\\."), function(y) y[length(y)])
		ttt <- data.frame(t(x))
		names(ttt) <- temp
		ttt
	}
	out2 <- llply(out, namefields)
	out2 <- out2[!names(out2) == ".attrs"]
	df <- do.call(rbind.fill, out2)
	df[order(df$`relevancy-score`, decreasing=FALSE),c("relevancy-score","orcid","given-names","family-name")]
}