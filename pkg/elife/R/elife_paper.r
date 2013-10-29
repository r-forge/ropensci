#' Get full text eLife papers via XML. 
#' 
#' @import httr XML
#' @param doi DOI to get full text for. 
#' @param what Specify what you want returned. One of xml, all, abstract, introduction,
#'    methods, results, discussion, figtabcaps, references.
#' @details Returns what you like with the what parameter.
#' @examples \dontrun{
#' elife_paper(doi="10.7554/eLife.00160")
#' }
#' @export
elife_paper <- function(doi = NULL, what='xml')
{
	url <- sprintf("http://elife.elifesciences.org/elife-source-xml/%s", doi)
	tt <- content(GET(url), as="text")
	out <- xmlParse(tt)
  
  if(what=='xml') { out }
   else if(what=='all') { xx }
  
	abstract <- xpathApply(out, "//abstract[@hwp:id='abstract-1']/p", fun=xmlValue)[[1]]
	exec_summary <- paste0(xpathApply(out, "//abstract[@hwp:id='abstract-2']/p", fun=xmlValue), collapse=" ")
	intro <- paste0(xpathApply(out, "//body/sec[@sec-type='intro']/p", fun=xmlValue), collapse=" ")
	
	xpathApply(out, "//body/sec[@sec-type='results']//p", fun=xmlValue)
  resultsnodes_title <- getNodeSet(out, "//body/sec[@sec-type='results']/sec/title")
	resultsnodes_p <- getNodeSet(out, "//body/sec[@sec-type='results']/sec/p")
  res_titles <- sapply(resultsnodes_title, xmlValue)
	res_p <- sapply(resultsnodes_p, xmlValue)
  names(res_p) <- res_titles
  
	disc <- 
    paste0(xpathApply(out, "//body/sec[@sec-type='discussion']/p", fun=xmlValue), collapse=" ")
	disc <- xpathApply(out, "//body/sec[@sec-type='discussion']//p", fun=xmlValue)
  disc[grep("DOI", disc)]
}

getNodeSet(out, "//body/sec[@sec-type='discussion']")

xpathApply(out, "//body/sec[@sec-type='discussion']//p[not(@hwp:id = 'p-55')]", fun=xmlValue)
xpathApply(out, "//body/sec[@sec-type='discussion']//p[not(parent::fig)]", fun=xmlValue)

xpathSApply(out, "//body/sec[@sec-type='discussion']//p[parent::caption]", fun=xmlValue)