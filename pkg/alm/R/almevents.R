#' Retrieve PLoS article-level metrics (ALM) events.
#' 
#' Events are the details of the metrics that are counted related to PLoS papers.
#' 
#' @importFrom RCurl getCurlHandle getForm
#' @importFrom RJSONIO fromJSON
#' @importFrom reshape sort_df
#' @importFrom plyr compact rbind.fill
#' @param doi Digital object identifier for an article in PLoS Journals (character)
#' @param pmid PubMed object identifier (numeric)
#' @param pmcid PubMed Central object identifier (numeric)
#' @param mdid Mendeley object identifier (character)
#' @param url API endpoint, defaults to http://alm.plos.org/api/v3/articles (character)
#' @param months Number of months since publication to request historical data for.
#' 		See details for a note. (numeric)
#' @param days Number of days since publication to request historical data for. 
#' 		See details for a note. (numeric)
#' @param source The source to get events data from. You can pass in a character
#' 		vector, like: \code{c("mendeley","crossref")}
#' @param key your PLoS API key, either enter, or loads from .Rprofile (character)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @details You can only supply one of the parmeters doi, pmid, pmcid, and mdid.
#' 		
#' 		Query for as many articles at a time as you like. Though queries are broken
#' 		up in to smaller bits of 30 identifiers at a time. 
#' 		
#' 		If you supply both the days and months parameters, days takes precedence,
#' 		and months is ignored.
#' 		
#' 		You can get events from many different sources. After calling almevents, 
#' 		then index the output by the data provider you want. The options are:
#' 		bloglines, citeulike, connotea, crossref, nature, postgenomic, pubmed, 
#' 		scopus, plos, researchblogging, biod, webofscience, pmc, facebook,
#' 		mendeley, twitter, wikipedia, and scienceseeker.
#' 		
#' 		Beware that some data source are not parsed yet, so there may be event data
#' 		but it is not provided yet as it is so messy to parse. 
#'   	
#'    See more info on PLOS's relative metrics event source here 
#'    \url{http://www.plosone.org/static/almInfo#relativeMetrics}
#' @return PLoS altmetrics as data.frame's.
#' @examples \dontrun{
#' # For one article
#' out <- almevents(doi="10.1371/journal.pone.0029797")
#' names(out) # names of sources
#' # remove those with no data
#' out <- out[!out %in% c("sorry, no events content yet","parser not written yet")]
#' out[["pmc"]] # get the results for PubMed Central
#' out[["twitter"]] # get the results for twitter (boo, there aren't any)
#' out[c("twitter","crossref")] # get the results for two sources
#' 
#' # 
#' out <- alm(doi="10.1371/journal.pgen.1003471")
#' out[["wordpress"]]
#' 
#' # Another example
#' out <- almevents(doi="10.1371/journal.pone.0001543")
#' # remove those with no data
#' out <- out[!out %in% c("sorry, no events content yet","parser not written yet")]
#' names(out)
#' 
#' # Another example
#' out <- almevents(doi="10.1371/journal.pone.0035869")
#' # remove those with no data
#' out <- out[!out %in% c("sorry, no events content yet","parser not written yet")]
#' names(out)
#' 
#' # Two doi's
#' dois <- c('10.1371/journal.pone.0001543','10.1371/journal.pone.0040117')
#' out <- almevents(doi=dois)
#' out[[1]]
#' out[[2]]
#' out[[1]][["figshare"]][[2]][[1]]
#' 
#' # Specify a specific source
#' almevents(doi="10.1371/journal.pone.0035869", source="crossref")
#' 
#' # Specify two specific sources
#' almevents(doi="10.1371/journal.pone.0035869", source=c("crossref","twitter"))
#' }
#' @export
almevents <- function(doi = NULL, pmid = NULL, pmcid = NULL, mdid = NULL, 
  url='http://alm.plos.org/api/v3/articles', months = NULL, days = NULL, 
  source = NULL, key = NULL, curl = getCurlHandle())
{
	id <- compact(list(doi=doi, pmid=pmid, pmcid=pmcid, mendeley=mdid))
	if(length(id)>1){ stop("Only supply one of: doi, pmid, pmcid, mdid") } else { NULL }
	key <- getkey(key)
	if(is.null(source)){source2 <- NULL} else{ source2 <- paste(source,collapse=",") }
	
	parse_events <- function() {	
		args <- compact(
			list(
				api_key = key, info = 'event', months = months, 
				days = days, source = source2, type = names(id)
				)
			)
		if(length(id[[1]])==0){stop("Please provide a DOI")} else
			if(length(id[[1]])==1){
				if(names(id) == "doi") id <- gsub("/", "%2F", id)
				args2 <- c(args, ids = id[[1]])
				out <- getForm(url, .params = args2, curl = curl)
				ttt <- RJSONIO::fromJSON(out)
			} else
				if(length(id[[1]])>1){
					if(length(id[[1]])>50){
						slice <- function(x, n) split(x, as.integer((seq_along(x) - 1) / n))
						idsplit <- slice(id[[1]], 50)
						repeatit <- function(y) {
							if(names(id) == "doi"){ 
								id2 <- paste(sapply(y, function(x) gsub("/", "%2F", x)), collapse=",")
							} else
							{
								id2 <- paste(id[[1]], collapse=",")
							}
							args2 <- c(args, ids = id2)
							out <- getForm(url, .params = args2, curl = curl)
							ttt <- RJSONIO::fromJSON(out)
						}
						temp <- lapply(idsplit, repeatit)
						ttt <- do.call(c, temp)
					} else {
						if(names(id) == "doi") {
							id2 <- paste(sapply(id, function(x) gsub("/", "%2F", x)), collapse=",")
						} else
						{
							id2 <- paste(id[[1]], collapse=",")
						}
						args2 <- c(args, ids = id2)
						out <- getForm(url, .params = args2, curl = curl)
						ttt <- RJSONIO::fromJSON(out)
					}
				}
		
		# get juse the events data
		events <- lapply(ttt, function(x) x$sources) 
	
		# Function to extract and parse events data for each source
		getevents <- function(x, label=NULL){
			
			# Parser code
			parsers <- function(y){
				if(y$name == "counter"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						year <- as.numeric(sapply(y$events, `[[`, "year"))
            month <- as.numeric(sapply(y$events, `[[`, "month"))
						pdf_views <- as.numeric(sapply(y$events, `[[`, "pdf_views"))
						html_views <- as.numeric(sapply(y$events, `[[`, "html_views"))
						xml_views <- as.numeric(sapply(y$events, `[[`, "xml_views"))
						data.frame(year, month, pdf_views, html_views, xml_views)
					}
				} else if(y$name == "citeulike"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
            y$events
					}
				} else if(y$name == "crossref"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{			
						parsecrossref <- function(x) {
              if(is.null(x[[1]][["publication_type"]])){
                x[[1]][["publication_type"]] <- NA
              }
              if(!("contributors" %in% names(x[[1]]))){
                x[[1]][["contributors"]] <- list(contributor=NA)
                x[[1]]$issn <- paste(x[[1]]$issn, collapse="; ")
								data.frame(x[[1]])
              } else if(length(x[[1]]$contributors$contributor[[1]])>1){
								x[[1]]$contributors$contributor <-
									paste(sapply(x[[1]]$contributors$contributor,
															 function(x) paste(x[1:2], collapse=" ")), collapse="; ")
								x[[1]]$issn <- paste(x[[1]]$issn, collapse="; ")
								data.frame(x[[1]])
							} else {
								x[[1]]$contributors$contributor <-
									paste(x[[1]]$contributors$contributor[1:2], collapse=" ")
								x[[1]]$issn <- paste(x[[1]]$issn, collapse="; ")
								data.frame(x[[1]])
							}
						}
						ldply(y$events, parsecrossref)
					}
				} else if(y$name == "nature"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						parsenature <- function(x){
							temp <- x$event
							blog_ <- data.frame(temp$blog[names(temp$blog) %in% c('title','url')])
							names(blog_) <- c('blog_title','blog_url')
							post_ <- data.frame(temp[names(temp) %in% c('title','num_words','url','percent_complex_words','created_at')])	
							names(post_) <- c('post_percent_complex_words','post_created_at','post_title','post_url','post_num_words')	
							cbind(blog_, post_)
						}
						ldply(y$events, parsenature)
					} 
				} else if(y$name == "researchblogging"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						parserblogging <- function(w){
							temp <- w$event
							bloginfo <- data.frame(temp[names(temp) %in% c('post_title','blog_name','blogger_name','published_date','post_url')])
							if(length(temp$citations$citation[[1]])>1){
								citations <- paste(sapply(temp$citations$citation, function(z) z$doi), sep="", collapse=",")
							} else
							{ 
								citations <- temp$citations$citation$doi
							}
							cbind(bloginfo, citations)
						}
            if(length(y$events)==1){
              parserblogging(y$events)
            } else
            {
              do.call(rbind, lapply(y$events, parserblogging))
            }
					}
				} else if(y$name == "biod"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						if(length(y$events) > 1){
							do.call(rbind, lapply(y$events, data.frame))
						} else
						{
							y$events
						}
					}
				} else if(y$name == "pubmed"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
						{ sapply(y$events, function(x) x[c("event","event_url")]) }
				} else if(y$name == "facebook"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						parsefb <- function(x){ 
              x[sapply(x, is.null)] <- "none"
              data.frame(x) 
						}
            if(names(y$events)[[1]]=="url"){
              parsefb(y$events)
            } else
            {
              lapply(y$events, parsefb)
            }
					}
				} else if(y$name == "mendeley"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						parsemendeley <- function(x){
							readers <- data.frame(name="readers", value=x$stats$readers)
							disc <- ldply(x$stats$discipline, function(x) data.frame(x))[,-1]
							country <- ldply(x$stats$country, function(x) data.frame(x))
							status <- ldply(x$stats$status, function(x) data.frame(x))
							dfs <- list(readers = readers, discipline = disc, country = country, status = status)
							ldply(dfs)
						}
						parsemendeley(y$events)
					}
				} else if(y$name == "twitter"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
					  temp <- lapply(y$events, function(x) data.frame(t(data.frame(x[[1]]))))
					  tempdf <- do.call(rbind, temp)
            row.names(tempdf) <- NULL
            tempdf
					}
				} else if(y$name == "wikipedia"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{ 
					  df <- data.frame(y$events)
					  df$lang <- row.names(df)
					  names(df) <- c("values","lang")
					  row.names(df) <- NULL
					  df
					}
				} else if(y$name == "bloglines"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{ 
						parsebloglines <- function(x){
							temp <- data.frame(t(x$event))
							if(any(names(temp) %in% "author")==TRUE && any(names(temp) %in% "site_name")==TRUE)
							{
								temp2 <- temp[,c("site_name","author")]
							} else
							{
								temp2 <- data.frame(site_name=temp$site_name, author="none")
							}
							cbind(temp2, event_url=x$event_url)
						}
						ldply(y$events, parsebloglines)
					}
				} else if(y$name == "postgenomic"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
						{
							temp <- y$events[[1]]
							name <- temp$event$blog_name
							eventurl <- temp$event_url
							dois <- sapply(temp$event$citing, function(x) x$doi_id )
							list(blog_name=name, event_url=eventurl, dois=dois)
						}
				} else if(y$name == "scopus"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
						{ y$events[[1]] }
				} else if(y$name == "wos"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{ 
						if(length(y$events) > 1){
							ldply(y$events, function(x) data.frame(t(x)))
						} else
						{
							y$events
						}
					}
				} else if(y$name == "pmc"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						parsepmc <- function(x, names_){
							gg <- data.frame(x)
							gg$it <- row.names(gg)
							if(!names_){as.numeric(as.character(t(sort_df(gg, "it")[,-2])))} else
							{ sort_df(gg, "it")[,-1] }
						}
						df <- data.frame(do.call(rbind, lapply(y$events, parsepmc, names_=FALSE)))
						names(df) <- parsepmc(y$events[[1]], TRUE)
						df
					}
				} else if(y$name == "connotea"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{ paste("parser not written yet") }
				} else if(y$name == "scienceseeker"){
					if(length(y$events)==0){paste("sorry, no events content yet")} else
					{
						 parsesciseeker <- function(x){
						 	temp <- x$event
						 	info <- temp[c('title','author')]
						 	recommendations <- data.frame(t(sapply(temp$`ss:community`$`ss:recommendations`, function(x) x[[2]])))
						 	names(recommendations) <- c("user","editor")
						 	categories <- paste(sapply(temp$category, function(x) x[[1]]), collapse=",")
						 	
						 	cbind(info, recommendations, categories=categories, event_url=x$event_url)
						 }
						 ldply(y$events, parsesciseeker)
					}
				} else if(y$name == "relativemetric"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    meta <- y$events[names(y$events) %in% c("start_date","end_date")]
				    data <- do.call(rbind.fill,
				                    lapply(y$events$subject_areas, function(x) 
				                      data.frame(x[[1]], t(data.frame(x[[2]])))
				                    )
				    )
            row.names(data) <- NULL
#             names(data) <- c('reference_set','one','two','three','four','five','six','seven')
				    list(meta=meta, data=data)
				  }
				} else if(y$name == "f1000"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
# 				    paste("parser not written yet")
				  }
				} else if(y$name == "figshare"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
# 				    paste("parser not written yet")
				  }
				} else if(y$name == "wordpress"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
# 				    paste("parser not written yet")
				  }
				} else if(y$name == "pmceurope"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
				    # 				    paste("parser not written yet")
				  }
				} else if(y$name == "pmceuropedata"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
				    # 				    paste("parser not written yet")
				  }
				} else if(y$name == "openedition"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
				    # 				    paste("parser not written yet")
				  }
				} else if(y$name == "reddit"){
				  if(length(y$events)==0){paste("sorry, no events content yet")} else
				  {
				    y$events
				    # 				    paste("parser not written yet")
				  }
				}
			}
			
			# Run the parsers on each element
			datout <- lapply(x, parsers)
			
			# Assign names to each list element
			if(is.null(label)){		
# 				names(datout) <- c("bloglines","citeulike","connotea","crossref","nature",
# 													 "postgenomic","pubmed","scopus","plos","researchblogging",
# 													 "biod","webofscience","pmc","facebook","mendeley","twitter",
# 													 "wikipedia","scienceseeker","relativemetric","f1000","figshare")
        names(datout) <- sapply(events[[1]], "[[", "name")
			} else
			{
				names(datout) <- label
			}
			return( datout )
		}
		
		# Actually get the events data
		temp <- lapply(events, getevents, label=source)
		
		# Return the data
		return( temp )
	}
	safe_parse_events <- plyr::failwith(NULL, parse_events)
	finaldata <- safe_parse_events()
	if(length(finaldata)>1){ return( finaldata )} else { finaldata[[1]] }
}