#' Search uBio by namebank ID.
#' 
#' @import httr
#' @importFrom XML xpathApply getNodeSet xmlValue xmlToList
#' @importFrom RCurl base64Decode
#' @importFrom plyr compact
#' @param searchName (string) - term to search within name string
#' @param searchAuth (string) - term to search within name authorship
#' @param searchYear (string) - term to search within name year
#' @param order (string) - (name or namebankID) field by which the results will 
#' be sorted (default is namebankID)
#' @param sci (int) - (sci, vern, or all) type of results to be returned 
#' (default is all)
#' @param vern (int) - (limit 1000) maximum number of results to be returned 
#' (default is 1000)
#' @param keyCode Your uBio API key; loads from .Rprofile. If you don't have 
#'    one, obtain one at http://www.ubio.org/index.php?pagename=form.
#' @details Can't seem to get json format results along with specifiying an 
#'    API key, so if you use json your key is not specified at the moment
#' @return A dataframe.
#' @examples \dontrun{
#' ubio_id(namebankID = 2483153)
#' }
#' @export
ubio_id <- function(namebankID = NULL, keyCode = NULL, callopts=list())
{
  url <- "http://www.ubio.org/webservices/service.php"
  keyCode <- getkey(keyCode, "ubioApiKey")
  args <- compact(list(
    'function' = 'namebank_object', namebankID = namebankID, keyCode = keyCode))
  tmp <- GET(url, query=args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  
  toget <- c("namebankID", "nameString", "fullNameString", "packageID", 
             "packageName", "basionymUnit", "rankID", "rankName")
  temp <- lapply(toget, function(x) sapply(xpathApply(tt, paste("/results/", x, sep="")), xmlValue))
  temp[2:3] <- sapply(temp[2:3], base64Decode)
  out <- data.frame(do.call(cbind, temp))
  names(out) <- c("namebankID", "nameString", "fullNameString", "packageID", 
                  "packageName", "basionymUnit", "rankID", "rankName")
  
  syns <- getxmldata(obj=tt, node="homotypicSynonyms", todecode=2:3)
  verns <- getxmldata(obj=tt, node="vernacularNames", todecode=2)
  
  list(data=out, synonyms=ldfast(syns, convertvec=TRUE), 
       vernaculars=ldfast(verns, convertvec=TRUE))
}

#' Function to parse xml data and decode strings
#' @export
#' @keywords internal
getxmldata <- function(obj, node, todecode){
  tmp <- getNodeSet(obj, sprintf("/results/%s", node))[[1]]
  tmp2 <- xpathApply(tmp, sprintf("//%s", node), fun=xmlToList)[[1]]
  lapply(tmp2, function(x){
    x[todecode] <- sapply(x[todecode], RCurl::base64Decode)
    x
  })
}