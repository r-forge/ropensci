#' Function to get Comtrade data.
#' 
#' @import XML
#' @importFrom httr GET stop_for_status
#' @importFrom plyr compact
#' @param cc Commodity Code
#' @param px Commodity Classification
#' @param r Reporter
#' @param y Year
#' @param comp Compression or not, default is TRUE
#' @param rg Trade Flow
#' @param p Partner Country
#' @param code Authorization code
#' @examples \dontrun{
#' # Install httr and XML if you don't have them installed
#' # Then load them
#' library(httr); library(XML); library(plyr)
#' 
#' # Search for data - IMPORTANT: put in your code in the code parameter
#' comtrade(cc='TOTAL', px='H2', r=372, y=2006, comp=TRUE, code='yourcode')
#' }

comtrade <- function(cc=NULL, px=NULL, r=NULL, y=NULL, comp=NULL, rg=NULL, 
                     p=NULL, code, callopts=list())
{
  # Base URL for all API calls
  base <- 'http://comtrade.un.org/ws/getSdmxV1.aspx'
  
  # make a list of query arguments, compact function removes those with NULL
  args <- compact(list(cc=cc, px=px, r=r, y=y, comp=comp, rg=rg, p=p, code=code))
  
  # Make the API call with the base url, arguments, and any optional curl 
  # parameters passed in callopts list
  tt <- GET(base, query=args, callopts)
  
  # Check for http status. If the status is soemthing bad, the fxn stops
  stop_for_status(tt)
  
  # content fxn extracts data from the object tt
  content(tt)
}