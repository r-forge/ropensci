

#' Authenticates with bit.ly oauth2 api
#'
#' @param key bit.ly consumer key
#' @param  secret bit.ly consumer secret
#' @return oauth credentials
#' @examples \dontrun{
#' bitly_auth()
#'}
bitly_auth <- function(key = getOption("BitlyKey"), secret = getOption("BitlySecret")) {
auth_url <- "https://bitly.com/oauth/authorize"

}
