# register app: https://www.tumblr.com/oauth/apps
# get tokens: https://api.tumblr.com/console
token <- list(
  consumer_key = 'NTYuq3rdFwIWM2H13fRFoJ9sW1JzgcjIPgbXFSumTnAp9C4MV9',
  consumer_secret = 'RuXGlU3WO4sLY5bLSrCnO3PiOmJPe20aKZAxQiEURXmemBmu1i',
  token = 'TxAEygZfE29CMsIyFpXrHo3nBoRcjWB1EUEtXJR1fF0fe8a1Jj',
  token_secret = 'UuhuibPXFWyrn9SzVZkzHBamFEKmrWBEjlXjc0x99RMRaPIxRq'
)


make_get_request <- function(path,params=list(),api_key = NULL){

  url <- httr::parse_url("https://api.tumblr.com")
  params <- c(params,api_key = api_key)
  request_results <- httr::GET(httr::modify_url(url, path = path),
                               query = params,httr::user_agent("Rtumblr"))

  status_code <- httr::status_code(request_results)
  if (!status_code %in% c(200)) {
    stop(paste("something went wrong. Status code:", status_code), call. = FALSE)
  }
  output <- httr::content(request_results)
  rate_limits <- get_rate_limit(request_results)
  attr(output,"rate_limit") <- rate_limits
  output
}


get_rate_limit <- function(response){
  header <- httr::headers(response)
  dplyr::bind_rows(header[c("x-ratelimit-perday-limit", "x-ratelimit-perday-remaining",
                            "x-ratelimit-perday-reset", "x-ratelimit-perhour-limit",
                            "x-ratelimit-perhour-remaining", "x-ratelimit-perhour-reset")])
}

handle_params <- function(params, before, after, offset) {
  if (!missing(before)) {
    params$before <- before
  }
  if (!missing(after)) {
    params$after <- after
  }
  if (!missing(offset)) {
    params$offset <- offset
  }
  params
}
