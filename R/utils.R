# register app: https://www.tumblr.com/oauth/apps
# get tokens: https://api.tumblr.com/console

make_get_request <- function(path,params=list(),api_key = NULL,header=NULL){

  url <- httr::parse_url("https://api.tumblr.com")
  params <- c(params,api_key = api_key)
  request_results <- httr::GET(httr::modify_url(url, path = path),
                               query = params,httr::user_agent("RTumblr"),header)

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
