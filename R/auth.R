handle_oauth1 <- function(app_credentials = NULL,path,params){

  url <- httr::parse_url("https://api.tumblr.com")
  tumblr <- httr::oauth_endpoint("request_token","authorize","access_token",base_url = "https://www.tumblr.com/oauth")
  app <- httr::oauth_app("RTumblr",key=app_credentials$consumer_key,
                         secret=app_credentials$consumer_secret)
  token <- httr::oauth1.0_token(tumblr,app)
  oauth <- httr::oauth_signature(httr::modify_url(url, path = path),method="GET",app = app,
                        token=token$credentials$oauth_token,
                        token_secret=token$credentials$oauth_token_secret, other_params = params)

  oauth_head <- httr::add_headers(
    Accept = "application/json",
    Authorization = paste0("OAuth ",paste0(oauth_encode(names(oauth)),"=\"",
                                           oauth_encode(oauth),"\"",collapse=",")))
  return(list(params = oauth,header = oauth_head))
}

oauth_encode <- function (x) vapply(x, oauth_encode1, character(1))

oauth_encode1 <- function (x){
  encode <- function(x) paste0("%", toupper(as.character(charToRaw(x))),
                               collapse = "")
  x <- as.character(x)
  chars <- strsplit(x, "")[[1]]
  ok <- !grepl("[^A-Za-z0-9_.~-]",chars)
  if (all(ok))
    return(x)
  chars[!ok] <- unlist(lapply(chars[!ok], encode))
  paste0(chars, collapse = "")
}

get_rtumblr_token_from_envvar <- function(name = "RTUMBLR_TOKEN",check_stop=TRUE){
  var <- Sys.getenv(name)
  if(var==""){
    if(check_stop){
      stop("No tumblr token found in env variables",call.=FALSE)
    } else{
      message("You should do software testing with the `RTUMBLR_TOKEN` envvar!")
      return(list(
        consumer_key="abcdefghijkl",
        consumer_secret="aaabbbcccddd"
      ))
    }
  }
  var_splt <- strsplit(var,";")[[1]]
  app_credentials <- list(
    consumer_key = var_splt[1],
    consumer_secret = var_splt[2]
  )
  app_credentials
}
