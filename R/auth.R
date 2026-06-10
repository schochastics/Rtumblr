# Build an OAuth1.0a signed Authorization header for a request.
#
# `params` are the request parameters (e.g. limit, offset, query) that will also be
# sent in the query string; they MUST be included in the signature base string.
# Returns the same contract as before: list(params, header), but `header` is now a
# named character vector (consumed by httr2::req_headers) and the oauth_* values live
# only in the header, not duplicated into the query string.
handle_oauth1 <- function(app_credentials = NULL, path, params = list(), method = "GET") {
  if (is.null(app_credentials)) {
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  # The access token/secret come from the cached 3-legged flow, not the env var.
  if (is.null(app_credentials$oauth_token) || is.null(app_credentials$oauth_token_secret)) {
    tok <- rtumblr_auth(app_credentials)
    app_credentials$oauth_token <- tok$oauth_token
    app_credentials$oauth_token_secret <- tok$oauth_token_secret
  }

  url <- paste0("https://api.tumblr.com/", path)
  oauth <- oauth1_signature(
    url = url,
    method = method,
    consumer_key = app_credentials$consumer_key,
    consumer_secret = app_credentials$consumer_secret,
    token = app_credentials$oauth_token,
    token_secret = app_credentials$oauth_token_secret,
    params = params
  )

  list(params = list(), header = oauth_header(oauth))
}

# Core OAuth1.0a HMAC-SHA1 signer (RFC 5849). `params` are the extra request params
# (query/body) to fold into the signature base string. `extra_oauth` allows callers in
# the 3-legged flow to add oauth_callback / oauth_verifier. Returns the full named list
# of oauth_* parameters including oauth_signature.
oauth1_signature <- function(url, method = "GET", consumer_key, consumer_secret,
                             token = NULL, token_secret = NULL, params = list(),
                             extra_oauth = list()) {
  oauth <- list(
    oauth_consumer_key = consumer_key,
    oauth_nonce = oauth_nonce(),
    oauth_signature_method = "HMAC-SHA1",
    oauth_timestamp = as.character(as.integer(unclass(Sys.time()))),
    oauth_version = "1.0"
  )
  if (!is.null(token)) {
    oauth$oauth_token <- token
  }
  oauth <- c(oauth, extra_oauth)

  # all params (oauth_* plus request params) go into the signature base string
  all_params <- c(oauth, params)
  all_params <- lapply(all_params, as.character)
  enc_keys <- oauth_encode(names(all_params))
  enc_vals <- oauth_encode(unlist(all_params, use.names = FALSE))
  pairs <- paste0(enc_keys, "=", enc_vals)
  param_string <- paste(sort(pairs), collapse = "&")

  base_string <- paste(
    toupper(method),
    oauth_encode(url),
    oauth_encode(param_string),
    sep = "&"
  )
  signing_key <- paste0(
    oauth_encode(consumer_secret), "&",
    oauth_encode(token_secret %||% "")
  )
  signature <- openssl::base64_encode(
    openssl::sha1(charToRaw(base_string), key = signing_key)
  )
  oauth$oauth_signature <- signature
  oauth
}

# Turn a signed oauth list into the request headers (named character vector).
oauth_header <- function(oauth) {
  auth <- paste0(
    "OAuth ",
    paste0(oauth_encode(names(oauth)), "=\"", oauth_encode(unlist(oauth)), "\"",
           collapse = ",")
  )
  c(Authorization = auth, Accept = "application/json")
}

oauth_nonce <- function(n = 32) {
  paste0(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = "")
}

oauth_encode <- function(x) vapply(x, oauth_encode1, character(1), USE.NAMES = FALSE)

oauth_encode1 <- function(x) {
  encode <- function(x) paste0("%", toupper(as.character(charToRaw(x))), collapse = "")
  x <- as.character(x)
  chars <- strsplit(x, "")[[1]]
  ok <- !grepl("[^A-Za-z0-9_.~-]", chars)
  if (all(ok)) {
    return(x)
  }
  chars[!ok] <- unlist(lapply(chars[!ok], encode))
  paste0(chars, collapse = "")
}

get_rtumblr_token_from_envvar <- function(name = "RTUMBLR_TOKEN", check_stop = TRUE) {
  var <- Sys.getenv(name)
  if (var == "") {
    if (check_stop) {
      stop("No tumblr token found in env variables", call. = FALSE)
    } else {
      message("You should do software testing with the `RTUMBLR_TOKEN` envvar!")
      return(list(
        consumer_key = "abcdefghijkl",
        consumer_secret = "aaabbbcccddd"
      ))
    }
  }
  var_splt <- strsplit(var, ";")[[1]]
  app_credentials <- list(
    consumer_key = var_splt[1],
    consumer_secret = var_splt[2]
  )
  # optional 3rd/4th fields let power users supply an access token directly and skip
  # the browser flow entirely (e.g. from https://api.tumblr.com/console)
  if (length(var_splt) >= 4) {
    app_credentials$oauth_token <- var_splt[3]
    app_credentials$oauth_token_secret <- var_splt[4]
  }
  app_credentials
}
