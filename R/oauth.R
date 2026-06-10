#' Authenticate with Tumblr (OAuth 1.0a)
#'
#' Performs the interactive three-legged OAuth 1.0a flow: it opens a browser so you
#' can authorize the app, then exchanges the verifier for a durable access token. The
#' access token is cached on disk (under [tools::R_user_dir()]) so the browser only
#' opens once; subsequent sessions reuse the cached token.
#'
#' You normally do not need to call this directly: any function that hits an
#' OAuth-protected endpoint will trigger it automatically when no cached token is found.
#' As before, you only need to provide your app's consumer key and secret via the
#' `RTUMBLR_TOKEN` environment variable (`"consumer_key;consumer_secret"`).
#'
#' @param app_credentials a named list with `consumer_key` and `consumer_secret`. If
#'   `NULL`, loaded from the `RTUMBLR_TOKEN` environment variable.
#' @param cache logical, whether to read/write the access token from the on-disk cache.
#' @param force logical, if `TRUE` re-run the flow even if a cached token exists.
#'
#' @return invisibly, a list with `oauth_token` and `oauth_token_secret`.
#' @export
#' @examples
#' \dontrun{
#' rtumblr_auth()
#' }
rtumblr_auth <- function(app_credentials = NULL, cache = TRUE, force = FALSE) {
  if (is.null(app_credentials)) {
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  # credentials supplied directly (4-field env var) — nothing to do
  if (!is.null(app_credentials$oauth_token) &&
      !is.null(app_credentials$oauth_token_secret)) {
    return(invisible(list(
      oauth_token = app_credentials$oauth_token,
      oauth_token_secret = app_credentials$oauth_token_secret
    )))
  }

  if (cache && !force) {
    cached <- read_token_cache(app_credentials$consumer_key)
    if (!is.null(cached)) {
      return(invisible(cached))
    }
  }

  if (!interactive()) {
    stop("OAuth authentication requires an interactive session. ",
         "Alternatively set RTUMBLR_TOKEN to ",
         "'consumer_key;consumer_secret;oauth_token;oauth_token_secret'.",
         call. = FALSE)
  }

  use_oob <- !requireNamespace("httpuv", quietly = TRUE)
  callback <- if (use_oob) "oob" else "http://localhost:1410/"

  # leg 1: request token
  rt <- oauth1_request(
    "https://www.tumblr.com/oauth/request_token",
    app_credentials,
    extra_oauth = list(oauth_callback = callback)
  )
  request_token <- rt[["oauth_token"]]
  request_secret <- rt[["oauth_token_secret"]]

  # leg 2: authorize in browser, capture verifier
  auth_url <- paste0("https://www.tumblr.com/oauth/authorize?oauth_token=", request_token)
  verifier <- oauth_authorize(auth_url, use_oob = use_oob)

  # leg 3: exchange for access token
  at <- oauth1_request(
    "https://www.tumblr.com/oauth/access_token",
    app_credentials,
    token = request_token,
    token_secret = request_secret,
    extra_oauth = list(oauth_verifier = verifier)
  )
  token <- list(
    oauth_token = at[["oauth_token"]],
    oauth_token_secret = at[["oauth_token_secret"]]
  )
  if (cache) {
    write_token_cache(app_credentials$consumer_key, token)
  }
  invisible(token)
}

# Perform a signed OAuth1 request used by legs 1 and 3 of the flow. The response is
# x-www-form-urlencoded (oauth_token=...&oauth_token_secret=...), parsed into a list.
oauth1_request <- function(url, app_credentials, token = NULL, token_secret = NULL,
                           extra_oauth = list(), method = "POST") {
  oauth <- oauth1_signature(
    url = url,
    method = method,
    consumer_key = app_credentials$consumer_key,
    consumer_secret = app_credentials$consumer_secret,
    token = token,
    token_secret = token_secret,
    extra_oauth = extra_oauth
  )
  req <- httr2::request(url)
  req <- httr2::req_method(req, method)
  req <- httr2::req_user_agent(req, "RTumblr")
  req <- httr2::req_headers(req, !!!as.list(oauth_header(oauth)))
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop("OAuth request to ", url, " failed (status ",
         httr2::resp_status(resp), ")", call. = FALSE)
  }
  parse_form_encoded(httr2::resp_body_string(resp))
}

parse_form_encoded <- function(x) {
  pairs <- strsplit(x, "&", fixed = TRUE)[[1]]
  out <- list()
  for (p in pairs) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    out[[kv[1]]] <- if (length(kv) > 1) utils::URLdecode(kv[2]) else ""
  }
  out
}

# Open the authorize URL in a browser and capture oauth_verifier, either via a local
# httpuv server (matches the historic httr behaviour) or out-of-band PIN entry.
oauth_authorize <- function(auth_url, use_oob = FALSE) {
  utils::browseURL(auth_url)
  if (use_oob) {
    message("Authorize the app in your browser, then paste the verification code (PIN).")
    return(trimws(readline("Verification code: ")))
  }
  verifier <- NULL
  app <- list(
    call = function(req) {
      params <- parse_form_encoded(sub("^\\?", "", req$QUERY_STRING))
      verifier <<- params[["oauth_verifier"]]
      list(
        status = 200L,
        headers = list("Content-Type" = "text/plain"),
        body = "Authentication complete. You may close this window and return to R."
      )
    }
  )
  server <- httpuv::startServer("127.0.0.1", 1410L, app)
  on.exit(httpuv::stopServer(server), add = TRUE)
  message("Waiting for authorization in the browser...")
  while (is.null(verifier)) {
    httpuv::service()
    Sys.sleep(0.1)
  }
  verifier
}

token_cache_path <- function(consumer_key) {
  dir <- tools::R_user_dir("Rtumblr", "cache")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  # one cache file per consumer key
  key_hash <- substr(openssl::md5(consumer_key %||% ""), 1, 16)
  file.path(dir, paste0("token-", key_hash, ".rds"))
}

read_token_cache <- function(consumer_key) {
  path <- token_cache_path(consumer_key)
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) NULL)
}

write_token_cache <- function(consumer_key, token) {
  saveRDS(token, token_cache_path(consumer_key))
  invisible(token)
}
