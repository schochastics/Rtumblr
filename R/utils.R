# register app: https://www.tumblr.com/oauth/apps
# get tokens: https://api.tumblr.com/console

`%||%` <- function(x, y) if (is.null(x)) y else x

make_request <- function(path, params = list(), body = NULL, api_key = NULL,
                         header = NULL, method = "GET") {
  params <- c(params, api_key = api_key)
  req <- httr2::request("https://api.tumblr.com")
  req <- httr2::req_url_path(req, path)
  if (length(params)) {
    req <- httr2::req_url_query(req, !!!params)
  }
  req <- httr2::req_user_agent(req, "RTumblr")
  req <- httr2::req_method(req, method)
  # keep the historic manual status check (httr2 aborts on 4xx/5xx by default)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  if (!is.null(header)) {
    req <- httr2::req_headers(req, !!!as.list(header))
  }
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  request_results <- httr2::req_perform(req)

  status_code <- httr2::resp_status(request_results)
  if (!status_code %in% c(200, 201)) {
    stop(paste("something went wrong. Status code:", status_code), call. = FALSE)
  }
  ctype <- httr2::resp_content_type(request_results)
  output <- if (grepl("json", ctype, fixed = TRUE)) {
    httr2::resp_body_json(request_results)
  } else {
    httr2::resp_body_raw(request_results)
  }
  attr(output, "rate_limit") <- get_rate_limit(request_results)
  output
}

make_get_request <- function(path, params = list(), api_key = NULL, header = NULL) {
  make_request(path, params = params, body = NULL, api_key = api_key,
               header = header, method = "GET")
}

# Sign and perform an OAuth1 GET request. The request params are both signed and sent
# in the query string; the oauth_* values travel in the Authorization header.
oauth_get <- function(path, params = list(), app_credentials = NULL) {
  oauth_head <- handle_oauth1(app_credentials, path, params, method = "GET")
  make_get_request(path, params = params, api_key = NULL, header = oauth_head$header)
}

get_rate_limit <- function(response) {
  header <- as.list(httr2::resp_headers(response))
  keys <- c("x-ratelimit-perday-limit", "x-ratelimit-perday-remaining",
            "x-ratelimit-perday-reset", "x-ratelimit-perhour-limit",
            "x-ratelimit-perhour-remaining", "x-ratelimit-perhour-reset")
  present <- header[keys[keys %in% names(header)]]
  if (!length(present)) {
    return(tibble::tibble())
  }
  tibble::as_tibble(lapply(present, as.character))
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

# Generic pagination over offset (capped at 1000 by the Tumblr API), then switching
# to timestamp paging via `before` using the oldest item's timestamp.
#
# fetch_page(limit, offset, before) must return a raw response list (as produced by
#   make_get_request); extract(out) must return the list of items from that response.
# n caps the total number of items returned (Inf for "all available").
paginate_get <- function(fetch_page, extract, n = Inf, page_size = 20) {
  collected <- list()
  offset <- 0L
  before <- NULL
  total <- NULL
  out <- NULL
  repeat {
    remaining <- n - length(collected)
    if (remaining <= 0) break
    this_limit <- min(page_size, remaining)

    out <- if (is.null(before)) {
      fetch_page(this_limit, offset, NULL)
    } else {
      fetch_page(this_limit, NULL, before)
    }
    items <- extract(out)
    if (length(items) == 0) break
    collected <- c(collected, items)

    total <- total %||% out[["response"]][["total_posts"]]
    if (!is.null(total) && length(collected) >= total) break
    if (length(collected) >= n) break

    if (offset + page_size < 1000) {
      offset <- offset + page_size
    } else {
      ts <- vapply(items, function(x) {
        as.numeric(x[["timestamp"]] %||% NA_real_)
      }, numeric(1))
      if (all(is.na(ts))) break
      before <- min(ts, na.rm = TRUE)
    }
  }
  attr(collected, "rate_limit") <- attr(out, "rate_limit")
  collected
}
