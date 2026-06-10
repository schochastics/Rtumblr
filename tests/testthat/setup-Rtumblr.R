# Use a fake token so the package's credential loading is satisfied offline.
if (!nzchar(Sys.getenv("RTUMBLR_TOKEN"))) {
  Sys.setenv("RTUMBLR_TOKEN" = "aaabbbcccdddeee;aaabbbcccdddeee")
}

# Make httptest2 fixtures independent of the (secret) api_key: strip its value from
# the request URL before the mock path is computed, and scrub it from recordings.
# The redactor is called with a request during replay and a response during recording.
if (requireNamespace("httptest2", quietly = TRUE)) {
  httptest2::set_redactor(function(x) {
    strip <- function(url) gsub("api_key=[^&]*", "api_key=", url)
    if (inherits(x, "httr2_request")) {
      x$url <- strip(x$url)
    } else if (!is.null(x$request)) {
      x$request$url <- strip(x$request$url)
    }
    x
  })
}
