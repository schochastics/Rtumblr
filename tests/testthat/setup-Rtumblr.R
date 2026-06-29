# Use a fake token so the package's credential loading is satisfied offline.
if (!nzchar(Sys.getenv("RTUMBLR_TOKEN"))) {
  Sys.setenv("RTUMBLR_TOKEN" = "aaabbbcccdddeee;aaabbbcccdddeee")
}

# Make httptest2 fixtures independent of the (secret) api_key: strip its value from
# the request URL before the mock path is computed, and scrub it from recordings.
# Also drop ".tumblr.com" from the URL so the recorded file paths stay short enough
# to be portable (CRAN flags tarball paths over 100 characters).
# The redactor is called with a request during replay and a response during recording.
if (requireNamespace("httptest2", quietly = TRUE)) {
  httptest2::set_redactor(function(x) {
    shorten <- function(url) {
      url <- gsub("api_key=[^&]*", "api_key=", url)
      gsub(".tumblr.com", "", url, fixed = TRUE)
    }
    if (inherits(x, "httr2_request")) {
      x$url <- shorten(x$url)
    } else if (!is.null(x$request)) {
      x$request$url <- shorten(x$request$url)
    }
    x
  })
}
