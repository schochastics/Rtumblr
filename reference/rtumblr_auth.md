# Authenticate with Tumblr (OAuth 1.0a)

Performs the interactive three-legged OAuth 1.0a flow: it opens a
browser so you can authorize the app, then exchanges the verifier for a
durable access token. The access token is cached on disk (under
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) so the
browser only opens once; subsequent sessions reuse the cached token.

## Usage

``` r
rtumblr_auth(app_credentials = NULL, cache = TRUE, force = FALSE)
```

## Arguments

- app_credentials:

  a named list with `consumer_key` and `consumer_secret`. If `NULL`,
  loaded from the `RTUMBLR_TOKEN` environment variable.

- cache:

  logical, whether to read/write the access token from the on-disk
  cache.

- force:

  logical, if `TRUE` re-run the flow even if a cached token exists.

## Value

invisibly, a list with `oauth_token` and `oauth_token_secret`.

## Details

You normally do not need to call this directly: any function that hits
an OAuth-protected endpoint will trigger it automatically when no cached
token is found. As before, you only need to provide your app's consumer
key and secret via the `RTUMBLR_TOKEN` environment variable
(`"consumer_key;consumer_secret"`).

## Examples

``` r
if (FALSE) { # \dontrun{
rtumblr_auth()
} # }
```
