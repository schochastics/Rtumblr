# Retrieve a User's Dashboard

Returns the posts in the authenticating user's dashboard. Requires OAuth
authentication (see
[`rtumblr_auth()`](https://schochastics.github.io/Rtumblr/reference/rtumblr_auth.md)).

## Usage

``` r
get_dashboard(limit = 20, offset = 0, n = limit, app_credentials = NULL, ...)
```

## Arguments

- limit:

  The number of results to return per request: 1-20

- offset:

  post index to start at

- n:

  maximum number of posts to retrieve across pages (default `limit`,
  i.e. a single page). Use `Inf` to retrieve all posts of the blog.

- app_credentials:

  a named list containing the consumer key and consumer secret. If NULL,
  attempts to load from an env variable

- ...:

  further parameters as described here:
  <https://www.tumblr.com/docs/en/api/v2>

## Value

a tibble of posts

## Examples

``` r
if (FALSE) { # \dontrun{
get_dashboard()
} # }
```
