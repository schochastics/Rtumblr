# Get Posts with Tag

Get Posts with Tag

## Usage

``` r
get_posts_tag(tag, before, limit = 20, n = limit, api_key = NULL, ...)
```

## Arguments

- tag:

  tag to search for

- before:

  the timestamp of when you'd like to see posts before

- limit:

  number of results to return per request: 1-20

- n:

  maximum number of posts to retrieve across pages (default `limit`,
  i.e. a single page). Use `Inf` to keep paging.

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

- ...:

  further parameters as described here:
  <https://www.tumblr.com/docs/en/api/v2>

## Value

a list of tibbles of blog posts by format of posts

## Details

This function uses the legacy post format since it appears to not
support the new post format. The `/tagged` endpoint does not support
`offset`; pagination is done with the `before` timestamp, which is
handled automatically when `n` is larger than `limit`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_posts_tag(tag="meme")
# retrieve up to 100 posts across pages
get_posts_tag(tag="meme", n = 100)
} # }
```
