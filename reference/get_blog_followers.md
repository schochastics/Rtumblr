# Retrieve followers

This method can be used to retrieve the publicly exposed list of blogs
that follow a blog, in order from most recently-followed to first.
**Only works with your own blog**

## Usage

``` r
get_blog_followers(
  blog,
  limit = 20,
  offset = 0,
  n = limit,
  app_credentials = NULL,
  ...
)
```

## Arguments

- blog:

  name of the blog

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

a tibble of blogs

## Examples

``` r
if (FALSE) { # \dontrun{
# replace "your-blog-name" with your Tumblr username
get_blog_followers(blog = "your-blog-name")
} # }
```
