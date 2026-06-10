# Retrieve Published Posts

Retrieve Published Posts

## Usage

``` r
get_blog_posts(blog, limit = 20, offset = 0, n = limit, api_key = NULL, ...)
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

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

- ...:

  further parameters as described here:
  <https://www.tumblr.com/docs/en/api/v2>

## Value

a tibble of blog posts

## Details

this function uses the new post format (npf:
<https://www.tumblr.com/docs/npf>). Pagination is handled automatically:
it pages through `offset` up to the API cap of 1000, then continues via
the `before` timestamp, until `n` posts are collected or the blog is
exhausted.

## Examples

``` r
if (FALSE) { # \dontrun{
# replace "blog-name" with a Tumblr username
get_blog_posts(blog = "blog-name")
# retrieve all posts of a blog
get_blog_posts(blog = "blog-name", n = Inf)
} # }
```
