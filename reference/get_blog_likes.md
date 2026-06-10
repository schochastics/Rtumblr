# Retrieve Blog's Likes

This method can be used to retrieve the publicly exposed likes from a
blog. **Seems to work only for your own blog**

## Usage

``` r
get_blog_likes(
  blog,
  limit = 20,
  offset = 0,
  n = limit,
  after,
  before,
  api_key = NULL,
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

- after:

  integer. Retrieve posts liked after the specified timestamp

- before:

  integer. Retrieve posts liked before the specified timestamp

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

- ...:

  further parameters as described here:
  <https://www.tumblr.com/docs/en/api/v2>

## Value

tibble of liked posts

## Details

You can only provide either before, after, or offset. If you provide
more than one of these options together you will get an error. You can
still use limit with any of those three options to limit your result
set. When using the offset parameter the maximum limit on the offset is
1000. If you would like to get more results than that use either before
or after.

## Examples

``` r
if (FALSE) { # \dontrun{
# replace "your-blog-name" with your Tumblr username
get_blog_likes(blog = "your-blog-name")
} # }
```
