# Retrieve a Single Post

Fetch a specific post by its id (new post format).

## Usage

``` r
get_post(blog, post_id, api_key = NULL)
```

## Arguments

- blog:

  name of the blog

- post_id:

  the id of the post to retrieve

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

## Value

a tibble with the post

## Examples

``` r
if (FALSE) { # \dontrun{
get_post("blog-name", post_id = "1234567890")
} # }
```
