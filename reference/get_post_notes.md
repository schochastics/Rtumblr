# Retrieve Notes for a Post

Get the notes (likes, reblogs, replies) for a specific post.

## Usage

``` r
get_post_notes(
  blog,
  post_id,
  mode = "all",
  before_timestamp = NULL,
  api_key = NULL
)
```

## Arguments

- blog:

  name of the blog

- post_id:

  the id of the post

- mode:

  one of "all", "likes", "conversation", "rollup", or
  "reblogs_with_tags"

- before_timestamp:

  fetch notes created before this timestamp (for pagination)

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

## Value

a tibble of notes

## Examples

``` r
if (FALSE) { # \dontrun{
get_post_notes("blog-name", post_id = "1234567890")
} # }
```
