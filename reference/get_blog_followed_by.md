# Check If Followed By Blog This method can be used to check if one of your blogs is followed by another blog.

Check If Followed By Blog This method can be used to check if one of
your blogs is followed by another blog.

## Usage

``` r
get_blog_followed_by(blog, query, app_credentials = NULL)
```

## Arguments

- blog:

  name of the blog

- query:

  string. The name of the blog that may be following your blog

- app_credentials:

  a named list containing the consumer key and consumer secret. If NULL,
  attempts to load from an env variable

## Value

logical

## Examples

``` r
if (FALSE) { # \dontrun{
# replace "your-blog-name" with your Tumblr username
get_blog_followed_by(blog = "your-blog-name", query = "blog-to-check")
} # }
```
