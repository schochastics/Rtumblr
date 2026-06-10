# Retrieve Blog Info

This method returns general information about the blog, such as the
title, number of posts, and other high-level data.

## Usage

``` r
get_blog_info(blog, api_key = NULL)
```

## Arguments

- blog:

  name of the blog

- api_key:

  app consumer key. If NULL, attempts to load from the environment
  variable RTUMBLR_TOKEN

## Value

tibble of information about blog

## Examples

``` r
if (FALSE) { # \dontrun{
get_blog_info("schochastics")
} # }
```
