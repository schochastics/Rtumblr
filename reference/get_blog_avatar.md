# Retrieve a Blog Avatar You can get a blog's avatar in 9 different sizes

Retrieve a Blog Avatar You can get a blog's avatar in 9 different sizes

## Usage

``` r
get_blog_avatar(blog, size = 64)
```

## Arguments

- blog:

  name of the blog

- size:

  Integer. The size of the avatar (square, one value for both length and
  width). Must be one of the values: 16, 24, 30, 40, 48, 64, 96, 128,
  512

## Value

png of avatar

## Examples

``` r
if (FALSE) { # \dontrun{
avatar <- get_blog_avatar("schochastics")
png::writePNG("avatar_schochastics.png")
} # }
```
