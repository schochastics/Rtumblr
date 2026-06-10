# Rtumblr

Rtumblr is a wrapper for the official tumblr API. (An [existing
CRAN](https://CRAN.R-project.org/package=tumblR) package has been
archived)

## Installation

You can install the development version of Rtumblr like so:

``` r

devtools::install_github("schochastics/Rtumblr")
```

## API keys

You need to register an app here: <https://www.tumblr.com/oauth/apps>
Then, create an environment variable called “RTUMBLR_TOKEN” (for
instance in `.Renviron`) from the consumer_key and the consumer_secret
as follows. If you have

``` r

consumer_key = 123456789
consumer_secret = abcdefghi
```

Then your environment variable RTUMBLR_TOKEN should be
`123456789;abcdefghi`

## Usage

Most API endpoints only work with your own account. The ones already
implemented include:

- [`get_blog_blocks()`](https://schochastics.github.io/Rtumblr/reference/get_blog_blocks.md):
  get a list of accounts you blocked
- [`get_blog_likes()`](https://schochastics.github.io/Rtumblr/reference/get_blog_likes.md):
  get a list of posts you liked
- [`get_blog_followers()`](https://schochastics.github.io/Rtumblr/reference/get_blog_followers.md):
  get your followers
- [`get_blog_following()`](https://schochastics.github.io/Rtumblr/reference/get_blog_following.md):
  get accounts you follow

Implemented endpoints that work with any account are:

- [`get_blog_avatar()`](https://schochastics.github.io/Rtumblr/reference/get_blog_avatar.md):
  get the avatar of a blog
- [`get_blog_info()`](https://schochastics.github.io/Rtumblr/reference/get_blog_info.md):
  get the general info of a blog
- [`get_blog_posts()`](https://schochastics.github.io/Rtumblr/reference/get_blog_posts.md):
  get the posts of a blog
- [`get_posts_tag()`](https://schochastics.github.io/Rtumblr/reference/get_posts_tag.md):
  get posts with a specific tag.

All function return a tibble (or list of tibble). To get information
about columns, see the official API documentation:
<https://www.tumblr.com/docs/en/api/v2>
