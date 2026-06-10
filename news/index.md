# Changelog

## Rtumblr 0.2.0

- The HTTP backend has been migrated from `httr` to `httr2`. OAuth 1.0a
  signing and the interactive browser authentication flow are now
  implemented in-package; the user-facing setup is unchanged (set
  `RTUMBLR_TOKEN="consumer_key;consumer_secret"`).
- [`rtumblr_auth()`](https://schochastics.github.io/Rtumblr/reference/rtumblr_auth.md)
  is exported to (re-)run the OAuth flow explicitly. The access token is
  now cached under `tools::R_user_dir("Rtumblr", "cache")` instead of
  `.httr-oauth`, so you will be asked to authorize once after upgrading.
  Power users can supply a pre-issued access token via a four-field
  `RTUMBLR_TOKEN`
  (`"consumer_key;consumer_secret;oauth_token;oauth_token_secret"`) to
  skip the browser.
- [`get_blog_posts()`](https://schochastics.github.io/Rtumblr/reference/get_blog_posts.md)
  now paginates automatically. Pass `n` to control how many posts to
  retrieve across pages (`n = Inf` fetches the whole blog). The same `n`
  argument was added to
  [`get_blog_likes()`](https://schochastics.github.io/Rtumblr/reference/get_blog_likes.md),
  [`get_blog_following()`](https://schochastics.github.io/Rtumblr/reference/get_blog_following.md),
  [`get_blog_followers()`](https://schochastics.github.io/Rtumblr/reference/get_blog_followers.md)
  and
  [`get_blog_blocks()`](https://schochastics.github.io/Rtumblr/reference/get_blog_blocks.md).
- The default `limit` for
  [`get_blog_posts()`](https://schochastics.github.io/Rtumblr/reference/get_blog_posts.md)
  (and the following/followers helpers) is now `20`, matching the Tumblr
  API maximum (the previous default of `50` was silently capped).
- Fixed a bug where
  [`get_blog_blocks()`](https://schochastics.github.io/Rtumblr/reference/get_blog_blocks.md),
  [`get_blog_following()`](https://schochastics.github.io/Rtumblr/reference/get_blog_following.md)
  and
  [`get_blog_followers()`](https://schochastics.github.io/Rtumblr/reference/get_blog_followers.md)
  ignored the `offset` argument.
- New read-only endpoints:
  [`get_user_info()`](https://schochastics.github.io/Rtumblr/reference/get_user_info.md),
  [`get_user_limits()`](https://schochastics.github.io/Rtumblr/reference/get_user_limits.md),
  [`get_dashboard()`](https://schochastics.github.io/Rtumblr/reference/get_dashboard.md),
  [`get_user_likes()`](https://schochastics.github.io/Rtumblr/reference/get_user_likes.md),
  [`get_user_following()`](https://schochastics.github.io/Rtumblr/reference/get_user_following.md),
  [`get_post()`](https://schochastics.github.io/Rtumblr/reference/get_post.md)
  and
  [`get_post_notes()`](https://schochastics.github.io/Rtumblr/reference/get_post_notes.md).
- Tests now use `httptest2` instead of `vcr`.

## Rtumblr 0.1.1

CRAN release: 2025-07-23

- Updates due to changes in the `vcr` package.

## Rtumblr 0.1.0

CRAN release: 2023-04-05

- Added a `NEWS.md` file to track changes to the package.
