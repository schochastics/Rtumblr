# Rtumblr 0.2.0

* The HTTP backend has been migrated from `httr` to `httr2`. OAuth 1.0a signing and
  the interactive browser authentication flow are now implemented in-package; the
  user-facing setup is unchanged (set `RTUMBLR_TOKEN="consumer_key;consumer_secret"`).
* `rtumblr_auth()` is exported to (re-)run the OAuth flow explicitly. The access token
  is now cached under `tools::R_user_dir("Rtumblr", "cache")` instead of `.httr-oauth`,
  so you will be asked to authorize once after upgrading. Power users can supply a
  pre-issued access token via a four-field `RTUMBLR_TOKEN`
  (`"consumer_key;consumer_secret;oauth_token;oauth_token_secret"`) to skip the browser.
* `get_blog_posts()` now paginates automatically. Pass `n` to control how many posts to
  retrieve across pages (`n = Inf` fetches the whole blog). The same `n` argument was
  added to `get_blog_likes()`, `get_blog_following()`, `get_blog_followers()` and
  `get_blog_blocks()`.
* The default `limit` for `get_blog_posts()` (and the following/followers helpers) is now
  `20`, matching the Tumblr API maximum (the previous default of `50` was silently capped).
* Fixed a bug where `get_blog_blocks()`, `get_blog_following()` and `get_blog_followers()`
  ignored the `offset` argument.
* New read-only endpoints: `get_user_info()`, `get_user_limits()`, `get_dashboard()`,
  `get_user_likes()`, `get_user_following()`, `get_post()` and `get_post_notes()`.
* Tests now use `httptest2` instead of `vcr`.

# Rtumblr 0.1.1

* Updates due to changes in the `vcr` package.

# Rtumblr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
