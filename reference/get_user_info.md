# Retrieve a User's Information

Returns information about the authenticating user, including which blogs
they own. Requires OAuth authentication (see
[`rtumblr_auth()`](https://schochastics.github.io/Rtumblr/reference/rtumblr_auth.md)).

## Usage

``` r
get_user_info(app_credentials = NULL)
```

## Arguments

- app_credentials:

  a named list containing the consumer key and consumer secret. If NULL,
  attempts to load from an env variable

## Value

a tibble with the user's information; the user's blogs are stored in the
`blogs` list column

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_info()
} # }
```
