# Retrieve a User's Limits

Returns the authenticating user's rate/usage limits. Requires OAuth
authentication (see
[`rtumblr_auth()`](https://schochastics.github.io/Rtumblr/reference/rtumblr_auth.md)).

## Usage

``` r
get_user_limits(app_credentials = NULL)
```

## Arguments

- app_credentials:

  a named list containing the consumer key and consumer secret. If NULL,
  attempts to load from an env variable

## Value

a list with the user's limits as returned by the API

## Examples

``` r
if (FALSE) { # \dontrun{
get_user_limits()
} # }
```
