library("vcr")
vcr::vcr_configure(
  filter_sensitive_data = list("<<<API TOKEN>>>" = get_rtumblr_token_from_envvar()$consumer_key),
  dir = vcr::vcr_test_path("fixtures")
)
vcr::check_cassette_names()
