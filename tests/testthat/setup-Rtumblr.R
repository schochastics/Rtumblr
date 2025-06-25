library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")

if (!nzchar(Sys.getenv("RTUMBLR_TOKEN"))) {
  if (dir.exists(vcr_dir)) {
    # Fake API token to fool our package
    Sys.setenv("RTUMBLR_TOKEN" = "aaabbbcccdddeee;aaabbbcccdddeee")
  } else {
    # If there's no mock files nor API token, impossible to run tests
    stop("No API key nor cassettes, tests cannot be run.",
         call. = FALSE)
  }
}

vcr::vcr_configure(
  filter_query_parameters = list("api_key" = ""),
  dir = vcr::vcr_test_path("fixtures")
)
