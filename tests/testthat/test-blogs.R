fake_token <- Rtumblr:::get_rtumblr_token_from_envvar(check_stop = FALSE)
fake_api <- fake_token$consumer_key

test_that("blog_info", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_blog_info("schochastics", api_key = fake_api)
  })
  expect_true(nrow(x) == 1)
  expect_true("tbl_df" %in% class(x))
})

test_that("blog_posts", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_blog_posts("schochastics", api_key = fake_api)
  })
  expect_true(nrow(x) == 1)
  expect_true("tbl_df" %in% class(x))
})

test_that("tagged_link", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_posts_tag("url")
  })
  expect_true(!is.null(x[["link"]]))
})

test_that("tagged_text", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_posts_tag("writer")
  })
  expect_true(!is.null(x[["text"]]))
})

test_that("tagged_photo", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_posts_tag("meme")
  })
  expect_true(!is.null(x[["photo"]]))
})

test_that("tagged_video", {
  httptest2::with_mock_dir("api_fixtures", {
    x <- get_posts_tag("video")
  })
  expect_true(!is.null(x[["video"]]))
})
