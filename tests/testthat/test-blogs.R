fake_token <- Rtumblr:::get_rtumblr_token_from_envvar(check_stop = FALSE)
fake_api <- fake_token$consumer_key

test_that("blog_info", {
  vcr::use_cassette("blog_info_default", {
    x <- get_blog_info("schochastics",api_key = fake_api)
  })
  expect_true(nrow(x) == 1)
  expect_true("tbl_df" %in% class(x))
})

# test_that("blog_avatar", {
#   vcr::use_cassette("blog_avatar_default", {
#     x <- dim(get_blog_avatar("schochastics"))
#   })
#   expect_true(x[1] == 64)
#   expect_true(x[2] == 64)
#   expect_true(x[3] == 3)
# })

test_that("blog_posts", {
  vcr::use_cassette("blog_posts_default", {
    x <- get_blog_posts("schochastics",api_key = fake_api)
  })
  expect_true(nrow(x) == 1)
  expect_true("tbl_df" %in% class(x))
})

test_that("tagged_link", {
  vcr::use_cassette("tagged_link_default", {
    x <- get_posts_tag("url")
  })
  expect_true(!is.null(x[["link"]]))
})

test_that("tagged_text", {
  vcr::use_cassette("tagged_text_default", {
    x <- get_posts_tag("writer")
  })
  expect_true(!is.null(x[["text"]]))
})

test_that("tagged_photo", {
  vcr::use_cassette("tagged_photo_default", {
    x <- get_posts_tag("meme")
  })
  expect_true(!is.null(x[["photo"]]))
})

test_that("tagged_video", {
  vcr::use_cassette("tagged_video_default", {
    x <- get_posts_tag("video")
  })
  expect_true(!is.null(x[["video"]]))
})
