test_that("%||% returns left unless NULL", {
  expect_equal(Rtumblr:::`%||%`(1, 2), 1)
  expect_equal(Rtumblr:::`%||%`(NULL, 2), 2)
})

test_that("handle_params only adds supplied optional params", {
  p <- Rtumblr:::handle_params(list(a = 1))
  expect_equal(p, list(a = 1))
  p <- Rtumblr:::handle_params(list(a = 1), before = 10, offset = 5)
  expect_equal(p$before, 10)
  expect_equal(p$offset, 5)
  expect_null(p$after)
})

# helper: a fake page source of `total` items, page_size items per call
fake_source <- function(total, page_size = 20, total_field = TRUE) {
  function(limit, offset, before) {
    start <- (if (is.null(offset)) 0L else offset) + 1
    end <- min(start + min(limit, page_size) - 1, total)
    items <- if (start > total) list() else lapply(start:end, function(i) {
      list(id = i, timestamp = 1000000 - i)
    })
    resp <- list(response = list(posts = items))
    if (total_field) resp$response$total_posts <- total
    attr(resp, "rate_limit") <- tibble::tibble(x = "1")
    resp
  }
}

test_that("paginate_get stops at n", {
  src <- fake_source(total = 1000)
  out <- Rtumblr:::paginate_get(src, function(o) o$response$posts, n = 35)
  expect_equal(length(out), 35)
})

test_that("paginate_get stops when source is exhausted", {
  src <- fake_source(total = 7)
  out <- Rtumblr:::paginate_get(src, function(o) o$response$posts, n = Inf)
  expect_equal(length(out), 7)
})

test_that("paginate_get single page when n == page request", {
  src <- fake_source(total = 1000)
  out <- Rtumblr:::paginate_get(src, function(o) o$response$posts, n = 20)
  expect_equal(length(out), 20)
})

test_that("paginate_get stops at total_posts", {
  src <- fake_source(total = 13)
  out <- Rtumblr:::paginate_get(src, function(o) o$response$posts, n = Inf)
  expect_equal(length(out), 13)
})

test_that("paginate_get carries rate_limit attribute", {
  src <- fake_source(total = 5)
  out <- Rtumblr:::paginate_get(src, function(o) o$response$posts, n = Inf)
  expect_false(is.null(attr(out, "rate_limit")))
})
