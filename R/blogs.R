#' Retrieve Blog Info
#'
#' This method returns general information about the blog, such as the title, number of posts, and other high-level data.
#'
#' @param blog name of the blog
#' @param api_key app consumer key. If NULL, attempts to load from the environment variable RTUMBLR_TOKEN
#'
#' @return tibble of information about  blog
#' @export
#' @examples
#' \dontrun{
#' get_blog_info("schochastics")
#' }
get_blog_info <- function(blog,api_key = NULL){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/info")
  output <- make_get_request(path,params = list(),api_key)
  rt <- attr(output,"rate_limit")
  parsed <- parse_blog_info(output[["response"]][["blog"]])
  attr(parsed,"rate_limit") <- rt
  parsed
}
#' Retrieve a Blog Avatar
#' You can get a blog's avatar in 9 different sizes
#' @inheritParams get_blog_info
#' @param size Integer. The size of the avatar (square, one value for both length and width). Must be one of the values:  16, 24, 30, 40, 48, 64, 96, 128, 512
#' @return png of avatar
#' @export
#' @examples
#' \dontrun{
#' avatar <- get_blog_avatar("schochastics")
#' png::writePNG("avatar_schochastics.png")
#' }
get_blog_avatar <- function(blog,size=64){
  if(!size%in%c(16, 24, 30, 40, 48, 64, 96, 128, 512)){
    stop("size must be one of 16, 24, 30, 40, 48, 64, 96, 128, 512")
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/avatar/",size)
  make_get_request(path,params = list())
}

#' Retrieve Blogs Blocks
#' @description Get the blogs that the requested blog is currently blocking. The requesting user must be an admin of the blog to retrieve this list.  Note that this endpoint is rate limited to 60 requests per minute.
#' @inheritParams get_blog_following
#' @param limit number of blocks to retrieve per request (1-20)
#' @param offset block number to start at (default 0)
#' @param n maximum number of blocks to retrieve across pages (default `limit`, i.e. a single page). Use `Inf` to retrieve all.
#' @return tibble of blocked blogs
#' @export
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_blocks(blog = "your-blog-name")
#' }
get_blog_blocks <- function(blog,limit = 20,offset = 0,n = limit,app_credentials = NULL){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/blocks")
  fetch_page <- function(lim,off,before){
    params <- list(limit = min(lim,20), offset = off + offset)
    oauth_get(path,params,app_credentials)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["blocked_tumblelogs"]],n = n)
  out <- dplyr::bind_rows(raw)
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Retrieve Blog's Likes
#'
#' This method can be used to retrieve the publicly exposed likes from a blog. **Seems to work only for your own blog**
#'
#' @inheritParams get_blog_following
#' @inheritParams get_blog_info
#' @param before integer. Retrieve posts liked before the specified timestamp
#' @param after integer. Retrieve posts liked after the specified timestamp
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @details You can only provide either before, after, or offset.
#' If you provide more than one of these options together you will get an error.
#' You can still use limit with any of those three options to limit your result set.
#' When using the offset parameter the maximum limit on the offset is 1000. If you would like to get more results than that use either before or after.
#' @return tibble of liked posts
#' @export
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_likes(blog = "your-blog-name")
#' }
get_blog_likes <- function(blog,limit = 20,offset=0,n = limit,after,before,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/likes")
  extra <- list(...)
  # before/after are mutually exclusive with offset; if supplied, do a single request
  if(!missing(after) || !missing(before)){
    params <- handle_params(c(list(limit = min(limit,20)),extra),after = after,before = before)
    output_lst <- make_get_request(path,params,api_key = api_key)
    return(parse_result_oauth(output_lst,"liked_posts"))
  }
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20)),extra)
    if(!is.null(before)) params$before <- before else params$offset <- off + offset
    make_get_request(path,params,api_key = api_key)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["liked_posts"]],n = n)
  out <- dplyr::bind_rows(raw)
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Retrieve following
#'
#'This method can be used to retrieve the publicly exposed list of blogs that a blog follows, in order from most recently-followed to first.
#' **Only works with your own account**
#' @inheritParams get_blog_posts
#' @param app_credentials a named list containing the consumer key and consumer secret. If NULL, attempts to load from an env variable
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#'
#' @return a tibble of blogs
#' @export
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_following(blog = "your-blog-name")
#' }
get_blog_following <- function(blog,limit = 20,offset=0,n = limit,app_credentials=NULL,...){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/following")
  extra <- list(...)
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20), offset = off + offset),extra)
    oauth_get(path,params,app_credentials)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["blogs"]],n = n)
  out <- dplyr::bind_rows(raw)
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Retrieve followers
#'
#' This method can be used to retrieve the publicly exposed list of blogs that follow a blog, in order from most recently-followed to first.
#' **Only works with your own blog**
#' @inheritParams get_blog_following
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#'
#' @return a tibble of blogs
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_followers(blog = "your-blog-name")
#' }
#' @export
get_blog_followers <- function(blog,limit = 20,offset=0,n = limit,app_credentials=NULL,...){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/followers")
  extra <- list(...)
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20), offset = off + offset),extra)
    oauth_get(path,params,app_credentials)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["users"]],n = n)
  out <- dplyr::bind_rows(raw)
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Check If Followed By Blog
#' This method can be used to check if one of your blogs is followed by another blog.
#'
#' @inheritParams get_blog_following
#' @param query string. The name of the blog that may be following your blog
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_followed_by(blog = "your-blog-name", query = "blog-to-check")
#' }
#' @return logical
#' @export
get_blog_followed_by <- function(blog,query,app_credentials = NULL){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/followed_by")
  params <- list(query = query)
  output_lst <- oauth_get(path,params,app_credentials)
  output_lst[["response"]][["followed_by"]]
}


#' Retrieve Published Posts
#'
#' @inheritParams get_blog_info
#' @param limit The number of results to return per request: 1-20
#' @param offset post index to start at
#' @param n maximum number of posts to retrieve across pages (default `limit`, i.e. a single page). Use `Inf` to retrieve all posts of the blog.
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @details this function uses the new post format (npf: <https://www.tumblr.com/docs/npf>).
#' Pagination is handled automatically: it pages through `offset` up to the API cap of 1000,
#' then continues via the `before` timestamp, until `n` posts are collected or the blog is exhausted.
#' @return a tibble of blog posts
#' @examples
#' \dontrun{
#' # replace "blog-name" with a Tumblr username
#' get_blog_posts(blog = "blog-name")
#' # retrieve all posts of a blog
#' get_blog_posts(blog = "blog-name", n = Inf)
#' }

#' @export
get_blog_posts <- function(blog,limit = 20,offset = 0,n = limit,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,"/posts")
  extra <- list(...)
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20), npf = TRUE),extra)
    if(!is.null(before)) params$before <- before else params$offset <- off + offset
    make_get_request(path,params,api_key)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["posts"]],n = n)
  output_tbl <- dplyr::bind_rows(lapply(raw,parse_blog_post))
  attr(output_tbl,"rate_limit") <- attr(raw,"rate_limit")
  output_tbl
}

#' Retrieve a Single Post
#'
#' Fetch a specific post by its id (new post format).
#' @inheritParams get_blog_info
#' @param post_id the id of the post to retrieve
#' @return a tibble with the post
#' @export
#' @examples
#' \dontrun{
#' get_post("blog-name", post_id = "1234567890")
#' }
get_post <- function(blog,post_id,api_key = NULL){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,"/posts/",post_id)
  output_lst <- make_get_request(path,params = list(npf = TRUE),api_key)
  posts <- output_lst[["response"]][["posts"]]
  out <- dplyr::bind_rows(lapply(posts,parse_blog_post))
  attr(out,"rate_limit") <- attr(output_lst,"rate_limit")
  out
}

#' Retrieve Notes for a Post
#'
#' Get the notes (likes, reblogs, replies) for a specific post.
#' @inheritParams get_blog_info
#' @param post_id the id of the post
#' @param mode one of "all", "likes", "conversation", "rollup", or "reblogs_with_tags"
#' @param before_timestamp fetch notes created before this timestamp (for pagination)
#' @return a tibble of notes
#' @export
#' @examples
#' \dontrun{
#' get_post_notes("blog-name", post_id = "1234567890")
#' }
get_post_notes <- function(blog,post_id,mode = "all",before_timestamp = NULL,api_key = NULL){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,"/notes")
  params <- list(id = post_id,mode = mode)
  if(!is.null(before_timestamp)) params$before_timestamp <- before_timestamp
  output_lst <- make_get_request(path,params,api_key)
  out <- dplyr::bind_rows(output_lst[["response"]][["notes"]])
  attr(out,"rate_limit") <- attr(output_lst,"rate_limit")
  out
}

#' Get Posts with Tag
#'
#' @inheritParams get_blog_posts
#' @param tag tag to search for
#' @param before the timestamp of when you'd like to see posts before
#' @param limit number of results to return per request: 1-20
#' @param n maximum number of posts to retrieve across pages (default `limit`, i.e. a single page). Use `Inf` to keep paging.
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @details This function uses the legacy post format since it appears to not support the new post format.
#' The `/tagged` endpoint does not support `offset`; pagination is done with the `before`
#' timestamp, which is handled automatically when `n` is larger than `limit`.
#' @return a list of tibbles of blog posts by format of posts
#' @export
#' @examples
#' \dontrun{
#' get_posts_tag(tag="meme")
#' # retrieve up to 100 posts across pages
#' get_posts_tag(tag="meme", n = 100)
#' }
get_posts_tag <- function(tag,before,limit = 20,n = limit,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- "v2/tagged"
  extra <- list(...)
  current_before <- if(missing(before)) NULL else before
  collected <- list()
  seen <- character()
  output_lst <- NULL
  repeat{
    remaining <- n - length(collected)
    if(remaining <= 0) break
    params <- c(list(tag = tag,limit = min(limit,20,remaining)),extra)
    if(!is.null(current_before)) params$before <- current_before
    output_lst <- make_get_request(path,params,api_key)
    items <- output_lst[["response"]]
    if(length(items) == 0) break
    # the tagged feed can return duplicates across the before boundary
    ids <- vapply(items,function(x) as.character(x[["id_string"]] %||% x[["id"]] %||% ""),character(1))
    keep <- !(ids %in% seen)
    items <- items[keep]
    if(length(items) == 0) break
    seen <- c(seen,ids[keep])
    collected <- c(collected,items)
    ts <- vapply(items,function(x) as.numeric(x[["timestamp"]] %||% NA_real_),numeric(1))
    if(all(is.na(ts))) break
    next_before <- min(ts,na.rm = TRUE)
    # stop if we cannot page back any further
    if(!is.null(current_before) && next_before >= current_before) break
    current_before <- next_before
  }
  if(length(collected) > n) collected <- collected[seq_len(n)]
  output_parsed <- lapply(collected,parse_blog_post_legacy)
  ident <- sapply(output_parsed,function(x) x[["type"]])

  output_srt <- vector("list",5)
  names(output_srt) <- c("text","photo","link","audio","video")
  for(type in names(output_srt)){
    output_srt[[type]] <- dplyr::bind_rows(output_parsed[ident==type])
  }
  attr(output_srt,"rate_limit") <- attr(output_lst,"rate_limit")
  output_srt
}
