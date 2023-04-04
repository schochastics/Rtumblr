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
#' @param limit number of blocks to retrieve (1-20)
#' @param offset block number to start at (default 0)
#' @return tibble of blocked blogs
#' @export
#' @examples
#' \dontrun{
#' # replace "your-blog-name" with your Tumblr username
#' get_blog_blocks(blog = "your-blog-name")
#' }
get_blog_blocks <- function(blog,limit = 20,offset = 0,app_credentials = NULL){
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/blocks")
  params <- list(limit = limit,offset = 0)
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  parse_result_oauth(output_lst,"blocked_tumblelogs")
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
get_blog_likes <- function(blog,limit = 20,offset=0,after,before,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/likes")
  params <- handle_params(list(limit = limit,offset = offset,...),after = after,before = before)
  output_lst <- make_get_request(path,params,api_key = api_key)
  parse_result_oauth(output_lst,"liked_posts")
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
get_blog_following <- function(blog,limit = 50,offset=0,app_credentials=NULL,...){
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/following")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  parse_result_oauth(output_lst,"blogs")
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
get_blog_followers <- function(blog,limit = 50,offset=0,app_credentials=NULL,...){
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/followers")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  parse_result_oauth(output_lst,"users")
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
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,".tumblr.com/followed_by")
  params <- list(query = query)
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  output_lst[["response"]][["followed_by"]]
}


#' Retrieve Published Posts
#'
#' @inheritParams get_blog_info
#' @param limit The number of results to return: 1–50
#' @param offset post index to start at
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @details this function uses the new post format (npf: <https://www.tumblr.com/docs/npf>)
#' @return a tibble of blog posts
#' @examples
#' \dontrun{
#' # replace "blog-name" with a Tumblr username
#' get_blog_posts(blog = "blog-name")
#' }

#' @export
get_blog_posts <- function(blog,limit = 50,offset = 0,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- paste0("v2/blog/",blog,"/posts")
  params <- handle_params(list(limit = limit,offset = offset,npf = TRUE,...))
  output_lst <- make_get_request(path,params,api_key)
  output_tbl <- dplyr::bind_rows(lapply(output_lst[["response"]][["posts"]],parse_blog_post))
  attr(output_tbl,"rate_limit") <- attr(output_lst,"rate_limit")
  output_tbl
}

#' Get Posts with Tag
#'
#' @inheritParams get_blog_posts
#' @param tag tag to search for
#' @param before the timestamp of when you'd like to see posts before
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @details This function uses the legacy post format since it appears to not support the new post format
#' @return a list of tibbles of blog posts by format of posts
#' @export
#' @examples
#' \dontrun{
#' get_posts_tag(tag="meme")
#' }
get_posts_tag <- function(tag,before,limit = 20,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- "v2/tagged"
  params <- handle_params(list(tag=tag,limit = limit,...), before = before)
  output_lst <- make_get_request(path,params,api_key)
  output_parsed <- lapply(output_lst[["response"]],parse_blog_post_legacy)
  ident <- sapply(output_parsed,function(x) x[["type"]])

  output_srt <- vector("list",5)
  names(output_srt) <- c("text","photo","link","audio","video")
  for(type in names(output_srt)){
    output_srt[[type]] <- dplyr::bind_rows(output_parsed[ident==type])
  }
  output_srt
}

