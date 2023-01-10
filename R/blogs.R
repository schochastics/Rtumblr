#' Retrieve Blog Info
#'
#' This method returns general information about the blog, such as the title, number of posts, and other high-level data.
#'
#' @param blog name of the blog
#' @param api_key app consumer key. If NULL, attempts to load from an env variable
#'
#' @return tibble of information about  blog
#' @export
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

#' Retrieve Published Posts
#'
#' @inheritParams get_blog_info
#' @param limit The number of results to return: 1–50
#' @param offset post index to start at
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#'
#' @return a tibble of blog posts
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
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#'
#' @return a tibble of blog posts
#' @export
get_tagged <- function(tag,limit = 20,api_key = NULL,...){
  if(is.null(api_key)){
    api_key <- get_rtumblr_token_from_envvar()$consumer_key
  }
  path <- "v2/tagged"
  params <- handle_params(list(tag=tag,limit = limit,offset = offset,npf = TRUE,...))
  output_lst <- make_get_request(path,params,api_key)
  output_tbl <- dplyr::bind_rows(lapply(output_lst[["response"]],parse_blog_post_legacy))
  attr(output_tbl,"rate_limit") <- attr(output_lst,"rate_limit")
  output_tbl
}
#' Retrieve following
#'
#'This method can be used to retrieve the publicly exposed list of blogs that a blog follows, in order from most recently-followed to first.
#' Only works with your own account
#' @inheritParams get_blog_posts
#' @param app_credentials a named list containing the consumer key and consumer secret. If NULL, attempts to load from an env variable
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blogs
#' @export
get_blog_following <- function(blog,limit = 50,offset=0,app_credentials=NULL,...){
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,"/following")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  output_lst
}

#' Retrieve followers
#'
#' This method can be used to retrieve the publicly exposed list of blogs that follow a blog, in order from most recently-followed to first.
#' Only works with your own account
#' @inheritParams get_blog_following
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blogs
#' @export
get_blog_followers <- function(blog,limit = 50,offset=0,app_credentials=NULL,...){
  api_key <- NULL
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- paste0("v2/blog/",blog,"/followers")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  output_lst
}
