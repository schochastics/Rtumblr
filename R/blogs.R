#' Retrieve Blog Info
#'
#' This method returns general information about the blog, such as the title, number of posts, and other high-level data.
#'
#' @param blog name of the blog
#' @param api_key
#'
#' @return tibble of information about  blog
#' @export
get_blog_info <- function(blog,api_key = NULL){
  path <- paste0("v2/blog/",blog,".tumblr.com/info")
  output <- make_get_request(path,params = list(),api_key)
  rt <- attr(output,"rate_limit")
  parsed <- parse_blog_info(output[["response"]][["blog"]])
  attr(parsed,"rate_limit") <- rt
  parsed
}

#' Retrieve Blog's Likes
#'
#' This method can be used to retrieve the publicly exposed likes from a blog.
#'
#' @inheritParams get_blog_info
#' @param limit The number of results to return: 1–20
#' @param before Retrieve posts liked before the specified timestamp
#' @param after Retrieve posts liked after the specified timestamp
#' @param offset Liked post number to start at
#' @details  You can only provide either before, after, or offset. If you provide more than one of these options together you will get an error.
#' You can still use limit with any of those three options to limit your result set.
#' When using the offset parameter the maximum limit on the offset is 1000. If you would like to get more results than that use either before or after.
#'
#' @return tibble of liked blog posts
#' @export
#'
get_blog_likes <- function(blog,limit = 20, before,after,offset,api_key = NULL){
  path <- paste0("v2/blog/",blog,"/likes")
  params <- handle_params(list(limit = limit),before,after,offset)
  output <- make_get_request(path,params,api_key)
  output
}

#' Retrieve Published Posts
#'
#' @inheritParams get_blog_likes
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blog posts
#' @export
get_blog_posts <- function(blog,limit = 20,api_key = NULL,...){
  path <- paste0("v2/blog/",blog,"/posts")
  params <- handle_params(list(limit = limit,npf = TRUE,...))
  output <- make_get_request(path,params,api_key)
  output
}


parse_blog_info <- function(blog){
  empty <- empty[["blog"]]
  singular_fields <- c("title","posts","name","updated","description",
                       "ask","ask_anon","followed","likes","is_blocked_from_primary",
                       "url","avatar")
  singular_list <- lapply(blog[singular_fields], function(x) ifelse(is.null(x), NA, x))
  names(singular_list) <- singular_fields
  output <- dplyr::bind_cols(tibble::as_tibble(singular_list),dplyr::bind_rows(blog[["theme"]]) )
}


