#' Retrieve a User's Information
#'
#' Returns information about the authenticating user, including which blogs they own.
#' Requires OAuth authentication (see [rtumblr_auth()]).
#'
#' @inheritParams get_blog_following
#' @return a tibble with the user's information; the user's blogs are stored in the `blogs` list column
#' @export
#' @examples
#' \dontrun{
#' get_user_info()
#' }
get_user_info <- function(app_credentials = NULL){
  output_lst <- oauth_get("v2/user/info",list(),app_credentials)
  user <- output_lst[["response"]][["user"]]
  out <- tibble::tibble(
    name = user[["name"]] %||% NA_character_,
    likes = user[["likes"]] %||% NA_integer_,
    following = user[["following"]] %||% NA_integer_,
    default_post_format = user[["default_post_format"]] %||% NA_character_,
    blogs = list(user[["blogs"]])
  )
  attr(out,"rate_limit") <- attr(output_lst,"rate_limit")
  out
}

#' Retrieve a User's Limits
#'
#' Returns the authenticating user's rate/usage limits.
#' Requires OAuth authentication (see [rtumblr_auth()]).
#'
#' @inheritParams get_blog_following
#' @return a list with the user's limits as returned by the API
#' @export
#' @examples
#' \dontrun{
#' get_user_limits()
#' }
get_user_limits <- function(app_credentials = NULL){
  output_lst <- oauth_get("v2/user/limits",list(),app_credentials)
  out <- output_lst[["response"]]
  attr(out,"rate_limit") <- attr(output_lst,"rate_limit")
  out
}

#' Retrieve a User's Dashboard
#'
#' Returns the posts in the authenticating user's dashboard.
#' Requires OAuth authentication (see [rtumblr_auth()]).
#'
#' @inheritParams get_blog_posts
#' @param app_credentials a named list containing the consumer key and consumer secret. If NULL, attempts to load from an env variable
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2>
#' @return a tibble of posts
#' @export
#' @examples
#' \dontrun{
#' get_dashboard()
#' }
get_dashboard <- function(limit = 20,offset = 0,n = limit,app_credentials = NULL,...){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- "v2/user/dashboard"
  extra <- list(...)
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20), offset = off + offset, npf = TRUE),extra)
    oauth_get(path,params,app_credentials)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["posts"]],n = n)
  out <- dplyr::bind_rows(lapply(raw,parse_blog_post))
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Retrieve a User's Likes
#'
#' Returns the posts liked by the authenticating user.
#' Requires OAuth authentication (see [rtumblr_auth()]).
#'
#' @inheritParams get_blog_likes
#' @param app_credentials a named list containing the consumer key and consumer secret. If NULL, attempts to load from an env variable
#' @return a tibble of liked posts
#' @export
#' @examples
#' \dontrun{
#' get_user_likes()
#' }
get_user_likes <- function(limit = 20,offset = 0,n = limit,after,before,app_credentials = NULL,...){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- "v2/user/likes"
  extra <- list(...)
  if(!missing(after) || !missing(before)){
    params <- handle_params(c(list(limit = min(limit,20)),extra),after = after,before = before)
    output_lst <- oauth_get(path,params,app_credentials)
    return(parse_result_oauth(output_lst,"liked_posts"))
  }
  fetch_page <- function(lim,off,before){
    params <- c(list(limit = min(lim,20)),extra)
    if(!is.null(before)) params$before <- before else params$offset <- off + offset
    oauth_get(path,params,app_credentials)
  }
  raw <- paginate_get(fetch_page,function(o) o[["response"]][["liked_posts"]],n = n)
  out <- dplyr::bind_rows(raw)
  attr(out,"rate_limit") <- attr(raw,"rate_limit")
  out
}

#' Retrieve the Blogs a User Is Following
#'
#' Returns the blogs followed by the authenticating user.
#' Requires OAuth authentication (see [rtumblr_auth()]).
#'
#' @inheritParams get_blog_following
#' @return a tibble of blogs
#' @export
#' @examples
#' \dontrun{
#' get_user_following()
#' }
get_user_following <- function(limit = 20,offset = 0,n = limit,app_credentials = NULL,...){
  if(is.null(app_credentials)){
    app_credentials <- get_rtumblr_token_from_envvar()
  }
  path <- "v2/user/following"
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
