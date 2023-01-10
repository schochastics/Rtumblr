#' Retrieve Blog Info
#'
#' This method returns general information about the blog, such as the title, number of posts, and other high-level data.
#'
#' @param blog name of the blog
#' @param api_key app consumer key
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

#' Retrieve Published Posts
#'
#' @inheritParams get_blog_info
#' @param limit The number of results to return: 1–50
#' @param offset post index to start at
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blog posts
#' @export
get_blog_posts <- function(blog,limit = 50,offset = 0,api_key = NULL,...){
  path <- paste0("v2/blog/",blog,"/posts")
  params <- handle_params(list(limit = limit,offset = offset,npf = TRUE,...))
  output_lst <- make_get_request(path,params,api_key)
  output_tbl <- dplyr::bind_rows(lapply(output_lst[["response"]][["posts"]],parse_blog_post))
  attr(output_tbl,"rate_limit") <- attr(output_lst,"rate_limit")
  output_tbl
}

#' Retrieve following
#'
#'This method can be used to retrieve the publicly exposed list of blogs that a blog follows, in order from most recently-followed to first.
#' Only works ith your own account
#' @inheritParams get_blog_posts
#' @param app_credentials a named list containing the consumer key and consumer secret
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blogs
#' @export
get_blog_following <- function(blog,limit = 50,offset=0,app_credentials,...){
  api_key <- NULL
  path <- paste0("v2/blog/",blog,"/following")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  output_lst
}

#' Retrieve following
#'
#'This method can be used to retrieve the publicly exposed list of blogs that follow a blog, in order from most recently-followed to first.
#' Only works ith your own account
#' @inheritParams get_blog_following
#' @param ... further parameters as described here: <https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts>
#'
#' @return a tibble of blogs
#' @export
get_blog_followers <- function(blog,limit = 50,offset=0,app_credentials,...){
  api_key <- NULL
  path <- paste0("v2/blog/",blog,"/followers")
  params <- handle_params(list(limit = limit,offset = 0,...))
  oauth_head <- handle_oauth1(app_credentials,path,params)
  output_lst <- make_get_request(path = path,params = oauth_head$params,api_key = api_key,header = oauth_head$header)
  output_lst
}


parse_blog_info <- function(blog){
  empty_blog <- empty[["blog_info"]]
  singular_fields <- c("title","posts","name","updated","description",
                       "ask","ask_anon","followed","likes","is_blocked_from_primary",
                       "url","avatar")
  singular_list <- lapply(blog[singular_fields], function(x) ifelse(is.null(x), NA, x))
  names(singular_list) <- singular_fields
  output <- dplyr::bind_cols(tibble::as_tibble(singular_list),dplyr::bind_rows(blog[["theme"]]) )
}

parse_blog_post <- function(post){
  empty_post <- empty[["blog_post"]][-1,]
  singular_fields <- c("type", "original_type", "is_blocks_post_format", "blog_name",
                       "id", "id_string", "post_url", "slug", "date", "timestamp", "state",
                       "reblog_key", "short_url", "summary", "should_open_in_legacy",
                       "recommended_source", "recommended_color", "note_count", "can_like",
                       "interactability_reblog", "can_reblog", "can_send_in_message",
                       "can_reply", "display_avatar")
  singular_list <- lapply(post[singular_fields], function(x) ifelse(is.null(x), NA, x))
  names(singular_list) <- singular_fields

  blog <- tibble::as_tibble(lapply(post[["blog"]],function(x) ifelse(is.null(x), NA, x)))
  tags <- list(unlist(post[["tags"]]))
  content <- lapply(post[["content"]],parse_content)
  output <- tibble::as_tibble(singular_list)
  output[["blog"]] <- list(blog)
  output[["tags"]] <- tags
  output[["content"]] <- list(content)
  output[["layout"]] <- ifelse(length(post[["layout"]])==0,I(list(list())),post[["layout"]])
  output[["trail"]] <- ifelse(length(post[["trail"]])==0,I(list(list())),post[["trail"]])
  output[["notes"]] <- ifelse(is.null(post[["notes"]]),I(list(list())),list(dplyr::bind_rows(post[["notes"]])))
  output <- dplyr::bind_rows(empty_post,output)
  output
}

parse_content <- function(content){
  type <- content[["type"]]
  if(type=="image"){
    output <- empty[["image"]]
    fields <- names(output)[-(1:2)]
    media <- dplyr::bind_rows(parse_media(content[["media"]]))
    colors <- ifelse(is.null(content[["colors"]]),NA,paste0("#",content[["colors"]],collapse = ";"))
    output[["media"]] <- list(media)
    output[["colors"]] <- colors
    output[fields] <- lapply(content[fields], function(x) ifelse(is.null(x), NA, x))
  } else if(type=="text"){
    formatting <- list(dplyr::bind_rows(content[["formatting"]]))
    subtype <- ifelse(is.null(content[["subtype"]]),NA,content[["subtype"]])
    indent_level <- ifelse(is.null(content[["indent_level"]]),NA,content[["indent_level"]])
    output <- tibble::tibble(
      text = content[["text"]],
      formatting = formatting,
      subtype = subtype,
      indent_level = indent_level
    )
  } else if(type=="link"){
    output <- empty[["link"]]
    fields <- names(output)
    output[fields] <- lapply(content[fields], function(x) ifelse(is.null(x), NA, x))
  } else if(type=="audio"){
    output <- empty[["audio"]]
    fields <- names(output)
    output[fields] <- lapply(content[fields], function(x) ifelse(is.null(x), NA, x))
  } else if(type=="video"){
    output <- empty[["video"]]
    fields <- names(output)
    output[fields] <- lapply(content[fields], function(x) ifelse(is.null(x), NA, x))
  }
  output[["type"]] <- type
  output
}

parse_media <- function(media){
  media <- lapply(media,function(x){
    x[["colors"]] <- NULL
    x
    })
  media
}
