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

parse_blog_post_legacy <- function(post){
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
  output[["trail"]] <- ifelse(length(post[["trail"]])==0,I(list(list())),post[["trail"]])
  output[["notes"]] <- ifelse(is.null(post[["notes"]]),I(list(list())),list(dplyr::bind_rows(post[["notes"]])))

  output[["title"]] <- NA_character_
  output[["body"]]  <- NA_character_

  type <- post[["type"]]

  if(type=="text"){
    output[["title"]] <- post[["title"]]
    output[["body"]] <- post[["body"]]
  } else if(type=="photo"){
    alt_sizes <- list(dplyr::bind_rows(post[["photos"]][["alt_sizes"]]))

  } else if(type=="link"){

  } else if(type=="audio"){

  } else if(type=="video"){

  } else{

  }
  output
}
