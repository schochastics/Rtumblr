## code to prepare `empty` dataset goes here
empty <- list()

empty[["blog_info"]] <- tibble::tibble(
  title = NA_character_,
  posts = NA_integer_,
  name = NA_character_,
  updated = NA_integer_,
  description = NA_character_,
  ask = NA,
  ask_anon = NA,
  followed = NA,
  likes = NA_integer_,
  is_blocked_from_primary = NA,
  avatar = I(list(list())),
  url = NA_character_
)

empty[["blog_post"]] <- tibble::tibble(
  type = NA_character_,
  original_type = NA_character_,
  is_blocks_post_format = NA,
  blog_name = NA_character_,
  blog = I(list(list())),
  id = NA_real_,
  id_string = NA_character_,
  post_url = NA_character_,
  slug = NA_character_,
  date = NA_character_,
  timestamp = NA_integer_,
  state = NA_character_,
  reblog_key = NA_character_,
  tags = I(list(list())),
  short_url = NA_character_,
  summary = NA_character_,
  should_open_in_legacy = NA,
  recommended_source = NA,
  recommended_color = NA,
  note_count = NA_integer_,
  content = I(list(list())),
  layout = I(list(list())),
  trail = I(list(list())),
  can_like = NA,
  interactability_reblog = NA_character_,
  can_reblog = NA,
  can_send_in_message = NA,
  can_reply = NA,
  display_avatar = NA
)

empty[["image"]] <- tibble::tibble(
  media = I(list(list())),
  colors=NA_character_,
  feedback_token = NA_character_,
  poster = I(list(list())),
  attribution = I(list(list())),
  alt_text = NA_character_,
  caption = NA_character_,
)

empty[["link"]] <- tibble::tibble(
  url = NA_character_,
  title = NA_character_,
  description = NA_character_,
  author = NA_character_,
  site_name = NA_character_,
  display_url = NA_character_,
  poster = I(list(list()))
)

empty[["audio"]] <- tibble::tibble(
  url = NA_character_,
  media = I(list(list())),
  provider = NA_character_,
  title = NA_character_,
  artist = NA_character_,
  album = NA_character_,
  poster = I(list(list())),
  embed_html = NA_character_,
  embed_url = NA_character_,
  metadata = I(list(list())),
  attribution = I(list(list()))
)

empty[["video"]] <- tibble::tibble(
  url = NA_character_,
  media = I(list(list())),
  provider = NA_character_,
  embed_html = NA_character_,
  embed_iframe = I(list(list())),
  embed_url = NA_character_,
  poster = I(list(list())),
  metadata = I(list(list())),
  attribution = I(list(list())),
  can_autoplay_on_cellular = NA
)

usethis::use_data(empty, internal = TRUE, overwrite = TRUE)
