## code to prepare `empty` dataset goes here
empty <- list()

empty[["blog"]] <- tibble::tibble(
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

usethis::use_data(empty, internal = TRUE, overwrite = TRUE)
