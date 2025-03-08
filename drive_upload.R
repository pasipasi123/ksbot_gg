library(googledrive)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

drive_put("ptalot.sqlite", "parkkitalodata/")

