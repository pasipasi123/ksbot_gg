library(googledrive)

drive_auth_configure(path = "client.json")

drive_put("ptalot.sqlite", "parkkitalodata/")

