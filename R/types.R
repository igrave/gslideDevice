# url <- "https://slides.googleapis.com/$discovery/rest?version=v1"
# discovery <- jsonlite::fromJSON("https://slides.googleapis.com/$discovery/rest?version=v1")
#
# discovery$resources
#
# library(purrr)
# has_element(discovery, "enum")
# ul <- unlist(discovery)
#
# ul_enum <- ul[which(grepl("enum", names(ul)))]
# enum_desc_index <- which(grepl("enumDescription", names(ul_enum)))
#
# names(ul_enum[-enum_desc_index])
#
#
# enum_dfs <-
# data.frame(
#   TYPE = gsub(".enum[[:digit:]]*", "", names(ul_enum[-enum_desc_index])),
#   ENUM = ul_enum[-enum_desc_index],
#   DESCRIPTION = ul_enum[enum_desc_index],
#   row.names = NULL
# )
#
# View(enum_dfs)
# split(enum_dfs, enum_dfs$TYPE)
