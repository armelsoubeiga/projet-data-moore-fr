library(xml2)
library(rvest)
library(tidyverse)
library(stringi)



# Liste of url
url <- "https://ongafed.wordpress.com/parlez-le-moore/"

content <- read_html(url)
tables <- content %>% html_table(fill = TRUE)
content_table <- tables[[1]]
colnames(content_table) <- content_table[1,]
content <- content_table[-1,]


# Save
data.table::fwrite(content,
                   "C:/Users/aso.RCTS/Downloads/Armel/github/jw-web/ongafed.txt",
                   row.names = FALSE)

