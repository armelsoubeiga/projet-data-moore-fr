library(xml2)
library(rvest)
library(tidyverse)
library(stringi)



# Liste of url
url <- "https://www.webonary.org/moore/browse/browse-vernacular-english/?letter=-&key=mos"
list_url <- paste0("https://www.webonary.org/moore/browse/browse-vernacular-english/?letter=",
                   letters,
                   "&key=mos")

list_of_url <- map_dfr(.x = unique(list_url),
                   .f = function(x){
                     tibble(
                       url_pag=try(
                         paste0(
                           "https://www.webonary.org/moore/browse/browse-vernacular-english/",
                             read_html(x) %>%
                             html_nodes("#wrapper .center #wp_page_numbers ul li a")  %>% 
                             html_attr("href") %>% unique()))
                       )})

list_of_url_ <- c(url,list_of_url$url_pag)


content_collect <- map_dfr(.x = list_of_url_[1:10], #[1:10]
                           .f = function(x){
                             tibble(
                               mos = tryCatch(
                                   read_html(x) %>%
                                   html_nodes(".senses .examplescontent .example") %>%
                                   html_text() %>% str_trim() %>% str_subset("\\*", negate = TRUE),
                              error = function(e){NA},
                              warning=  function(e){NA}),
                               
                              fr = tryCatch(
                                  read_html(x) %>%
                                  html_nodes(".senses .examplescontent .translationcontents .translation")%>%
                                  html_text() %>% str_remove('Frn')%>% str_subset("\\*", negate = TRUE) %>% str_split_fixed('Eng',2) %>% .[,1],
                              error = function(e){NA},
                              warning=  function(e){NA}),
                              
                              en = tryCatch(
                                read_html(x) %>%
                                  html_nodes(".senses .examplescontent .translationcontents .translation")%>%
                                  html_text() %>% str_remove('Frn')%>% str_subset("\\*", negate = TRUE) %>% str_split_fixed('Eng',2) %>% .[,2],
                                error = function(e){NA},
                                warning=  function(e){NA})
                              )})


# Save
data.table::fwrite(content_collect,
                   "C:/Users/aso.RCTS/Downloads/Armel/github/jw-web/webonary.txt",
                   row.names = FALSE)

