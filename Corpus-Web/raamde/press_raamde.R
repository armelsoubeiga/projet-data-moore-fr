library(xml2)
library(rvest)
library(tidyverse)
library(stringi)



# Liste of url
url <- "http://raamde-bf.com/"
list_url <- read_html(url) %>%
            html_nodes("#page #content #secondary #archives-2 ul li a")  %>% 
            html_attr("href") %>% unique()
list_url <- list_url[-length(list_url)]


#Pagination collect
number_pag <- map_dfr(.x = list_url,
                      .f = function(x){
                        tibble(url = x,
                               nbr_pg=read_html(x) %>%
                                 html_nodes("#page #content .nav-links a")  %>% 
                                 html_text()  %>% str_subset("Suivant", negate = TRUE)  %>% 
                                 as.integer() %>% tail(1)
                        )})

list_url_ <- merge(as.data.frame(list_url),number_pag,by.x="list_url",by.y ="url",all = TRUE)
list_url_$nbr_pg[is.na(list_url_$nbr_pg)] <- 1


# Build all pagination url
number_pag <- na.omit(list_url_)
number_pag_order <- number_pag %>%
                    split(1:nrow(.)) %>%
                    map_dfr(.f = function(x){
                      tibble(url = x$list_url,
                             nbr_pg=1:x$nbr_pg)})

number_pag_order$url_1 <- str_split_fixed(number_pag_order$url,"\\?",2)[,1]
number_pag_order$url_2 <- str_split_fixed(number_pag_order$url,"\\?",2)[,2]

list_of_url <- paste0(number_pag_order$url_1,
                      '?paged=', 
                      number_pag_order$nbr_pg,
                      "&",
                      number_pag_order$url_2)




# article url collection
list_of_aricle <- map_dfr(.x = list_of_url[5], #list_of_url
                       .f = function(x){
                         tibble(
                           url_art=try(
                             read_html(x) %>%
                               html_nodes("#page #content #main article header h2 a")  %>% 
                               html_attr("href")))})



##
##
#Contente
content <- map_dfr(.x = list_of_aricle$url_art , #[1:10]
                   .f = function(x){
                    tibble(
                      txt_split = tryCatch(read_html(x) %>%
                                  html_nodes("#page #content #main article p strong") %>%
                                  html_text() %>% .[2]),
                      
                      txt = paste0(tryCatch(
                            read_html(x) %>%
                                  html_nodes("#page #content #main article p") %>% 
                                  html_text() %>% str_trim(),
                             error = function(e){NA},
                             warning=  function(e){NA}),
                            collapse = " "),
                      
                      mos = str_split_fixed(txt,txt_split,n=2)[,1], 
                      fr = str_split_fixed(txt,txt_split,n=2)[,2]
                    )})






# parial save
data.table::fwrite(as.data.frame(list_of_url),
                   "C:/Users/aso.RCTS/Downloads/Armel/github/jw-web/list_of_url.txt",
                   row.names = FALSE)

