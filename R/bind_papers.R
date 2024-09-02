

bind_papers <- function(entries_init, scholar_pubs){

  articles <- scholar_pubs %>%
    filter(pubid %in% my_article_ids | cid %in% my_article_ids) %>%
    clean_author_list("BC Jaeger")

  entries_out <- entries_init %>%
    add_row(section = "peer_reviewed_articles",
            title = articles$title,
            loc = glue::glue_data(articles, "{author}<br/>*{journal}*. {number}."),
            institution = as.character(articles$cites),
            start = articles$year,
            end = articles$year)

  entries_out

}


clean_author_list <- function(data, preferred_name){

  name_emboldened <- paste0("**", preferred_name, "**")

  data %>%
    separate_longer_delim(author, delim = ', ') %>%
    group_split(title) %>%
    map_dfr(
      .f = ~ {
        if(!any(str_detect(.x$author, "Jaeger"))){
          .x$author[nrow(.x)-1] <- preferred_name
        }
        .x$author <- paste(.x$author, collapse = ', ')
        .x[1, ]
      }
    ) %>%
    mutate(author = str_replace(author, "BC Jaeger", name_emboldened),
           author = str_replace(author, "B Jaeger", name_emboldened))

}


bind_abstracts <- function(entries_init, scholar_pubs){

  abstracts <- scholar_pubs %>%
    filter(str_detect(tolower(journal), "conference") |
             str_detect(tolower(title), "abstract") |
             str_detect(tolower(number), "supp")) %>%
    clean_author_list("BC Jaeger")

  entries_init %>%
    add_row(section = "abstracts",
            title = abstracts$title,
            loc = glue::glue_data(abstracts, "{author}<br/>*{journal}*. {number}."),
            start = abstracts$year,
            end = abstracts$year)

}

# scholar_data <- get_publications(scholar_id) %>%
#   as_tibble() %>%
#   mutate(is_preprint = str_detect(tolower(journal), "xiv"),
#          is_abstract =
#            str_detect(tolower(journal), "conference") |
#            str_detect(tolower(number), "supp"))
#
# preprints <- scholar_data %>%
#   filter(is_preprint)
#
# abstracts <- scholar_data %>%
#   filter(is_abstract)
#

