

bind_papers <- function(entries_init, scholar_pubs){

  articles <- scholar_pubs %>%
    filter(pubid %in% my_article_ids | cid %in% my_article_ids) %>%
    separate_longer_delim(author, delim = ', ') %>%
    group_split(title) %>%
    map_dfr(
      .f = ~ {
        if(!any(str_detect(.x$author, "Jaeger"))){
          .x$author[nrow(.x)-1] <- "BC Jaeger"
        }
        .x$author <- paste(.x$author, collapse = ', ')
        .x[1, ]
      }
    ) %>%
    mutate(author = str_replace(author, "BC Jaeger", "**BC Jaeger**"),
           author = str_replace(author, "B Jaeger", "**BC Jaeger**"))

  entries_out <- entries_init %>%
    add_row(section = "peer_reviewed_articles",
            title = articles$title,
            loc = glue::glue_data(articles, "{author}<br/>*{journal}*. {number}."),
            institution = as.character(articles$cites),
            start = articles$year,
            end = articles$year)

  entries_out

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

