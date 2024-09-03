

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

bind_rpacks <- function(entries_init, scholar_pubs){

  rpack_ids <- c('_xSYboBqXhAC' = 'aorsf',
                 'u-x6o8ySG0sC' = 'r2glmm')

  rpack_repos <- c('bcjaeger/r2glmm' = 'r2glmm',
                   'bcjaeger/table.glue' = 'table.glue',
                   "jhs-hwg/cardioStatsUSA" = 'cardioStatsUSA',
                   "bcjaeger/PooledCohort" = 'PooledCohort',
                   'ropensci/aorsf' = 'aorsf')

  rpack_dl_recent <- cran_downloads(packages = rpack_repos,
                                    from = today() - 30,
                                    to = today()) %>%
    filter(count > 0) %>%
    group_by(package) %>%
    summarize(downloads_last_30 = sum(count)) %>%
    rename(title = package)

  rpack_dl_total <- cran_downloads(packages = rpack_repos,
                                   from = "2016-09-15", # r2glmm released
                                   to = today()) %>%
    filter(count > 0) %>%
    group_by(package) %>%
    summarize(downloads_total = sum(count),
              year = year(min(date))) %>%
    rename(title = package)

  rpack_stars <- names(rpack_repos) %>%
    set_names(rpack_repos) %>%
    map_int(
      .f = ~ gh::gh(
        endpoint = paste0("GET /repos/", .x, "/stargazers"),
        .accept = "application/vnd.github.v3.star+json",
        .limit = Inf
      ) %>%
        length()
    ) %>%
    enframe(name = 'title',
            value = 'stars_github')

  rpacks <- filter(scholar_pubs, pubid %in% names(rpack_ids)) %>%
    transmute(
      description_1 = glue("Accompanying paper: {title}. *{journal}*. {number}"),
      title = recode(pubid, !!!rpack_ids)
    ) %>%
    right_join(rpack_stars, by = 'title') %>%
    left_join(rpack_dl_recent, by = 'title') %>%
    left_join(rpack_dl_total, by = 'title') %>%
    mutate(
      description_2 = table_glue("Downloaded {downloads_last_30} times in \\
                                  last 30 days, downloaded {downloads_total} \\
                                  times in total"),
      description_3 = table_glue("GitHub stars: {stars_github}")
    )

  entries_init %>%
    add_row(section = "r_packages",
            title = rpacks$title,
            loc = rpacks$loc,
            start = rpacks$year,
            end = year(today()),
            description_1 = rpacks$description_1,
            description_2 = rpacks$description_2)

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

