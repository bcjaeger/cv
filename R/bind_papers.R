
#' Add peer reviewed articles to the entries table.
#'
#' Articles come from two places:
#'   1. Google Scholar items whose pubid is listed in `my_article_ids`
#'   2. Recent works from OpenAlex / Europe PMC (see R/fetch_pubs.R) that are
#'      not already covered by (1).
bind_papers <- function(entries_init, scholar_pubs, recent_pubs = NULL){

  articles <- scholar_pubs %>%
    filter(pubid %in% my_article_ids | cid %in% my_article_ids) %>%
    clean_author_list("BC Jaeger")

  scholar_rows <- tibble(
    title = articles$title,
    loc = as.character(glue::glue_data(articles, "{author}<br/>*{journal}*. {number}.")),
    cites = as.integer(articles$cites),
    year = as.integer(articles$year)
  )

  new_rows <- recent_rows(recent_pubs, "article", existing = scholar_rows$title)

  rows <- bind_rows(scholar_rows, new_rows)

  entries_init %>%
    add_row(section = "peer_reviewed_articles",
            title = rows$title,
            loc = rows$loc,
            institution = as.character(rows$cites),
            start = rows$year,
            end = rows$year)

}

#' Recent works of a given kind that are not already on Google Scholar
recent_rows <- function(recent_pubs, kind_wanted, existing) {

  if (is.null(recent_pubs) || nrow(recent_pubs) == 0) {
    return(tibble(title = character(), loc = character(),
                  cites = integer(), year = integer()))
  }

  recent_pubs %>%
    filter(kind == kind_wanted) %>%
    drop_existing_titles(existing) %>%
    format_recent_pubs(preferred_name = "BC Jaeger") %>%
    mutate(loc = as.character(loc))

}

clean_author_list <- function(data, preferred_name){

  name_emboldened <- paste0("**", preferred_name, "**")

  data %>%
    separate_longer_delim(author, delim = ', ') %>%
    group_split(title) %>%
    map_dfr(
      .f = ~ {
        if(!any(str_detect(.x$author, regex("Jaeger", ignore_case = TRUE)))){
          .x$author[nrow(.x)-1] <- preferred_name
        }
        .x$author <- paste(.x$author, collapse = ', ')
        .x[1, ]
      }
    ) %>%
    mutate(author = str_replace(author,
                                regex("\\bB(C)? Jaeger\\b", ignore_case = TRUE),
                                name_emboldened))

}

bind_abstracts <- function(entries_init, scholar_pubs, recent_pubs = NULL){

  abstracts <- scholar_pubs %>%
    filter(str_detect(tolower(journal), "conference") |
             str_detect(tolower(title), "abstract") |
             str_detect(tolower(number), "supp")) %>%
    clean_author_list("BC Jaeger")

  scholar_rows <- tibble(
    title = abstracts$title,
    loc = as.character(glue::glue_data(abstracts, "{author}<br/>*{journal}*. {number}.")),
    year = as.integer(abstracts$year)
  )

  new_rows <- recent_rows(recent_pubs, "abstract", existing = scholar_rows$title)

  rows <- bind_rows(scholar_rows, select(new_rows, -cites))

  entries_init %>%
    add_row(section = "abstracts",
            title = rows$title,
            loc = rows$loc,
            start = rows$year,
            end = rows$year)

}

#' Add R packages to the entries table using metadata from fetch_rpack_info()
bind_rpacks <- function(entries_init, rpack_info){

  rpacks <- rpack_info %>%
    arrange(desc(start_year), package) %>%
    mutate(
      suite = basename(suite_repo),
      suite_note = if_else(
        is.na(suite_repo), NA_character_,
        as.character(glue("Part of the [`{suite}`](https://github.com/{suite_repo}) family of packages"))
      ),
      usage_note = if_else(
        on_cran,
        as.character(table_glue("On CRAN: downloaded {downloads_last_30} times in the last \\
                                 30 days and {downloads_total} times in total")),
        as.character(glue("Available on [GitHub]({repo_url})"))
      ),
      version_note = case_when(
        !is.na(pkg_version) & !is.na(stars_github) ~
          as.character(glue("Version {pkg_version}; GitHub stars: {stars_github}")),
        !is.na(stars_github) ~ as.character(glue("GitHub stars: {stars_github}")),
        !is.na(pkg_version) ~ as.character(glue("Version {pkg_version}")),
        TRUE ~ NA_character_
      )
    )

  entries_init %>%
    add_row(section = "r_packages",
            title = rpacks$package,
            loc = rpacks$pkg_title,
            start = rpacks$start_year,
            end = year(today()),
            description_1 = coalesce(as.character(rpacks$paper),
                                     as.character(rpacks$suite_note)),
            description_2 = as.character(rpacks$usage_note),
            description_3 = as.character(rpacks$version_note))

}
