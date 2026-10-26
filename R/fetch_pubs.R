# Pull recent publications from open bibliographic APIs -----------------------
#
# Google Scholar (via the scholar package) remains the source for the
# historical record and for citation counts, but new Scholar items only reach
# the CV once their pubid is added to `my_article_ids`. To keep the CV current
# without hand-curating ids, recent work is also pulled from:
#
#   - OpenAlex   (works linked to my ORCID; includes conference abstracts)
#   - Europe PMC (PubMed-indexed works matching my author name)
#
# Results are classified (article / abstract / preprint / exclude),
# de-duplicated by DOI and by fuzzy title, and then merged with Scholar in
# bind_papers() / bind_abstracts(). Anything that slips through incorrectly can
# be dropped via data/pub_exclusions.csv.

`%||%` <- function(x, y) if (is.null(x)) y else x

chr_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
}

empty_pubs <- function() {
  tibble::tibble(
    source = character(), doi = character(), title = character(),
    date = as.Date(character()), year = integer(), type = character(),
    source_type = character(), journal = character(), volume = character(),
    issue = character(), pages = character(), authors = list(),
    cites = integer(), flags = character()
  )
}

# OpenAlex --------------------------------------------------------------------

fetch_openalex_works <- function(orcid, from_date, mailto = NULL) {

  req <- httr2::request("https://api.openalex.org/works") |>
    httr2::req_url_query(
      filter = glue::glue("author.orcid:{orcid},from_publication_date:{from_date}"),
      `per-page` = 200,
      mailto = mailto
    ) |>
    httr2::req_user_agent("bcjaeger/cv (https://github.com/bcjaeger/cv)") |>
    httr2::req_retry(max_tries = 3)

  results <- list()
  cursor <- "*"

  out <- tryCatch({
    while (!is.null(cursor)) {
      body <- req |>
        httr2::req_url_query(cursor = cursor) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE)
      if (length(body$results) == 0) break
      results <- c(results, body$results)
      cursor <- body$meta$next_cursor
    }
    purrr::map_dfr(results, parse_openalex_work)
  }, error = function(e) {
    warning("OpenAlex fetch failed: ", conditionMessage(e), call. = FALSE)
    empty_pubs()
  })

  if (nrow(out) == 0) empty_pubs() else out
}

parse_openalex_work <- function(w) {

  loc <- w$primary_location %||% list()
  src <- loc$source %||% list()
  bib <- w$biblio %||% list()

  first <- chr_or_na(bib$first_page)
  last  <- chr_or_na(bib$last_page)
  pages <- dplyr::case_when(
    is.na(first) ~ NA_character_,
    is.na(last) | last == first ~ first,
    TRUE ~ paste0(first, "-", last)
  )

  authors <- purrr::map_chr(
    w$authorships %||% list(),
    ~ chr_or_na(.x$author$display_name %||% .x$raw_author_name)
  )

  flags <- c(
    if (isTRUE(w$is_paratext)) "paratext",
    if (isTRUE(w$is_retracted)) "retracted"
  )

  tibble::tibble(
    source = "openalex",
    doi = tolower(sub("^https?://(dx\\.)?doi\\.org/", "", chr_or_na(w$doi))),
    title = chr_or_na(w$title %||% w$display_name),
    date = as.Date(chr_or_na(w$publication_date)),
    year = as.integer(chr_or_na(w$publication_year)),
    type = chr_or_na(w$type),
    source_type = chr_or_na(src$type),
    journal = chr_or_na(src$display_name),
    volume = chr_or_na(bib$volume),
    issue = chr_or_na(bib$issue),
    pages = pages,
    authors = list(format_author_names(authors, style = "given_family")),
    cites = as.integer(w$cited_by_count %||% NA_integer_),
    flags = paste(flags, collapse = ";")
  )
}

# Europe PMC -------------------------------------------------------------------

fetch_europepmc_works <- function(author, from_date) {

  query <- glue::glue('AUTH:"{author}" AND FIRST_PDATE:[{from_date} TO 3000-12-31]')

  req <- httr2::request(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
  ) |>
    httr2::req_url_query(query = query, format = "json",
                         pageSize = 1000, resultType = "lite") |>
    httr2::req_user_agent("bcjaeger/cv (https://github.com/bcjaeger/cv)") |>
    httr2::req_retry(max_tries = 3)

  results <- list()
  cursor <- "*"

  out <- tryCatch({
    repeat {
      body <- req |>
        httr2::req_url_query(cursorMark = cursor) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE)
      hits <- body$resultList$result %||% list()
      if (length(hits) == 0) break
      results <- c(results, hits)
      next_cursor <- body$nextCursorMark %||% cursor
      if (identical(next_cursor, cursor)) break
      cursor <- next_cursor
    }
    purrr::map_dfr(results, parse_europepmc_work)
  }, error = function(e) {
    warning("Europe PMC fetch failed: ", conditionMessage(e), call. = FALSE)
    empty_pubs()
  })

  if (nrow(out) == 0) empty_pubs() else out
}

parse_europepmc_work <- function(r) {

  pub_type <- chr_or_na(r$pubType)
  src <- chr_or_na(r$source)

  flags <- c(
    if (!is.na(pub_type) && grepl("erratum|correction|retraction", pub_type,
                                  ignore.case = TRUE)) "erratum",
    if (identical(src, "PPR")) "preprint"
  )

  authors <- strsplit(chr_or_na(r$authorString) %|NA|% "", ",\\s*")[[1]]
  authors <- sub("\\.$", "", authors)

  tibble::tibble(
    source = "europepmc",
    doi = tolower(chr_or_na(r$doi)),
    title = sub("\\.$", "", chr_or_na(r$title)),
    date = as.Date(chr_or_na(r$firstPublicationDate)),
    year = as.integer(chr_or_na(r$pubYear)),
    type = if (identical(src, "PPR")) "preprint" else "article",
    source_type = if (identical(src, "PPR")) "repository" else "journal",
    journal = chr_or_na(r$journalTitle),
    volume = chr_or_na(r$journalVolume),
    issue = chr_or_na(r$issue),
    pages = chr_or_na(r$pageInfo),
    authors = list(format_author_names(authors, style = "family_initials")),
    cites = as.integer(r$citedByCount %||% NA_integer_),
    flags = paste(flags, collapse = ";")
  )
}

`%|NA|%` <- function(x, y) ifelse(is.na(x), y, x)

# Author formatting -----------------------------------------------------------

#' Convert author names to the "BC Jaeger" style used by Google Scholar.
#' @param style "given_family" for "Byron C. Jaeger", or "family_initials"
#'   for "Jaeger BC" (Europe PMC / PubMed).
format_author_names <- function(x, style = c("given_family", "family_initials")) {

  style <- match.arg(style)
  x <- trimws(x[!is.na(x) & nzchar(trimws(x))])
  if (length(x) == 0) return(character())

  suffix <- "^(jr|sr|ii|iii|iv)\\.?$"

  purrr::map_chr(x, function(name) {

    tokens <- strsplit(name, "\\s+")[[1]]
    tokens <- tokens[!grepl(suffix, tokens, ignore.case = TRUE)]
    if (length(tokens) < 2) return(name)

    if (style == "family_initials") {
      initials <- tokens[length(tokens)]
      # collective names ("CRIC Study Investigators") have no initials
      if (!grepl("^[A-Z]{1,4}$", initials)) return(name)
      return(paste(initials, paste(tokens[-length(tokens)], collapse = " ")))
    }

    family <- tokens[length(tokens)]
    given <- tokens[-length(tokens)]
    initials <- paste0(toupper(substr(gsub("[^[:alpha:]]", "", given), 1, 1)),
                       collapse = "")
    paste(initials, family)
  })
}

#' Collapse a vector of formatted names into a CV author string, truncating
#' long lists the way Google Scholar does while always showing my name.
collapse_authors <- function(authors,
                             preferred_name = "BC Jaeger",
                             max_authors = 6) {

  authors <- unlist(authors)
  if (length(authors) == 0) return(paste0("**", preferred_name, "**"))

  is_me <- grepl("^B[A-Z]* Jaeger$", authors, ignore.case = TRUE)
  authors[is_me] <- preferred_name

  if (length(authors) > max_authors) {
    shown <- authors[seq_len(max_authors)]
    if (any(is_me) && !any(is_me[seq_len(max_authors)])) {
      shown <- c(shown, preferred_name)
    }
    shown <- c(shown, "…")
  } else {
    shown <- authors
  }

  shown <- paste(shown, collapse = ", ")
  sub(preferred_name, paste0("**", preferred_name, "**"), shown, fixed = TRUE)
}

# Classification + de-duplication ----------------------------------------------

classify_pubs <- function(pubs) {

  supp_rx <- "(?i)supp|^S\\d+$|^\\d+S$"
  abstract_title_rx <- "(?i)^(abstract\\s+[A-Z0-9-]+\\s*:|\\d+-[A-Z]+\\s*:)"

  pubs |>
    dplyr::mutate(
      kind = dplyr::case_when(
        grepl("erratum|paratext|retracted", flags) ~ "exclude",
        type %in% c("erratum", "retraction", "paratext", "dataset",
                    "software", "peer-review", "other") ~ "exclude",
        type == "preprint" | grepl("preprint", flags) |
          grepl("rxiv", journal, ignore.case = TRUE) ~ "preprint",
        type == "conference-abstract" ~ "abstract",
        grepl(supp_rx, issue, perl = TRUE) |
          grepl(supp_rx, volume, perl = TRUE) ~ "abstract",
        grepl(abstract_title_rx, title, perl = TRUE) ~ "abstract",
        source_type == "repository" ~ "exclude",
        is.na(journal) ~ "exclude",
        TRUE ~ "article"
      )
    )
}

title_tokens <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = " ")
  x <- tolower(gsub("<[^>]+>", " ", x))
  x <- gsub("[^a-z0-9]+", " ", x)
  lapply(strsplit(trimws(x), "\\s+"), unique)
}

title_similarity <- function(a, b) {
  # Jaccard similarity between every title in `a` and every title in `b`
  ta <- title_tokens(a)
  tb <- title_tokens(b)
  outer(seq_along(ta), seq_along(tb), Vectorize(function(i, j) {
    u <- length(union(ta[[i]], tb[[j]]))
    if (u == 0) 0 else length(intersect(ta[[i]], tb[[j]])) / u
  }))
}

#' Keep the first occurrence of each near-duplicate title (rows are assumed to
#' already be in priority order).
dedupe_titles <- function(pubs, threshold = 0.75) {
  if (nrow(pubs) <= 1) return(pubs)
  sim <- title_similarity(pubs$title, pubs$title)
  keep <- rep(TRUE, nrow(pubs))
  for (i in seq_len(nrow(pubs))[-1]) {
    earlier <- which(keep[seq_len(i - 1)])
    if (length(earlier) && any(sim[i, earlier] >= threshold)) keep[i] <- FALSE
  }
  pubs[keep, ]
}

#' Drop rows of `pubs` whose title matches any title in `existing_titles`.
drop_existing_titles <- function(pubs, existing_titles, threshold = 0.75) {
  existing_titles <- existing_titles[!is.na(existing_titles)]
  if (nrow(pubs) == 0 || length(existing_titles) == 0) return(pubs)
  sim <- title_similarity(pubs$title, existing_titles)
  pubs[apply(sim, 1, max) < threshold, ]
}

#' Combine OpenAlex + Europe PMC records into one classified, de-duplicated
#' table of recent works.
combine_recent_pubs <- function(openalex, epmc, exclusions = NULL) {

  pubs <- dplyr::bind_rows(openalex, epmc) |>
    dplyr::filter(!is.na(title)) |>
    classify_pubs()

  if (nrow(pubs) == 0) return(dplyr::mutate(pubs, kind = character()))

  # if any source flags a DOI as excludable (e.g., an erratum), drop it everywhere
  bad_dois <- pubs$doi[pubs$kind == "exclude" & !is.na(pubs$doi)]

  if (!is.null(exclusions) && nrow(exclusions) > 0) {
    bad_dois <- c(bad_dois, tolower(stats::na.omit(exclusions$doi)))
    excl_titles <- stats::na.omit(exclusions$title)
    if (length(excl_titles)) {
      pubs <- drop_existing_titles(pubs, excl_titles, threshold = 0.9)
    }
  }

  pubs |>
    dplyr::filter(kind != "exclude", is.na(doi) | !doi %in% bad_dois) |>
    # citation counts: take the best available across sources
    dplyr::group_by(doi_key = dplyr::coalesce(doi, paste0("no-doi-", dplyr::row_number()))) |>
    dplyr::mutate(cites = suppressWarnings(max(cites, na.rm = TRUE)),
                  cites = ifelse(is.finite(cites), cites, NA_integer_)) |>
    # prefer OpenAlex (full author lists, abstracts) over Europe PMC
    dplyr::arrange(factor(source, levels = c("openalex", "europepmc")),
                   .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-doi_key) |>
    dplyr::arrange(kind, factor(source, levels = c("openalex", "europepmc")), date) |>
    dplyr::group_split(kind) |>
    purrr::map_dfr(dedupe_titles) |>
    dplyr::arrange(dplyr::desc(date))
}

#' Format recent works into the columns used by the entries table
format_recent_pubs <- function(pubs, preferred_name = "BC Jaeger") {

  if (nrow(pubs) == 0) {
    return(tibble::tibble(title = character(), loc = character(),
                          cites = integer(), year = integer()))
  }

  pubs |>
    dplyr::mutate(
      author = purrr::map_chr(authors, collapse_authors,
                              preferred_name = preferred_name),
      number = purrr::pmap_chr(
        list(volume, issue, pages),
        function(v, i, p) {
          vi <- paste0(if (!is.na(v)) v else "",
                       if (!is.na(i)) paste0(" (", i, ")") else "")
          paste(c(if (nzchar(trimws(vi))) trimws(vi), if (!is.na(p)) p),
                collapse = ", ")
        }
      ),
      title = sub("\\.$", "", title),
      loc = glue::glue("{author}<br/>*{journal}*. {number}.")
    ) |>
    dplyr::select(title, loc, cites, year)
}
