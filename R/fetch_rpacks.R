# Pull metadata for my R packages ---------------------------------------------
#
# Packages are listed in data/r_packages.csv (one row per package). Everything
# else (title, version, GitHub stars, first release year, CRAN downloads) is
# pulled from GitHub and the RStudio CRAN mirror logs, so adding a package to
# the CV only requires adding a row to that file.

fetch_github_package_meta <- function(repo) {

  empty <- tibble::tibble(
    repo = repo, pkg_title = NA_character_, pkg_version = NA_character_,
    stars_github = NA_integer_, repo_created = as.Date(NA),
    repo_url = paste0("https://github.com/", repo)
  )

  tryCatch({

    meta <- gh::gh("GET /repos/{repo}", repo = repo)

    desc_raw <- gh::gh("GET /repos/{repo}/contents/DESCRIPTION", repo = repo)
    desc_txt <- rawToChar(jsonlite::base64_dec(gsub("\n", "", desc_raw$content)))
    desc <- read.dcf(textConnection(desc_txt), fields = c("Title", "Version"))

    tibble::tibble(
      repo = repo,
      pkg_title = gsub("\\s+", " ", desc[1, "Title"]),
      pkg_version = desc[1, "Version"],
      stars_github = as.integer(meta$stargazers_count),
      repo_created = as.Date(substr(meta$created_at, 1, 10)),
      repo_url = meta$html_url
    )

  }, error = function(e) {
    warning("GitHub metadata failed for ", repo, ": ", conditionMessage(e),
            call. = FALSE)
    empty
  })
}

fetch_cran_downloads <- function(packages, from = "2016-09-15") {

  empty <- tibble::tibble(package = packages,
                          downloads_total = 0L,
                          downloads_last_30 = 0L,
                          first_download = as.Date(NA))

  tryCatch({

    dl <- cranlogs::cran_downloads(packages = packages,
                                   from = from,
                                   to = lubridate::today()) |>
      dplyr::filter(count > 0)

    dl |>
      dplyr::group_by(package) |>
      dplyr::summarize(
        downloads_total = sum(count),
        downloads_last_30 = sum(count[date > lubridate::today() - 30]),
        first_download = min(date),
        .groups = "drop"
      ) |>
      dplyr::right_join(tibble::tibble(package = packages), by = "package") |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("downloads"),
                                  ~ dplyr::coalesce(as.integer(.x), 0L)))

  }, error = function(e) {
    warning("CRAN download logs failed: ", conditionMessage(e), call. = FALSE)
    empty
  })
}

#' Assemble everything the CV needs about each package.
#' @param r_packages data from data/r_packages.csv
#' @param scholar_pubs Google Scholar publications (for accompanying papers)
fetch_rpack_info <- function(r_packages, scholar_pubs) {

  gh_meta <- purrr::map_dfr(r_packages$repo, fetch_github_package_meta)

  cran <- fetch_cran_downloads(r_packages$package)

  papers <- scholar_pubs |>
    dplyr::filter(pubid %in% stats::na.omit(r_packages$scholar_pubid)) |>
    dplyr::transmute(
      scholar_pubid = pubid,
      paper = glue::glue("Accompanying paper: {title}. *{journal}*. {number}")
    )

  r_packages |>
    dplyr::left_join(gh_meta, by = "repo") |>
    dplyr::left_join(cran, by = "package") |>
    dplyr::left_join(papers, by = "scholar_pubid") |>
    dplyr::mutate(
      on_cran = downloads_total > 0,
      start_year = dplyr::coalesce(lubridate::year(first_download),
                                   lubridate::year(repo_created))
    )
}
