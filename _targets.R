
source("packages.R")
source("conflicts.R")

## Load your R files
tar_source()

scholar_id <- "4IKD_roAAAAJ&hl"

scholar_stats_tar <- tar_target(scholar_stats, get_profile(scholar_id))

scholar_pubs_tar <- tar_target(scholar_pubs, get_publications(scholar_id))

# recent works from open APIs; always re-run so new papers appear
openalex_pubs_tar <- tar_target(
  openalex_pubs,
  fetch_openalex_works(my_orcid, pub_search_start, mailto = my_email),
  cue = tar_cue(mode = "always")
)

epmc_pubs_tar <- tar_target(
  epmc_pubs,
  fetch_europepmc_works(my_epmc_author, pub_search_start),
  cue = tar_cue(mode = "always")
)

pub_exclusions_file_tar <- tar_target(pub_exclusions_file,
                                      command = "data/pub_exclusions.csv",
                                      format = 'file')

pub_exclusions_tar <- tar_target(
  pub_exclusions,
  read_csv(pub_exclusions_file, col_types = "ccc")
)

recent_pubs_tar <- tar_target(
  recent_pubs,
  combine_recent_pubs(openalex_pubs, epmc_pubs, exclusions = pub_exclusions)
)

r_packages_file_tar <- tar_target(r_packages_file,
                                  command = "data/r_packages.csv",
                                  format = 'file')

r_packages_tar <- tar_target(
  r_packages,
  read_csv(r_packages_file, col_types = "cccc")
)

rpack_info_tar <- tar_target(
  rpack_info,
  fetch_rpack_info(r_packages, scholar_pubs),
  cue = tar_cue(mode = "always")
)

entries_file_tar <- tar_target(entries_file,
                               command = "data/entries_init.csv",
                               format = 'file')

entries_init_tar <- tar_target(entries_init, command = read_csv(entries_file))

entries_tar <- tar_target(entries, command = {
  entries_init %>%
    bind_papers(scholar_pubs, recent_pubs) %>%
    bind_abstracts(scholar_pubs, recent_pubs) %>%
    bind_rpacks(rpack_info)
})

skills_file_tar <- tar_target(skills_file,
                              command = "data/skills.csv",
                              format = 'file')

skills_tar <- tar_target(skills, read_csv(skills_file))

text_blocks_file_tar <- tar_target(text_blocks_file,
                                   command = "data/text_blocks.csv",
                                   format = 'file')

text_blocks_tar <- tar_target(text_blocks, read_csv(text_blocks_file))

contact_info_file_tar <- tar_target(contact_info_file,
                                    command = "data/contact_info.csv",
                                    format = 'file')

contact_info_tar <- tar_target(contact_info, read_csv(contact_info_file))

cv_data_tar <- tar_map(
  values = tibble(pdf_mode = c(TRUE, FALSE),
                  name = c("pdf", "html")),
  names = name,
  tar_target(
    cv_data,
    create_CV_object(skills = skills,
                     text_blocks = text_blocks,
                     entries = entries,
                     contact_info = contact_info,
                     pdf_mode = pdf_mode))
  )


cv_html_tar <- tar_render(cv_html,
                          path = "cv.rmd",
                          params = data.frame(pdf_mode = FALSE),
                          output_file = "cv.html")

cv_pdf_tar <- tar_render(cv_pdf,
                         path = "cv.rmd",
                         params = data.frame(pdf_mode = TRUE),
                         output_file = "cv_for_pdf.html")

cv_pdf_file_tar <- tar_target(cv_pdf_file,
                              command = "cv_for_pdf.html",
                              format = 'file')

# Convert to PDF using Pagedown
cv_pdf_print_tar <- tar_target(cv_pdf_print,
                               command = chrome_print(input = cv_pdf_file,
                                                      output = "cv.pdf"))


targets <- list(
  scholar_stats_tar,
  scholar_pubs_tar,
  openalex_pubs_tar,
  epmc_pubs_tar,
  pub_exclusions_file_tar,
  pub_exclusions_tar,
  recent_pubs_tar,
  r_packages_file_tar,
  r_packages_tar,
  rpack_info_tar,
  entries_file_tar,
  entries_init_tar,
  entries_tar,
  skills_file_tar,
  skills_tar,
  text_blocks_file_tar,
  text_blocks_tar,
  contact_info_file_tar,
  contact_info_tar,
  cv_data_tar,
  cv_html_tar,
  cv_pdf_tar,
  cv_pdf_file_tar,
  cv_pdf_print_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)



