
source("packages.R")
source("conflicts.R")

## Load your R files
tar_source()

scholar_id <- "4IKD_roAAAAJ&hl"

scholar_stats_tar <- tar_target(scholar_stats, get_profile(scholar_id))

scholar_pubs_tar <- tar_target(scholar_pubs, get_publications(scholar_id))

entries_file_tar <- tar_target(entries_file,
                               command = "data/entries_init.csv",
                               format = 'file')

entries_init_tar <- tar_target(entries_init, command = read_csv(entries_file))

entries_tar <- tar_target(entries, command = {
  entries_init %>%
    bind_papers(scholar_pubs)
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



