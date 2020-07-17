#' Build CV
#'
#' @param full_name My name
#' @param data_location character, link to google sheets
#' @param peer_review Object of class `BibEntry`
#' @param tech_report Object of class `BibEntry`
#' @param conference Object of class `BibEntry`
#' @param software Object of class `BibEntry`
#' @param datasets Object of class `BibEntry`
#' @param output_file_name what should the output doc name be
#' @param output_dir output directory, defaults to current working directory
#'
#' @return files written to the current directory
#' @export
#' @importFrom fs path
#' @importFrom RefManageR ReadZotero WriteBib
#' @importFrom rmarkdown render
#' @importFrom bookdown pdf_document2
build_cv <- function(full_name = "Michael Schramm",
                     data_location = NULL,
                     peer_review = NULL,
                     tech_report = NULL,
                     conference = NULL,
                     software = NULL,
                     datasets = NULL,
                     output_file_name = "cv.pdf",
                     output_dir = getwd()) {

  ## get the rmd file path and other file paths
  template_loc <- fs::path(system.file("templates/", package = "CVR"), "cv.Rmd")
  tex_loc <- fs::path(system.file("templates/", package = "CVR"), "svm-latex-cv.tex")
  lua_mb_loc <- fs::path(system.file("templates/", package = "CVR"), "multiple-bibliographies.lua")
  csl_loc <- fs::path(system.file("templates/", package = "CVR"), "modapa.csl")


  cv_data <- create_CV_object(data_location = data_location)

  ref_data <- RefManageR::ReadZotero(group = "2533336",
                        .params=list(key='jaGy6K1gbCoBGcq2BWtHsvop'))

  peer_review_path <- paste0(output_dir, "/peer_review.bib")
  RefManageR::WriteBib(peer_review, file = peer_review_path)

  tech_report_path <- paste0(output_dir, "/tech_report.bib")
  RefManageR::WriteBib(tech_report, file = tech_report_path)

  conf_path <- paste0(output_dir, "/conference.bib")
  RefManageR::WriteBib(conference, file = conf_path)

  soft_path <- paste0(output_dir, "/software.bib")
  RefManageR::WriteBib(software, file = soft_path)

  data_path <- paste0(output_dir, "/datasets.bib")
  RefManageR::WriteBib(datasets, file = data_path)

  pandoc_args = paste0("--lua-filter=", lua_mb_loc)
  rmarkdown::render(input = template_loc,
                    bookdown::pdf_document2(latex_engine = "pdflatex",
                                            template = tex_loc,
                                            number_sections = FALSE,
                                            pandoc_args = pandoc_args),
                    output_file = output_file_name,
                    output_dir = output_dir,
                    intermediates_dir = output_dir)

  }


