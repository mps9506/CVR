#' Build CV
#'
#' @param full_name character string, my name.
#' @param data_location character string, link to google sheets document.
#' @param peer_review Object of class `BibEntry`.
#' @param tech_report Object of class `BibEntry`.
#' @param conference Object of class `BibEntry`.
#' @param software Object of class `BibEntry`.
#' @param datasets Object of class `BibEntry`.
#' @param rmd_template character string indicating path to rmarkdown template or one of `c(svm_cv)`.
#' @param output_file_name character string for the name of the output document.
#' @param output_dir character string for the name of the output directory, defaults to current working directory.
#' @param spell_check logical use the spell_check package to spell check output? defaults to FALSE
#'
#' @return files written to the current directory
#' @export
#' @importFrom fs path
#' @importFrom RefManageR ReadZotero WriteBib
#' @importFrom rmarkdown render
#' @importFrom bookdown pdf_document2
#' @importFrom spelling spell_check_files
build_cv <- function(full_name = "Michael Schramm",
                     data_location = NULL,
                     peer_review = NULL,
                     tech_report = NULL,
                     conference = NULL,
                     software = NULL,
                     datasets = NULL,
                     rmd_template = c("svm_cv"),
                     output_file_name = "cv.pdf",
                     output_dir = getwd(),
                     spell_check = FALSE) {

  ## get the rmd file path and other file paths
  if(rmd_template == "svm_cv") {

    template_loc <- fs::path(system.file("templates/", package = "CVR"), "svm-latex-cv.Rmd")
    tex_loc <- fs::path(system.file("templates/", package = "CVR"), "svm-latex-cv.tex")
    lua_mb_loc <- fs::path(system.file("templates/", package = "CVR"), "multiple-bibliographies.lua")
    pandoc_args <- paste0("--lua-filter=", lua_mb_loc)
    csl_loc <- fs::path(system.file("templates/", package = "CVR"), "modapa.csl")

  }


  cv_data <- create_CV_object(data_location = data_location)

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


  if (rmd_template == "svm_cv") {

    rmarkdown::render(input = template_loc,
                      bookdown::pdf_document2(latex_engine = "pdflatex",
                                              template = tex_loc,
                                              number_sections = FALSE,
                                              pandoc_args = pandoc_args),
                      output_file = output_file_name,
                      output_dir = output_dir,
                      intermediates_dir = output_dir)
  }

  else {
    rmarkdown::render(output_file = output_file_name,
                      output_dir = output_dir,
                      intermediates_dir = output_dir)
  }
  if (spell_check) {
    spelling::spell_check_files(path = paste0(output_dir, output_file_name))
  }

  }


