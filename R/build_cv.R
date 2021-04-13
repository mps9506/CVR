# build_cv is a modified version of the build_cv function in
# Nick Strayer's datadrivencv package: https://github.com/nstrayer/datadrivencv
# and licensed under the MIT License

#' Build CV
#'
#' @param full_name character string, my name.
#' @param data_location character string, link to google sheets document.
#' @param peer_review character string, file path to .bib file.
#' @param tech_report character string, file path to .bib file.
#' @param conference character string, file path to .bib file.
#' @param software character string, file path to .bib file.
#' @param datasets character string, file path to .bib file.
#' @param rmd_template character string indicating path to rmarkdown template or one of `c('svm_cv', 'yaac')`.
#' @param output_file_name character string for the name of the output document.
#' @param output_dir character string for the name of the output directory, defaults to current working directory.
#' @param spell_check logical use the spell_check package to spell check output? defaults to FALSE
#' @param yml named list of some yml parameters to pass to the rmd. There is probably a better way to do this.
#'
#' @return files written to the current directory
#' @export
#' @importFrom fs path path_package path_wd
#' @importFrom rmarkdown render
#' @importFrom bookdown pdf_document2
#' @importFrom spelling spell_check_files
#' @importFrom rlang warn
build_cv <- function(full_name = "Michael Schramm",
                     data_location = NULL,
                     peer_review = NULL,
                     tech_report = NULL,
                     conference = NULL,
                     software = NULL,
                     datasets = NULL,
                     rmd_template = c("svm_cv", "yaac"),
                     output_file_name = "cv.pdf",
                     output_dir = getwd(),
                     spell_check = FALSE,
                     yml = list(title = "CV",
                                author = "Michael Schramm",
                                firstname = "Michael",
                                lastname = "Schramm",
                                github = "mps9506",
                                phone = "910-232-3760",
                                address = "Bryan, Texas",
                                email = "mpschramm@gmail.com",
                                tagline = "Researcher | Watersheds, water quality and open science")) {

  ## temporarily set user options to keep intermediate files
  ## store the old user options first, then change tinytex.clean, then restore original user options
  old_options <- options()

  if (rmd_template == "svm_cv") {
    path = "templates/svm-latex-cv"
  }

  if (rmd_template == "yaac") {
    path = "templates/yaac"
    options(tinytex.clean = FALSE)
  }

  ## Copy the template files to the working directory first
  ## because when using render, it wants to store the .log and .aux files
  ## where the rmd file is located not in the intermediate or output path

  ## get the rmd file path and other file paths for svm cv template
  full_path <- path_package("mpsCVR", path)
  files <- list.files(full_path, recursive = TRUE)
  if (rmd_template == "yaac") {
    dir.create(fs::path(output_dir, "fonts"))
  }
  file.copy(from = fs::path(full_path, files),
            to = fs::path(output_dir, files))

  if(rmd_template == "svm_cv") {

    template_loc <- fs::path(output_dir, "svm-latex-cv.Rmd")
    tex_loc <- fs::path(output_dir, "svm-latex-cv.tex")

    # Inject multiple-bibliographies lua filter
    # code from Mitchell O'Hara-Wild's vitae package
    # https://github.com/mitchelloharawild/vitae
    lua_mb_loc <- fs::path(output_dir, "multiple-bibliographies.lua")
    if(rmarkdown::pandoc_version() <= package_version("2.7.3")) {
      warn(sprintf("Detected pandoc version %s, which may cause issues with bibliography_entries().
Please update pandoc if you have any issues knitting bibliographies (this can be done by updating RStudio).", rmarkdown::pandoc_version()))
    }
    cat(
      gsub("<<PANDOC_PATH>>", rmarkdown::find_pandoc()$dir, fixed = TRUE,
           readLines(fs::path_package("mpsCVR", "templates/svm-latex-cv/multiple-bibliographies.lua"), encoding = "UTF-8")),
      file = lua_mb_loc, sep = "\n"
    )
    pandoc_args <- paste0("--lua-filter=", lua_mb_loc)
    csl_loc <- fs::path(output_dir, "modapa.csl")

  }

  ## get the rmd file path and other file paths for yaac resume
  if(rmd_template == "yaac") {

    template_loc <- fs::path(output_dir, "yaac.Rmd")
    tex_loc <- fs::path(output_dir, "yaac.tex")
    doc_class <- fs::path(output_dir, "yaac-another-awesome-cv.cls")
    # Inject multiple-bibliographies lua filter
    # code from Mitchell O'Hara-Wild's vitae package
    # https://github.com/mitchelloharawild/vitae
    lua_mb_loc <- fs::path(output_dir, "multiple-bibliographies.lua")
    if(rmarkdown::pandoc_version() <= package_version("2.7.3")) {
      warn(sprintf("Detected pandoc version %s, which may cause issues with bibliography_entries().
Please update pandoc if you have any issues knitting bibliographies (this can be done by updating RStudio).", rmarkdown::pandoc_version()))
    }
    cat(
      gsub("<<PANDOC_PATH>>", rmarkdown::find_pandoc()$dir, fixed = TRUE,
           readLines(fs::path_package("mpsCVR", "templates/yaac/multiple-bibliographies.lua"), encoding = "UTF-8")),
      file = lua_mb_loc, sep = "\n"
    )
    pandoc_args <- paste0("--lua-filter=", lua_mb_loc)
    csl_loc <- fs::path(output_dir, "modapa.csl")


  }


  cv_data <- create_CV_object(data_location = data_location)

  if(!is.null(peer_review)) {
    peer_review_path <- peer_review
  }

  if(!is.null(tech_report)) {
    tech_report_path <- tech_report
  }

  if(!is.null(conference)) {
    conf_path <- conference
  }

  if(!is.null(software)) {
    soft_path <- software
  }

  if(!is.null(datasets)) {
    data_path <- datasets
  }


  ## yaac needs to use lualatex
  if (rmd_template == "svm_cv") {
    latex_engine <- "xelatex"
  }
  else {
    latex_engine <- "lualatex"
  }


  if (rmd_template %in% c("svm_cv", "yaac")) {

    rmarkdown::render(input = template_loc,
                      output_format = bookdown::pdf_document2(latex_engine = latex_engine,
                                              template = tex_loc,
                                              number_sections = FALSE,
                                              pandoc_args = pandoc_args,
                                              keep_tex = FALSE,
                                              md_extensions = "-autolink_bare_uris+raw_attribute"),
                      output_file = output_file_name,
                      output_dir = output_dir,
                      intermediates_dir = output_dir,
                      clean = TRUE)
  }


  else {
    rmarkdown::render(output_file = output_file_name,
                      output_dir = output_dir,
                      intermediates_dir = output_dir)
  }

  ## spell check
  if (spell_check) {
    spelling::spell_check_files(path = paste0(output_dir, output_file_name))
  }

  ## clean up any generated files
  file.remove(fs::path(output_dir, files))
  if (rmd_template == "yaac") {
    unlink(fs::path(output_dir, "fonts"), recursive = TRUE)
  }

  on.exit(options(old_options), add = TRUE)

  }


