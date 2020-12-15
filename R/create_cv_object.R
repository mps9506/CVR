# create_cv_object is a modified version of the create_cv_object function in
# Nick Strayer's datadrivencv package: https://github.com/nstrayer/datadrivencv
# and licensed under the MIT License

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?

#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
#' @export
#' @importFrom stringr str_detect str_extract
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom lubridate year ymd dmy
#' @importFrom tidyr unite starts_with
#' @importFrom dplyr mutate case_when arrange mutate_all desc
#' @importFrom rlang .data
create_CV_object <-  function(data_location,
                              source_location = "github.com/mps9506",
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    links = c()
  )


  if(sheet_is_publicly_readable){
    # This tells google sheets to not try and authenticate. Note that this will only
    # work if your sheet has sharing set to "anyone with link can view"
    googlesheets4::gs4_deauth()
  } else {
    # My info is in a public sheet so there's no need to do authentication but if you want
    # to use a private sheet, then this is the way you need to do it.
    # designate project-specific cache so we can render Rmd without problems
    options(gargle_oauth_cache = ".secrets")
  }

  read_gsheet <- function(sheet_id){
    googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
  }
  cv$entries_data  <- read_gsheet(sheet_id = "entries")
  cv$skills        <- read_gsheet(sheet_id = "language_skills")
  cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
  cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
  cv$skills_block  <- read_gsheet(sheet_id = "skills_block")



  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

    date_year
  }

  parse_dates <- function(dates){

    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = " \\item ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(.data$description_bullets != "", paste0(" \\item ", .data$description_bullets), ""),
      start = ifelse(.data$start == "NULL", NA, .data$start),
      end = ifelse(.data$end == "NULL", NA, .data$end),
      start_year = extract_year(.data$start),
      end_year = extract_year(.data$end),
      no_start = is.na(.data$start),
      has_start = !.data$no_start,
      no_end = is.na(.data$end),
      has_end = !.data$no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(.data$end),
        has_start & no_end  ~ paste(.data$start, "-", "Current"),
        TRUE                ~ paste(.data$start, "-", .data$end)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(parse_dates(.data$end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

  cv
}

# print_section is a modified version of the print_section function in
# Nick Strayer's datadrivencv package: https://github.com/nstrayer/datadrivencv
# and licensed under the MIT License

#' @title Print Section
#' @description Take a position data frame and the section id desired and prints the section to markdown.
#'
#' @param cv output from create_cv_object
#' @param glue_template glue string
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
#' @param latexify logical, convert strings to latex? default = `FALSE`
#' @return formatted string
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom glue glue_data
#' @keywords internal
#' @importFrom rlang .data
print_section <- function(cv,
                          section_id,
                          glue_template = "default",
                          latexify = FALSE){


  if(glue_template == "default"){
    glue_template <- "
{timeline} \\hfill {institution} \\newline {title} \\hfill {loc} \\newline \\begin{{itemize}} {description_bullets} \\end{{itemize}} \\newline
"
  }

  section_data <- dplyr::filter(cv$entries_data, .data$section == section_id)

  if(latexify == TRUE) {
    section_data <- section_data %>%
      dplyr::mutate(
        institution = latexify(.data$institution, doublebackslash = FALSE),
        title = latexify(.data$title, doublebackslash = FALSE)
        )
  }

  # Take entire entries data frame and removes the links in descending order
  # so links for the same position are right next to each other in number.
  for(i in 1:nrow(section_data)){
    for(col in c('title', 'description_bullets')){
      strip_res <- list(cv = cv, text = section_data[i, col])
      section_data[i, col] <- strip_res$text
      cv <- strip_res$cv
    }
  }

  return(glue::glue_data(section_data, glue_template))
  #print(glue::glue_data(section_data, glue_template))

  #invisible(strip_res$cv)
}

# print_text_block is a modified version of the print_text_block function in
# Nick Strayer's datadrivencv package: https://github.com/nstrayer/datadrivencv
# and licensed under the MIT License

#' @title Print Text Block
#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
#' @return markdown formatted string
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @export
#' @keywords internal
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, .data$loc == label) %>%
    dplyr::pull(.data$text)

  #strip_res <- sanitize_links(cv, text_block)
  strip_res <- list(cv = cv, text = text_block)

  cat(strip_res$text)

  invisible(strip_res$cv)
}


# print_contact_info is a modified version of the print_contact_info function in
# Nick Strayer's datadrivencv package: https://github.com/nstrayer/datadrivencv
# and licensed under the MIT License

#' @title Print Contact Info
#' @description Contact information section with icons
#' @param cv cv object
#' @return formatted string
#' @importFrom glue glue_data
#' @export
#' @keywords internal
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  ) %>% print()

  invisible(cv)
}



#' Print Skills Block
#'
#' @param cv cv object
#' @importFrom glue glue_data
#' @return formatted string
#' @export
#' @keywords internal
print_skills_block <- function(cv) {
  glue_template <- "
\\keywordsentry{{{id}}}{{{text}}}"

  print(glue::glue_data(cv$skills_block, glue_template))
  invisible(cv)
}
