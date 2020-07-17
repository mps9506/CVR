
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CVR

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mps9506/CVR")
```

## About

CVR is a personal R package to automate generation of my CV and resume.
CVR is based heavily on
[datadrivencv](https://github.com/nstrayer/datadrivencv),
[vitae](https://github.com/mitchelloharawild/vitae), and templates
provide by
[svm-r-markdown-templates](https://github.com/svmiller/svm-r-markdown-templates).
There are no plans to release this package since it is heavily
customized for my needs but does not provide any additional
functionality to the above codebases.

The package uses `BibEntry` objects from
[RefManageR](https://github.com/ropensci/RefManageR) to generate the
list of works in the CV. Other entries are retrieved from Google Sheets
following the general format used by
[datadrivencv](https://github.com/nstrayer/datadrivencv).

``` r

library(CVR)
library(RefManageR)

### Generate BibEntry objects
### see https://github.com/ropensci/RefManageR for examples
### I'm using personal Zotero collections here

key <- Sys.getenv("zotero_key")

peer_review <- RefManageR::ReadZotero(group = "2533336",
                                      .params = list(key = key,
                                                   itemType = "journalArticle"))

tech_report <- ReadZotero(group = "2533336",
                          .params = list(key = key,
                                       itemType = "report"))

conference <- ReadZotero(group = "2533336",
                         .params=list(key = key,
                                      itemType = "presentation"))

software <- ReadZotero(group = "2533336",
                       .params = list(key = key,
                                    itemType = "computerProgram"))

datasets <- ReadZotero(group = "2533336",
                       .params = list(key = key,
                                    tag = "dataset"))

build_cv(full_name = "Michael Schramm",
         data_location = "https://docs.google.com/spreadsheets/d/1oeMexIuFWJIJa-xofMLqSvPtg49VceJqM2y3YDqcW-4/edit?usp=sharing",
         peer_review = peer_review,
         tech_report = tech_report,
         conference = conference,
         software = software,
         datasets = datasets,
         output_file_name = "cv.pdf")
```

This should return a pdf in the current working directory.
