#' Defines additional questions asked before CRAN submission.
#' DO NOT EXPORT!
release_questions <- function() {
  c(
    "Updated NEWS.md?",
    "Updated README.md?",
    "Re-run reverse dependencies?",
    "Everything checked?",
    "Abide good practices?",
    "Updated cran-comments.md?",
    "Updated vignettes?",
    "Version upped?"
  )
}
