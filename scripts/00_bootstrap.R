# scripts/00_bootstrap.R
# Bootstrap script for dietrecallkit
# Run once from the repo root (new or existing package dir).
# Idempotent: safe to re-run.

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  usethis.quiet = TRUE
)

need <- function(pkgs) pkgs[!pkgs %in% rownames(installed.packages())]

# ---- 0) Ensure core tooling ----
core <- c("pkgbuild","usethis","devtools","roxygen2","testthat","desc","git2r")
to_get <- need(core)
if (length(to_get)) install.packages(to_get)

# Build tools check
if (!pkgbuild::has_build_tools(debug = TRUE)) {
  stop(
    paste(
      "C/C++ build tools not found.",
      "On Windows: install Rtools appropriate for your R version.",
      "On macOS: install Xcode Command Line Tools (xcode-select --install).",
      "On Linux: install build-essential (gcc/g++/make).",
      sep = "\n- "
    ),
    call. = FALSE
  )
}

# Optional speed-up: pak
if (!"pak" %in% rownames(installed.packages())) {
  try(install.packages("pak"), silent = TRUE)
}

install_if_missing <- function(pkgs) {
  miss <- need(pkgs)
  if (!length(miss)) return(invisible(TRUE))
  if ("pak" %in% rownames(installed.packages())) {
    pak::pak(miss)
  } else {
    install.packages(miss)
  }
  invisible(TRUE)
}

# Development + documentation + linting helpers
install_if_missing(c(
  "pkgdown","lintr","covr","rmarkdown","knitr",
  "ggplot2","dplyr","tibble","tidyr","rlang"
))

# ---- 1) Seed DESCRIPTION if missing ----
if (!file.exists("DESCRIPTION")) {
  usethis::ui_silence({
    usethis::use_description(fields = list(
      Package = "dietrecallkit",
      Title = "Toolkit for Cleaning and Processing Dietary Recall Data and Indicators",
      Version = "0.0.0.9000",
      `Authors@R` = 'person("Tosin", "Akingbemisilu", email = "tosinakingbemisilu@gmail.com", role = c("aut","cre"))',
      Description = "Utilities to clean, process, and generate indicators from dietary recall data.",
      License = "MIT + file LICENSE",
      Encoding = "UTF-8",
      Roxygen = "list(markdown = TRUE)",
      Depends = "R (>= 4.1.0)",
      LazyData = "false"
    ))
  })
}

# ---- 2) Normalize DESCRIPTION fields ----
desc <- desc::desc(file = "DESCRIPTION")
desc$set("Package", "dietrecallkit")  # enforce valid package name

# Ensure Imports
ensure_imports <- c("stats","tibble","dplyr","tidyr","ggplot2","rlang")
current_imports <- unlist(strsplit(desc$get("Imports") %||% "", "\\s*,\\s*"))
missing_imports <- setdiff(ensure_imports, current_imports)
if (length(missing_imports)) {
  all_imports <- unique(c(current_imports[current_imports != ""], missing_imports))
  desc$set("Imports", paste(sort(all_imports), collapse = ",\n    "))
}

# Ensure Suggests
ensure_suggests <- c("testthat (>= 3.0.0)","rmarkdown","knitr","pkgdown","lintr","covr")
current_suggests <- unlist(strsplit(desc$get("Suggests") %||% "", "\\s*,\\s*"))
missing_suggests <- setdiff(ensure_suggests, current_suggests)
if (length(missing_suggests)) {
  all_suggests <- unique(c(current_suggests[current_suggests != ""], missing_suggests))
  desc$set("Suggests", paste(sort(all_suggests), collapse = ",\n    "))
}

desc$write(file = "DESCRIPTION")

# ---- 3) Project niceties ----
if (!file.exists("R")) dir.create("R")
if (!file.exists("README.Rmd")) usethis::ui_silence(usethis::use_readme_rmd(open = FALSE))
if (!file.exists("NEWS.md"))   usethis::ui_silence(usethis::use_news_md(open = FALSE))
usethis::ui_silence(usethis::use_roxygen_md())
usethis::ui_silence(usethis::use_testthat(edition = 3))
usethis::ui_silence(usethis::use_mit_license("Tosin Akingbemisilu"))
usethis::ui_silence(usethis::use_git_ignore(c(".Rhistory", ".Rproj.user", ".DS_Store", "inst/doc")))
usethis::ui_silence(usethis::use_build_ignore(c("scripts", "README.Rmd", "LICENSE.md", ".github", ".Rproj")))

# Optional: lintr config
if (!file.exists(".lintr")) {
  writeLines("linters: lintr::with_defaults(line_length_linter = lintr::line_length_linter(120))", ".lintr")
  usethis::ui_silence(usethis::use_build_ignore(".lintr"))
}

# ---- 4) Seed NEWS and minimal test ----
if (!file.exists("NEWS.md")) {
  writeLines(c(
    "# dietrecallkit 0.0.0.9000",
    "",
    "* Initial scaffolding commit (package setup with devtools/usethis)."
  ), "NEWS.md")
}

# ---- 5) Dummy function (to make check pass) ----
if (!file.exists("R/hello.R")) {
  writeLines(c(
    "#' Hello World Function",
    "#'",
    "#' This is a placeholder function to ensure the package passes R CMD check.",
    "#' Delete once you add real functions.",
    "#' ",
    "#' @return A character string.",
    "#' @export",
    "hello_world <- function() {",
    "  'Hello, dietary recall world!'",
    "}"
  ), "R/hello.R")
}

# ---- 6) Dummy test for dummy function ----
if (!file.exists("tests/testthat/test-hello.R")) {
  writeLines(c(
    "test_that('hello_world works', {",
    "  expect_equal(hello_world(), 'Hello, dietary recall world!')",
    "})"
  ), "tests/testthat/test-hello.R")
}

# ---- 7) RStudio project ----
if (!file.exists(paste0(basename(getwd()), ".Rproj"))) {
  usethis::ui_silence(usethis::use_rstudio())
}

# ---- 8) Git: auto-commit scaffolding if fresh repo ----
if (git2r::in_repository(getwd())) {
  repo <- git2r::repository(getwd())
  commits <- git2r::commits(repo)
  if (length(commits) == 0) {
    git2r::add(repo, ".")
    git2r::commit(repo, "Initial scaffolding commit")
    message("Git: created initial commit for scaffolding.")
  } else {
    message("Git: repository already has commits, leaving history untouched.")
  }
}

# ---- 9) Generate docs ----
devtools::document()    # generate NAMESPACE and man/
devtools::build_readme() # Build readme

# ---- 10) Success message ----
pkg_name <- unname(desc::desc_get("Package"))
message(sprintf("Bootstrap complete. Package bootstrapped as: %s", pkg_name))
message("Run devtools::check() when ready.")
