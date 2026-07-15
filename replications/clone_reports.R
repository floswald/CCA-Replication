library(dplyr)

YEAR       <- 2026
CSV_PATH   <- "replications/assignments.csv"
REPORT_DIR <- sprintf("replications/reports-%d", YEAR)

# ── 1. clone or pull each replicator's report repo ─────────────────────────────

url_to_slug <- function(url) sub("^https?://github.com/", "", sub("/$", "", url))

has_branch <- function(dest, branch) {
  system2("git", c("-C", shQuote(dest), "rev-parse", "--verify",
                    paste0("origin/", branch)),
          stdout = FALSE, stderr = FALSE) == 0
}

checkout_report_branch <- function(dest, preferred = "round1") {
  system2("git", c("-C", shQuote(dest), "fetch", "--all"))
  branch <- if (has_branch(dest, preferred)) preferred else {
    system2("git", c("-C", shQuote(dest), "remote", "show", "origin"),
            stdout = TRUE) |>
      grep("HEAD branch", x = _, value = TRUE) |>
      sub(".*: ", "", x = _)
  }
  message(sprintf("checkout %s @ %s", dest, branch))
  system2("git", c("-C", shQuote(dest), "checkout", branch))
  system2("git", c("-C", shQuote(dest), "pull", "--ff-only"))
}

clone_or_pull <- function(url, dest) {
  if (dir.exists(dest)) {
    message(sprintf("fetch %s", dest))
  } else {
    message(sprintf("clone %s -> %s", url, dest))
    system2("gh", c("repo", "clone", url_to_slug(url), shQuote(dest)))
  }
  if (dir.exists(dest)) checkout_report_branch(dest)
}

pull_all_reports <- function(csv_path = CSV_PATH, report_dir = REPORT_DIR) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  if (!"report_repo_url" %in% names(df)) {
    stop("assignments.csv has no report_repo_url column")
  }

  dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)

  df <- df |> filter(nchar(trimws(report_repo_url)) > 0)

  for (i in seq_len(nrow(df))) {
    row  <- df[i, ]
    slug <- sprintf("%s%s_repl_%s%s",
                     row$author_first, row$author_family,
                     row$replicator_first, row$replicator_family)
    slug <- gsub("[^A-Za-z0-9_]", "", slug)
    dest <- file.path(report_dir, slug)
    clone_or_pull(row$report_repo_url, dest)
  }

  invisible(df)
}

# ── 2. render every .qmd report found in the cloned repos ─────────────────────

render_reports <- function(report_dir = REPORT_DIR) {
  repos <- list.dirs(report_dir, recursive = FALSE)

  for (repo in repos) {
    qmds <- list.files(repo, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
    if (length(qmds) == 0) {
      warning(sprintf("no .qmd found in %s", repo))
      next
    }
    for (f in qmds) {
      message(sprintf("render %s", f))
      status <- system2("quarto", c("render", shQuote(f)))
      if (status != 0) warning(sprintf("quarto render failed: %s", f))
    }
  }
}

# ── driver ──────────────────────────────────────────────────────────────────────

pull_all_reports()
render_reports()
