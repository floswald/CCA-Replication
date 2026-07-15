library(googlesheets4)
library(httr2)
library(dplyr)

# ── year-specific config: this is the only block to edit for a new cohort ──────
YEAR           <- 2026
SHEET_URL      <- "https://docs.google.com/spreadsheets/d/19N5T9-dxrXxM21TiNVzWYLozYGbOlyXKRlzvAAHm5Rw/edit?usp=sharing"
COMMUNITY_SLUG <- "cca-packages"
OUT_CSV        <- "replications/assignments.csv"

# ── helpers ────────────────────────────────────────────────────────────────────

zenodo_get <- function(path, key, ...) {
  request(paste0("https://zenodo.org/api/", path)) |>
    req_headers(Authorization = paste("Bearer", key)) |>
    req_url_query(...) |>
    req_perform() |>
    resp_body_json()
}

# token-set overlap match: handles "Luca Emanuele, Marcianò" ↔ "Luca Emanuele Marcianò"
name_tokens <- function(s) {
  s |> tolower() |> gsub("[^a-zàáâãäåèéêëìíîïòóôõöùúûü ]", "", x = _) |>
    strsplit("\\s+") |> unlist() |> unique()
}

match_name <- function(zenodo_name, sheet_first, sheet_family) {
  z  <- name_tokens(zenodo_name)
  sf <- name_tokens(paste(sheet_first, sheet_family))
  length(intersect(z, sf)) >= 2
}

# derangement: no element at its own index
random_derangement <- function(n, seed) {
  set.seed(seed)
  repeat {
    perm <- sample(n)
    if (!any(perm == seq_len(n))) return(perm)
  }
}

# ── 1. Google Sheet ────────────────────────────────────────────────────────────

get_sheet <- function(url) {
  gs4_auth(email = "florian.oswald@unito.it")
  df <- read_sheet(url)
  df |>
    rename(
      first_name   = `First name`,
      family_name  = `Family name`,
      paper_title  = `Title of your paper/project`,
      year_phd     = `Year of PhD`,
      software     = `software used`,
      email        = `Email address`
    ) |>
    mutate(student_id = row_number())
}

# ── 2. Zenodo pending submissions ──────────────────────────────────────────────

get_zenodo_submissions <- function(slug, key) {
  uuid <- zenodo_get(sprintf("communities/%s", slug), key)$id
  message(sprintf("Community UUID: %s", uuid))

  q <- sprintf(
    'receiver.community:"%s" AND type:community-submission AND status:submitted',
    uuid
  )
  hits <- zenodo_get("requests", key, q = q, sort = "newest", size = 100)$hits$hits
  message(sprintf("Pending submissions: %d", length(hits)))

  # Fetch draft metadata for each record
  lapply(hits, \(h) {
    rec_id   <- h$topic$record
    meta     <- zenodo_get(sprintf("records/%s/draft", rec_id), key)$metadata
    creator  <- meta$creators[[1]]$name  # "Family, Given" legacy format
    list(
      record_id      = rec_id,
      zenodo_title   = meta$title,
      creator_name   = creator,
      request_id     = h$id,
      zenodo_user_id = as.character(h$created_by$user)
    )
  }) |> bind_rows()
}

# ── 3. Match students ↔ packages ───────────────────────────────────────────────

match_students_to_packages <- function(students, packages) {
  packages$student_id <- NA_integer_

  for (i in seq_len(nrow(packages))) {
    zname <- packages$creator_name[i]
    for (j in seq_len(nrow(students))) {
      if (match_name(zname, students$first_name[j], students$family_name[j])) {
        packages$student_id[i] <- students$student_id[j]
        break
      }
    }
  }

  unmatched <- packages |> filter(is.na(student_id))
  if (nrow(unmatched) > 0) {
    warning(sprintf(
      "Unmatched packages:\n%s",
      paste(unmatched$creator_name, collapse = "\n")
    ))
  }

  packages
}

# ── 4. Assign replicators (derangement) ────────────────────────────────────────

assign_replicators <- function(students, packages, seed) {
  # work on matched packages only, ordered by student_id
  matched <- packages |>
    filter(!is.na(student_id)) |>
    arrange(student_id)

  n    <- nrow(matched)
  perm <- random_derangement(n, seed)

  matched |>
    mutate(replicator_student_id = matched$student_id[perm]) |>
    left_join(
      students |> select(student_id, first_name, family_name, email, paper_title),
      by = "student_id"
    ) |>
    left_join(
      students |>
        select(student_id, first_name, family_name, email) |>
        rename(
          replicator_student_id = student_id,
          replicator_first      = first_name,
          replicator_family     = family_name,
          replicator_email      = email
        ),
      by = "replicator_student_id"
    ) |>
    select(
      author_first    = first_name,
      author_family   = family_name,
      author_email    = email,
      paper_title,
      zenodo_title,
      record_id,
      zenodo_user_id,
      replicator_first,
      replicator_family,
      replicator_email
    )
}

# ── 5. Invite students as community members ────────────────────────────────────

invite_community_members <- function(community_uuid, zenodo_user_ids, key,
                                     role = "reader", dry_run = TRUE) {
  results <- lapply(zenodo_user_ids, \(uid) {
    body <- list(
      members = list(list(id = uid, type = "user")),
      role    = role,
      visible = FALSE
    )

    if (dry_run) {
      message(sprintf("[dry-run] would invite user %s as %s", uid, role))
      return(list(user_id = uid, status = "dry-run"))
    }

    resp <- request(
      sprintf("https://zenodo.org/api/communities/%s/invitations", community_uuid)
    ) |>
      req_headers(Authorization = paste("Bearer", key),
                  `Content-Type` = "application/json") |>
      req_body_json(body) |>
      req_error(is_error = \(x) FALSE) |>
      req_perform()

    status <- resp_status(resp)
    message(sprintf("user %s → HTTP %d", uid, status))
    list(user_id = uid, http_status = status,
         ok = status %in% c(200L, 201L, 204L))
  })

  bind_rows(results)
}

# ── 6. Generate replicator email specs ────────────────────────────────────────

replicator_email_html <- function(row, deadline) {
  deposit_url <- sprintf("https://zenodo.org/records/%s?preview=1", row$record_id)
  template_url <- "https://github.com/JPE-Reproducibility/JPEtemplate"

  sprintf('
Hi %s!
<br><br>
I have assigned you a replication package to check as part of the
<b>CCA PhD Replication Project</b>. Please review the package below and
submit your report by the deadline.

<h2>Getting Started &#x1F4BB;</h2>

<table border="1" cellpadding="4" cellspacing="0" width="100%%">
<tr>
  <td width="25%%"><b>Package title</b></td>
  <td>%s</td>
</tr>
<tr>
  <td width="25%%"><b>Zenodo deposit link</b></td>
  <td><a href="%s">%s</a></td>
</tr>
<tr>
  <td width="25%%"><b>Replication template</b></td>
  <td><a href="%s">%s</a> &mdash; click &ldquo;Use this template&rdquo;</td>
</tr>
<tr>
  <td width="25%%"><b>Deadline</b></td>
  <td>%s</td>
</tr>
</table>

<h2>Steps &#x1F5D2;&#xFE0F;</h2>
<ol>
  <li>Log in to <a href="https://zenodo.org">zenodo.org</a> with your credentials
      and open the deposit link above. You need to be logged in as a community
      member to access the files.</li>
  <li>Go to the template repo above and click <b>Use this template</b> to create
      your own copy. Follow the README instructions.</li>
  <li>Edit the <code>.qmd</code> report file in the repo and compile it to PDF
      (recommended: quarto with the typst engine).</li>
  <li>Once your report is committed and pushed, open an issue on your repo and
      ping <code>@floswald</code> with a short summary of your findings.</li>
</ol>

<h2>Deadline &#x23F0;</h2>

Your report is due by <b>%s</b> (2 weeks from today).

<h2>&#x26A0;&#xFE0F; Do NOT accept or decline on Zenodo</h2>

When you open the deposit, Zenodo may show you an <b>Accept</b> or <b>Decline</b>
button for the community submission. <b>Do not click either.</b>
I will handle that myself after reviewing your report.
<br><br>

<h2>Warning &#x26A0;&#xFE0F;</h2>

&#x26A0;&#xFE0F; <b>This assignment is confidential.</b> Do not forward,
share, or discuss the contents of the package with anyone outside this
project. Treat it as you would a confidential journal submission.
<br><br>

<h2>Questions?</h2>

Reach out by email or open a GitHub issue and tag me. Happy to help with
computational or logistics questions.
<br><br>
Thanks for participating!
<br><br>
Florian
',
    row$replicator_first,        # Hi NAME
    row$zenodo_title,            # package title
    deposit_url, deposit_url,    # link × 2
    template_url, template_url,  # template × 2
    deadline,                    # table deadline
    deadline                     # body deadline
  )
}

generate_replicator_emails <- function(csv_path = OUT_CSV,
                                       deadline = format(Sys.Date() + 14, "%B %d, %Y")) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(df)), \(i) {
    row <- df[i, ]
    list(
      to      = row$replicator_email,
      subject = sprintf("[CCA Replication %d] Your assigned package: %s", YEAR, row$zenodo_title),
      html    = replicator_email_html(row, deadline)
    )
  })
}

# ── driver ─────────────────────────────────────────────────────────────────────

run_pipeline <- function(seed = 42, invite = FALSE, invite_role = "reader") {
  key <- Sys.getenv("ZENODO")
  if (nchar(key) == 0) stop("ZENODO env var not set")

  message("── 1. Reading Google Sheet")
  students <- get_sheet(SHEET_URL)

  message("── 2. Fetching Zenodo submissions")
  packages <- get_zenodo_submissions(COMMUNITY_SLUG, key)

  message("── 3. Matching students to packages")
  packages <- match_students_to_packages(students, packages)

  message("── 4. Randomly assigning replicators (seed=", seed, ")")
  result <- assign_replicators(students, packages, seed)

  message("── 5. Writing CSV: ", OUT_CSV)
  write.csv(result, OUT_CSV, row.names = FALSE)

  print(result |> select(author_family, paper_title, replicator_family))

  if (invite || !interactive()) {
    community_uuid <- zenodo_get(sprintf("communities/%s", COMMUNITY_SLUG), key)$id
    user_ids <- unique(result$zenodo_user_id)
    message(sprintf("── 6. Inviting %d users as '%s' (dry_run=%s)",
                    length(user_ids), invite_role, !invite))
    inv <- invite_community_members(community_uuid, user_ids, key,
                                    role = invite_role, dry_run = !invite)
    print(inv)
  }

  invisible(result)
}

run_pipeline(seed = 42, invite = TRUE, invite_role = "reader")
