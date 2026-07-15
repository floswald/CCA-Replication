library(httr2)
library(dplyr)

zenodo_key <- Sys.getenv("ZENODO")
if (nchar(zenodo_key) == 0) stop("ZENODO env var not set")

community_slug <- "cca-packages"

# Resolve slug → UUID (required for request queries)
get_community_uuid <- function(slug, key) {
  request(sprintf("https://zenodo.org/api/communities/%s", slug)) |>
    req_headers(Authorization = paste("Bearer", key)) |>
    req_perform() |>
    resp_body_json() |>
    (\(x) x$id)()
}

# Fetch all submitted (pending review) community-submission requests
fetch_pending_submissions <- function(community_uuid, key, size = 100) {
  q <- sprintf(
    'receiver.community:"%s" AND type:community-submission AND status:submitted',
    community_uuid
  )
  request("https://zenodo.org/api/requests") |>
    req_headers(Authorization = paste("Bearer", key)) |>
    req_url_query(q = q, sort = "newest", size = size, page = 1) |>
    req_perform() |>
    resp_body_json()
}

community_uuid <- get_community_uuid(community_slug, zenodo_key)
message(sprintf("Community UUID: %s", community_uuid))

result <- fetch_pending_submissions(community_uuid, zenodo_key)
hits   <- result$hits$hits
message(sprintf("Pending submissions: %d", length(hits)))

if (length(hits) > 0) {
  df <- tibble(
    request_id = vapply(hits, \(x) x$id,                       character(1)),
    title      = vapply(hits, \(x) x$title %||% NA_character_,  character(1)),
    status     = vapply(hits, \(x) x$status,                   character(1)),
    created    = vapply(hits, \(x) x$created,                  character(1)),
    user_id    = vapply(hits, \(x) as.character(x$created_by$user %||% NA), character(1)),
    record_id  = vapply(hits, \(x) x$topic$record %||% NA_character_, character(1))
  )
  print(df)
} else {
  message("No pending submissions found.")
}
