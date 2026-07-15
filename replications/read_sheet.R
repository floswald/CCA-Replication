library(googlesheets4)
library(dplyr)

sheet_url <- "https://docs.google.com/spreadsheets/d/19N5T9-dxrXxM21TiNVzWYLozYGbOlyXKRlzvAAHm5Rw/edit?usp=sharing"

# Auth with cached token; browser flow on first run
gs4_auth(email = "florian.oswald@unito.it")

# Read all sheets
snames <- sheet_names(sheet_url)
message("Sheets found: ", paste(snames, collapse = ", "))

# Read each sheet into a named list
sheets <- lapply(snames, \(s) read_sheet(sheet_url, sheet = s))
names(sheets) <- snames

# Print first sheet
print(sheets[[1]])
