# Download the curated Google Sheet (tabs: isolates, articles, counts, vocab) into data/ and vocab/.
# The sheet must be shared as "Anyone with the link can view"; no authentication is needed.
# Usage: FGSC_SHEET_ID=<id> Rscript scripts/00_fetch_sheet.R
suppressPackageStartupMessages({ library(readr); library(dplyr) })

# Master sheet: FGSCdb_clean in the FGSCDB Drive folder
id <- Sys.getenv("FGSC_SHEET_ID")
if (id == "") id <- "1TFn5SL9aDlGvOspZCuKfcGkZ1DyuN_-SY8Nm0bEqMJw"
tab <- function(name) {
  url <- sprintf("https://docs.google.com/spreadsheets/d/%s/gviz/tq?tqx=out:csv&sheet=%s", id, URLencode(name))
  read_csv(url, col_types = cols(.default = "c"), na = c("", "NA"), show_col_types = FALSE)
}
dir.create("data", showWarnings = FALSE); dir.create("vocab", showWarnings = FALSE)
write_csv(tab("isolates"), "data/fgsc_isolates.csv", na = "")
write_csv(tab("articles"), "data/fgsc_articles.csv", na = "")
write_csv(tab("counts"),   "data/fgsc_counts.csv",   na = "")

# vocab tab -> one CSV per field, in the format the cleaning/validation scripts read
v <- tab("vocab") %>% mutate(across(everything(), ~ coalesce(.x, "")))
out <- list(
  species      = v %>% filter(field == "species")      %>% transmute(original, species = standard),
  tri_genotype = v %>% filter(field == "tri_genotype") %>% transmute(original, tri_genotype = standard, tri_group = group, note),
  host         = v %>% filter(field == "host")         %>% transmute(original, host = standard, host_group = group),
  organ        = v %>% filter(field == "organ")        %>% transmute(original, organ = standard),
  country      = v %>% filter(field == "country")      %>% transmute(original, country = standard, iso3 = note, region = group),
  chemotype    = v %>% filter(field == "chemotype")    %>% transmute(original, chemotype = standard))
for (k in names(out)) write_csv(out[[k]], file.path("vocab", paste0(k, ".csv")), na = "")
cat("Fetched sheet", id, "\n")
