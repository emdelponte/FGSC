# Validate the curated tables before the site is built.
# Stops with a readable report when something would corrupt the database;
# warnings are printed but do not stop the build.
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })

iso <- read_csv("data/fgsc_isolates.csv", col_types = cols(.default = "c"))
art <- read_csv("data/fgsc_articles.csv", col_types = cols(.default = "c"))
cnt <- read_csv("data/fgsc_counts.csv", col_types = cols(.default = "c"))
voc <- function(f, col) unique(read_csv(file.path("vocab", f), col_types = cols(.default = "c"), na = character())[[col]])

errors <- character(); warns <- character()
err  <- function(ok, msg, ids = NULL) if (any(!ok, na.rm = TRUE)) errors <<- c(errors, sprintf("%s (%d): %s", msg, sum(!ok, na.rm = TRUE), paste(head(ids[!ok], 8), collapse = ", ")))
warn <- function(ok, msg, ids = NULL) if (any(!ok, na.rm = TRUE)) warns  <<- c(warns,  sprintf("%s (%d): %s", msg, sum(!ok, na.rm = TRUE), paste(head(ids[!ok], 8), collapse = ", ")))

# isolates
req <- c("isolate_id", "article_id", "reference", "species", "tri_genotype", "tri_group", "host", "host_group",
         "country", "iso3", "latitude", "longitude", "coord_precision")
miss <- setdiff(req, names(iso)); if (length(miss)) stop("isolates: missing columns ", paste(miss, collapse = ", "))
err(!duplicated(iso$isolate_id), "isolates: duplicated isolate_id", iso$isolate_id)
err(iso$species %in% voc("species.csv", "species"), "isolates: species not in vocab/species.csv", iso$species)
err(iso$tri_genotype %in% voc("tri_genotype.csv", "tri_genotype"), "isolates: TRI genotype not in vocab", iso$tri_genotype)
err(iso$tri_group %in% voc("tri_genotype.csv", "tri_group"), "isolates: TRI group not in vocab", iso$tri_group)
err(iso$host %in% voc("host.csv", "host"), "isolates: host not in vocab/host.csv", iso$host)
err(iso$host_group %in% voc("host.csv", "host_group"), "isolates: host group not in vocab", iso$host_group)
err(iso$country %in% voc("country.csv", "country"), "isolates: country not in vocab/country.csv", iso$country)
lat <- suppressWarnings(as.numeric(iso$latitude)); lon <- suppressWarnings(as.numeric(iso$longitude))
err(!is.na(lat) & abs(lat) <= 90, "isolates: latitude missing or out of range", iso$isolate_id)
err(!is.na(lon) & abs(lon) <= 180, "isolates: longitude missing or out of range", iso$isolate_id)
err(iso$coord_precision %in% c("reported", "locality (from same-location records)", "municipality (geocoded)", "county centroid", "country centroid"),
    "isolates: unknown coord_precision", iso$coord_precision)
yr <- suppressWarnings(as.integer(iso$year))
err(is.na(iso$year) | (!is.na(yr) & yr >= 1900 & yr <= as.integer(format(Sys.Date(), "%Y"))), "isolates: invalid year", iso$isolate_id)
err(iso$reference %in% art$reference, "isolates: reference not listed in articles", iso$reference)
warn(is.na(iso$strain_code) | !duplicated(paste(iso$article_id, iso$strain_code)), "isolates: strain code repeated within an article", iso$isolate_id)

# articles
err(!duplicated(art$article_id), "articles: duplicated article_id", art$article_id)
err(!duplicated(art$reference), "articles: duplicated reference label", art$reference)
warn(!is.na(art$doi) | art$reference == "Unpublished", "articles: DOI missing", art$reference)

# counts (aggregated data)
if (nrow(cnt)) {
  err(cnt$geo_level %in% c("country", "admin1", "admin2", "locality"), "counts: geo_level must be country/admin1/admin2/locality", cnt$count_id)
  n <- suppressWarnings(as.integer(cnt$n_isolates))
  err(!is.na(n) & n > 0, "counts: n_isolates must be a positive integer", cnt$count_id)
  err(cnt$species %in% c(voc("species.csv", "species"), "not crossed"), "counts: species not in vocab (or 'not crossed')", cnt$species)
  err(cnt$tri_genotype %in% c(voc("tri_genotype.csv", "tri_genotype"), "not crossed"), "counts: TRI genotype not in vocab (or 'not crossed')", cnt$tri_genotype)
  err(!(cnt$article_id %in% iso$article_id), "counts: article already has isolate-level rows (double counting)", cnt$article_id)
  err(cnt$country %in% voc("country.csv", "country"), "counts: country not in vocab", cnt$country)
}

if (length(warns)) { cat("WARNINGS\n"); cat(paste0("- ", warns, "\n"), sep = "") }
if (length(errors)) { cat("ERRORS\n"); cat(paste0("- ", errors, "\n"), sep = ""); quit(status = 1) }
cat("Validation passed:", nrow(iso), "isolates,", nrow(art), "articles,", nrow(cnt), "count rows\n")
