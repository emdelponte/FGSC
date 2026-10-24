# FGSCdb — cleaning & standardization pipeline
# Input : raw/isolates_database_long.xlsx (export of the Google Sheet used by the Shiny app)
#         vocab/*.csv (controlled vocabularies: original -> standard)
#         geo/ne_10m_admin_0.geojson, geo/us_counties.json (for coordinate checks/repairs)
# Output: data/ (clean CSVs + JSON for the website) and FGSCdb_clean.xlsx
#
# Principle: nothing is silently dropped. Every changed value goes to changelog,
# every suspicious record goes to flags, and the original sheet stays untouched.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(readr); library(purrr); library(jsonlite); library(sf); library(openxlsx)
})
suppressMessages(sf_use_s2(FALSE))
dir.create("data", showWarnings = FALSE)

raw <- read_excel("raw/isolates_database_long.xlsx", col_types = "text")
vocab <- function(f) read_csv(file.path("vocab", f), col_types = cols(.default = "c"),
                              na = character()) %>% mutate(original = na_if(original, ""))

changelog <- list()
log_change <- function(df, field, old, new, rule) {
  ch <- tibble(isolate_id = df$isolate_id, field = field, old = old, new = new, rule = rule) %>%
    filter(!(is.na(old) & is.na(new)), is.na(old) | is.na(new) | old != new) %>%
    # numbers stored as text ("2007.0" vs "2007", or tiny rounding) are not changes
    filter(is.na(old) | is.na(new) | is.na(suppressWarnings(as.numeric(old))) |
             is.na(suppressWarnings(as.numeric(new))) |
             abs(suppressWarnings(as.numeric(old)) - suppressWarnings(as.numeric(new))) > 1e-6)
  changelog[[length(changelog) + 1]] <<- ch
  invisible(ch)
}
squish_na <- function(x) { x <- str_squish(x); x[x %in% c("", "NA", "na", "N/A")] <- NA; x }

# ---- 1. base table ---------------------------------------------------------
d <- raw %>%
  mutate(across(everything(), squish_na)) %>%
  mutate(isolate_id = sprintf("FGSC%05d", row_number()),
         article_id = as.integer(sheet_id),
         row_in_article = as.integer(ID))

# ---- 2. vocabularies -------------------------------------------------------
map_vocab <- function(d, col, vfile, outcols, rule) {
  v <- vocab(vfile)
  unmatched <- setdiff(unique(d[[col]]), v$original)
  if (length(unmatched)) stop(sprintf("Values of %s not in vocab/%s: %s", col, vfile,
                                      paste(unmatched, collapse = " | ")))
  out <- d %>% left_join(v %>% select(original, all_of(outcols)), by = setNames("original", col))
  log_change(out, col, out[[col]], out[[outcols[1]]], rule)
  out
}
d <- d %>%
  map_vocab("FGSC", "species.csv", "species", "vocab: species") %>%
  map_vocab("TRI_genotype", "tri_genotype.csv", c("tri_genotype", "tri_group"), "vocab: TRI genotype") %>%
  map_vocab("Host", "host.csv", c("host", "host_group"), "vocab: host") %>%
  map_vocab("Organ", "organ.csv", "organ", "vocab: organ") %>%
  map_vocab("Country", "country.csv", c("country", "iso3", "region"), "vocab: country")

# Sardinia was coded as a country: keep the information in the location field
sard <- which(d$Country == "Sardinia")
old <- d$Location.name[sard]
d$Location.name[sard] <- paste0(d$Location.name[sard], ", Sardinia")
log_change(d[sard, ], "Location.name", old, d$Location.name[sard], "Sardinia moved from country to location")

# chemotype: only the listed spellings change, everything else is kept as is
chv <- vocab("chemotype.csv")
d <- d %>% mutate(chemotype = coalesce(chv$chemotype[match(Chemotype, chv$original)], Chemotype))
log_change(d, "Chemotype", d$Chemotype, d$chemotype, "vocab: chemotype")

# DON/ZEA genotype: keep the ZEA information as a comment
dz <- which(raw$TRI_genotype %in% "DON/ZEA")
d$Comments[dz] <- str_c(coalesce(d$Comments[dz], ""), if_else(is.na(d$Comments[dz]), "", "; "),
                        "original TRI genotype label: DON/ZEA")

# cultivar, year, reference
d <- d %>% mutate(
  cultivar = if_else(str_to_lower(Cultivar) %in% c("unknown", "unkown"), NA_character_, Cultivar),
  year = suppressWarnings(as.integer(Year)),
  reference = coalesce(Pub1, "Unpublished"),
  genbank = str_squish(str_replace_all(GenBank.accession.number, "\\s*\\n\\s*", " ")),
  location = if_else(str_to_lower(Location.name) %in% c("unknown"), NA_character_, Location.name)
)
log_change(d, "Cultivar", d$Cultivar, d$cultivar, "unknown/unkown -> missing")
log_change(d, "Year", d$Year, as.character(d$year), "non-numeric year ('unknown') -> missing")
log_change(d, "Pub1", d$Pub1, d$reference, "missing reference -> 'Unpublished'")

# ---- 3. coordinates --------------------------------------------------------
d <- d %>% mutate(lat = suppressWarnings(as.numeric(Latitude)),
                  lon = suppressWarnings(as.numeric(Longitude)))
malformed <- !is.na(d$Latitude) & is.na(d$lat) | !is.na(d$Longitude) & is.na(d$lon)
d$lat[malformed] <- NA; d$lon[malformed] <- NA
# a coordinate pair is valid only if both are present and in range
bad_pair <- xor(is.na(d$lat), is.na(d$lon)) | abs(d$lat) > 90 | abs(d$lon) > 180
bad_pair[is.na(bad_pair)] <- FALSE
d$lat[bad_pair] <- NA; d$lon[bad_pair] <- NA
d$coord_precision <- if_else(!is.na(d$lat), "reported", NA_character_)

# (a) borrow from other records of the same article + same location
sib <- d %>% filter(!is.na(lat), !is.na(location)) %>%
  group_by(article_id, location) %>%
  summarise(lat_s = median(lat), lon_s = median(lon), .groups = "drop")
d <- d %>% left_join(sib, by = c("article_id", "location")) %>%
  mutate(fill = is.na(lat) & !is.na(lat_s),
         coord_precision = if_else(fill, "locality (from same-location records)", coord_precision),
         lat = if_else(fill, lat_s, lat), lon = if_else(fill, lon_s, lon)) %>%
  select(-lat_s, -lon_s, -fill)

# (b) US county centroid when the location names a county + state
counties <- st_read("geo/us_counties.json", quiet = TRUE)
state_fips <- c(Nebraska = "31", Kansas = "20", "North Dakota" = "38", "South Dakota" = "46",
                Minnesota = "27", Kentucky = "21", "North Carolina" = "37", Virginia = "51")
cc <- suppressWarnings(st_coordinates(st_point_on_surface(counties)))
counties <- counties %>% st_drop_geometry() %>%
  mutate(c_lon = cc[, 1], c_lat = cc[, 2], key = paste(STATE, str_to_lower(NAME)))
need <- which(is.na(d$lat) & d$country == "United States" & !is.na(d$location))
for (i in need) {
  parts <- str_split(d$location[i], ",\\s*")[[1]]
  st <- state_fips[intersect(parts, names(state_fips))]
  if (length(st) == 1) {
    hit <- counties %>% filter(key == paste(st, str_to_lower(parts[1])))
    if (nrow(hit) == 1) {
      d$lat[i] <- hit$c_lat; d$lon[i] <- hit$c_lon; d$coord_precision[i] <- "county centroid"
    }
  }
}

# (c) country centroid as last resort, so the record still counts in maps by country
world <- st_read("geo/ne_10m_admin_0.geojson", quiet = TRUE) %>%
  select(ADM0_A3, ISO_A3, ADMIN) %>%
  mutate(iso3 = if_else(ISO_A3 == "-99", ADM0_A3, ISO_A3))
wc <- suppressWarnings(st_coordinates(st_point_on_surface(world)))
cent <- tibble(iso3 = world$iso3, c_lon = wc[, 1], c_lat = wc[, 2]) %>% distinct(iso3, .keep_all = TRUE)
d <- d %>% left_join(cent, by = "iso3") %>%
  mutate(fill = is.na(lat),
         coord_precision = if_else(fill, "country centroid", coord_precision),
         lat = if_else(fill, c_lat, lat), lon = if_else(fill, c_lon, lon)) %>%
  select(-c_lat, -c_lon, -fill)
log_change(d, "Latitude", raw$Latitude, as.character(round(d$lat, 6)), "coordinate repaired/imputed")
log_change(d, "Longitude", raw$Longitude, as.character(round(d$lon, 6)), "coordinate repaired/imputed")

# ---- 4. flags (records are kept; flags are for review) ---------------------
flags <- list()
flag <- function(ids, f, detail) flags[[length(flags) + 1]] <<- tibble(isolate_id = ids, flag = f, detail = detail)

flag(d$isolate_id[malformed | bad_pair], "coord_malformed",
     paste0("original: ", raw$Latitude, " / ", raw$Longitude)[malformed | bad_pair])

# reported point farther than 25 km outside the stated country
pts <- d %>% filter(coord_precision == "reported") %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
wsel <- world %>% filter(iso3 %in% unique(pts$iso3))
dist_km <- map_dbl(seq_len(nrow(pts)), function(i) NA_real_)
for (iso in unique(pts$iso3)) {
  idx <- which(pts$iso3 == iso); poly <- wsel %>% filter(iso3 == iso)
  if (nrow(poly) == 0) next
  inside <- lengths(st_intersects(pts[idx, ], poly)) > 0
  dist_km[idx] <- 0
  if (any(!inside)) {
    dist_km[idx[!inside]] <- as.numeric(st_distance(
      st_transform(pts[idx[!inside], ], 3857), st_transform(st_union(poly), 3857))) / 1000
  }
}
far <- which(dist_km > 25)
if (length(far)) flag(pts$isolate_id[far], "coord_outside_country",
                      sprintf("%.0f km outside %s", dist_km[far], pts$country[far]))

key_cols <- setdiff(names(raw), c("ID"))
dup_all <- d %>% group_by(across(all_of(key_cols))) %>% filter(n() > 1) %>% ungroup()
flag(dup_all$isolate_id, "identical_record", "all fields identical to another row of the same article (except row ID)")

dup_strain <- d %>% filter(!is.na(Strain.code)) %>% group_by(article_id, Strain.code) %>%
  filter(n() > 1) %>% ungroup() %>% filter(!isolate_id %in% dup_all$isolate_id)
flag(dup_strain$isolate_id, "duplicate_strain_code", paste("strain", dup_strain$Strain.code, "repeated in the same article"))

rerep <- d %>% filter(!is.na(Strain.code), str_detect(Strain.code, "[A-Za-z]")) %>%
  group_by(Strain.code, species, country) %>% filter(n_distinct(reference) > 1) %>%
  mutate(detail = paste("same strain/species/country in:", paste(sort(unique(reference)), collapse = "; "))) %>% ungroup()
flag(rerep$isolate_id, "possible_rereport", rerep$detail)

flag(d$isolate_id[d$tri_genotype == "N-15-ADON"], "check_label", "TRI genotype label 'N-15-ADON': confirm meaning in source")
flag(d$isolate_id[raw$TRI_genotype %in% "s3-ADON"], "check_label", "'s3-ADON' recoded to 3-ADON: confirm in source")
flag(d$isolate_id[d$species == "Undetermined"], "species_missing", "species not reported")
flags <- bind_rows(flags)

# ---- 5. final tables -------------------------------------------------------
isolates <- d %>% transmute(
  isolate_id, article_id, row_in_article, reference, reference2 = Pub2, reference3 = Pub3,
  strain_code = Strain.code, collection_code = Collection.code,
  species, species_id_method = Species.ID.method,
  tri_genotype, tri_group, genotyping_method = Genotyping.method,
  chemotype, chemotyping_method = Chemotyping.method, chemotyping_substrate = chemotyping.substrate,
  host, host_group, organ, cultivar, location, country, iso3, region, year,
  latitude = round(lat, 5), longitude = round(lon, 5), coord_precision,
  genbank, comments = Comments)

articles <- isolates %>% group_by(article_id, reference) %>% summarise(
  n_isolates = n(), n_species = n_distinct(species[species != "Undetermined"]),
  countries = paste(sort(unique(country)), collapse = "; "),
  year_min = suppressWarnings(min(year, na.rm = TRUE)), year_max = suppressWarnings(max(year, na.rm = TRUE)),
  .groups = "drop") %>%
  mutate(across(c(year_min, year_max), ~ if_else(is.finite(.x), .x, NA_integer_)),
         pub_year = as.integer(str_extract(reference, "\\d{4}")),
         doi = NA_character_, data_level = "isolate", notes = NA_character_) %>%
  arrange(reference)

counts_template <- tibble(
  count_id = character(), article_id = integer(), reference = character(), doi = character(),
  geo_level = character(),        # country | admin1 | admin2 | locality
  country = character(), admin1 = character(), locality = character(),
  latitude = double(), longitude = double(),
  year_start = integer(), year_end = integer(), host = character(), organ = character(),
  species = character(),          # species name, or 'not crossed' when only genotype counts are given
  tri_genotype = character(),     # genotype, or 'not crossed'
  n_isolates = integer(), source_in_article = character(), notes = character())

changelog <- bind_rows(changelog) %>% arrange(isolate_id, field)

# ---- 6. write --------------------------------------------------------------
write_csv(isolates, "data/fgsc_isolates.csv", na = "")
write_csv(articles, "data/fgsc_articles.csv", na = "")
write_csv(counts_template, "data/fgsc_counts.csv", na = "")
write_csv(changelog, "data/changelog.csv", na = "")
write_csv(flags, "data/flags.csv", na = "")

vocab_all <- bind_rows(
  vocab("species.csv") %>% transmute(field = "species", original, standard = species),
  vocab("tri_genotype.csv") %>% transmute(field = "tri_genotype", original, standard = tri_genotype, group = tri_group, note),
  vocab("host.csv") %>% transmute(field = "host", original, standard = host, group = host_group),
  vocab("organ.csv") %>% transmute(field = "organ", original, standard = organ),
  vocab("country.csv") %>% transmute(field = "country", original, standard = country, group = region, note = iso3),
  vocab("chemotype.csv") %>% transmute(field = "chemotype", original, standard = chemotype))

readme <- tibble(sheet = c("isolates", "articles", "counts", "vocab", "changelog", "flags"),
  description = c(
    "One row per isolate (clean). coord_precision tells how the coordinate was obtained.",
    "One row per article/dataset. Fill DOI. data_level: isolate | aggregate | both.",
    "Aggregated counts from articles without isolate-level data (one row per reported combination). Do not enter here articles already in 'isolates'.",
    "Controlled vocabularies: original spelling -> standard value (+ group).",
    "Every value changed by the cleaning script (isolate_id, field, old, new, rule).",
    "Records to review: malformed coordinates, duplicates, possible re-reports across articles, labels to confirm."))

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1")
for (s in list(list("README", readme), list("isolates", isolates), list("articles", articles),
               list("counts", counts_template), list("vocab", vocab_all),
               list("changelog", changelog), list("flags", flags))) {
  addWorksheet(wb, s[[1]]); writeData(wb, s[[1]], s[[2]], headerStyle = hs)
  freezePane(wb, s[[1]], firstRow = TRUE); setColWidths(wb, s[[1]], 1:ncol(s[[2]]), "auto")
}
saveWorkbook(wb, "FGSCdb_clean.xlsx", overwrite = TRUE)

cat("isolates:", nrow(isolates), "| articles:", nrow(articles),
    "| changes:", nrow(changelog), "| flags:", nrow(flags), "\n")
print(count(isolates, coord_precision))
print(count(flags, flag))
print(count(changelog, field, rule))
