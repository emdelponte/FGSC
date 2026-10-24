# Build the compact JSON the website reads (docs/data/fgsc.json)
# Columns are dictionary-encoded (integer codes + lookup arrays) to keep the file small.
suppressPackageStartupMessages({ library(readr); library(dplyr); library(jsonlite) })

iso <- read_csv("data/fgsc_isolates.csv", show_col_types = FALSE, guess_max = 1e5)
art <- read_csv("data/fgsc_articles.csv", show_col_types = FALSE)
cnt <- read_csv("data/fgsc_counts.csv", show_col_types = FALSE)

enc <- function(x, levels = NULL) {
  x <- ifelse(is.na(x), "—", as.character(x))
  lv <- if (is.null(levels)) names(sort(table(x), decreasing = TRUE)) else levels
  list(levels = lv, codes = match(x, lv) - 1L)
}
fields <- list(sp = "species", tg = "tri_group", tri = "tri_genotype", h = "host", hg = "host_group",
               org = "organ", c = "country", rg = "region", ref = "reference", pr = "coord_precision")
dict <- list(); cols <- list()
for (k in names(fields)) { e <- enc(iso[[fields[[k]]]]); dict[[k]] <- e$levels; cols[[k]] <- e$codes }
cols$y   <- iso$year
cols$lat <- iso$latitude
cols$lon <- iso$longitude
cols$id  <- iso$isolate_id
cols$st  <- iso$strain_code
cols$loc <- iso$location

out <- list(
  meta = list(generated = format(Sys.time(), "%Y-%m-%d"), n = nrow(iso)),
  dict = dict, cols = cols,
  articles = art %>% select(article_id, reference, n_isolates, countries, year_min, year_max, doi),
  counts = cnt
)
dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)
write_json(out, "docs/data/fgsc.json", auto_unbox = TRUE, na = "null", digits = 5)
write_csv(iso, "docs/data/fgsc_isolates.csv", na = "")
# Script copies of the JSON so the page also works when opened from disk (file://), where fetch() is blocked
js <- function(json, var, out) writeLines(paste0("window.", var, "=", paste(readLines(json, warn = FALSE), collapse = ""), ";"), out)
js("docs/data/fgsc.json", "FGSC_DATA", "docs/data/fgsc.js")
if (file.exists("docs/data/world.json")) js("docs/data/world.json", "FGSC_WORLD", "docs/data/world.js")
cat("docs/data/fgsc.json:", round(file.size("docs/data/fgsc.json") / 1e6, 2), "MB\n")

# Standalone index.html (full HTML document) built from the page fragment site-src/page.html
page <- readLines("site-src/page.html", warn = FALSE, encoding = "UTF-8")
cut <- which(grepl('<div class="wrap">', page, fixed = TRUE))[1]   # title, meta, links and styles go in <head>
head_part <- page[seq_len(cut - 1)]; body_part <- page[cut:length(page)]
writeLines(c('<!doctype html>', '<html lang="en">', '<head>', '<meta charset="utf-8">',
             '<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">',
             '<style>html{color-scheme:light}:root{padding-top:env(safe-area-inset-top,0px);padding-bottom:env(safe-area-inset-bottom,0px)}body{margin:0}img{max-width:100%}[hidden]{display:none!important}</style>',
             head_part, '</head>', '<body>', body_part, '</body>', '</html>'), "docs/index.html", useBytes = TRUE)
cat("docs/index.html written\n")
