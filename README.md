# FGSCdb

Georeferenced database of *Fusarium graminearum* species complex (FGSC) isolates identified to species and
trichothecene genotype, compiled from the literature. The website is static (no server, no database) and is
served by Netlify from the `docs/` folder at https://fgsc.netlify.app.

> Del Ponte et al. 2022. Phytopathology 112:741–751. https://doi.org/10.1094/PHYTO-06-21-0277-RVW

## Repository layout

| Path | What it holds |
|---|---|
| `docs/` | The published site: `index.html` and `data/` (built files, do not edit by hand) |
| `site-src/page.html` | Source of the web page (map, charts, table, sources) |
| `data/` | Curated tables exported from the master Google Sheet: isolates, articles, counts, changelog, flags |
| `vocab/` | Controlled vocabularies and the gazetteer used to geocode localities |
| `scripts/` | R pipeline (see below) |
| `raw/` | The 2021 Google Sheet used by the old Shiny app, kept for provenance |
| `legacy-distill/` | Sources of the previous distill site |

## Data flow

```
Master Google Sheet (FGSCdb_clean)          scripts/00_fetch_sheet.R  -> data/*.csv, vocab/*.csv
  tabs: isolates, articles, counts, vocab   scripts/03_validate.R     -> stops on invalid values
                                            scripts/02_site_data.R    -> docs/index.html, docs/data/
```

* **isolates**: one row per isolate, in standard vocabulary. `coord_precision` records how the coordinate was
  obtained: `reported`, `locality (from same-location records)`, `municipality (geocoded)`, `county centroid`,
  `country centroid`.
* **articles**: one row per source (`article_id`, `reference`, `doi`, `data_level` = isolate | aggregate | both).
* **counts**: aggregated data from sources without isolate-level tables (geo unit × period × host × species ×
  TRI genotype → `n_isolates`); use `not crossed` when a source gives species and genotype counts separately.
  A source must not appear in both `isolates` and `counts`.
* **vocab**: controlled vocabularies; the validator rejects values that are not listed.

`scripts/01_clean.R` is the one-time 2026 cleanup of the 2021 sheet (`raw/`). It needs Natural Earth country
boundaries and US county shapes in `geo/` (not versioned). Every change it made is in `data/changelog.csv`, and
records to review are in `data/flags.csv`.

## Rebuild the site

Locally, after the data CSVs are up to date:

```r
source("scripts/03_validate.R"); source("scripts/02_site_data.R")
```

Open `docs/index.html` directly or serve `docs/` with any static server.

On GitHub: Actions > "Build FGSCdb" > Run workflow. It downloads the master sheet, validates, rebuilds `docs/`
and commits; Netlify then redeploys. The sheet must be shared as "Anyone with the link can view".

## Netlify

Publish directory: `docs`. No build command.
